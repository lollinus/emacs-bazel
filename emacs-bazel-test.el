;;; emacs-bazel-test.el --- Tests for emacs-bazel -*- lexical-binding: t; -*-

;; Copyright 2026 lollinus
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; ERT tests for emacs-bazel.el and emacs-bazel-debug.el.
;; Run with: emacs --batch -L ~/projects/bazel.el -L . -l ert -l emacs-bazel-test -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'emacs-bazel)
(require 'emacs-bazel-debug)

;;;; Test helpers

(defmacro emacs-bazel-test--with-temp-workspace (&rest body)
  "Create a temporary Bazel workspace directory and execute BODY.
Binds `root' to the workspace root path.  Creates WORKSPACE and
BUILD files so bazel.el recognises it as a repository."
  (declare (indent 0) (debug t))
  `(let ((root (file-name-as-directory (make-temp-file "emacs-bazel-test-" t))))
     (unwind-protect
         (progn
           ;; Minimal Bazel workspace markers
           (with-temp-file (expand-file-name "WORKSPACE" root)
             (insert "workspace(name = \"test_workspace\")\n"))
           (with-temp-file (expand-file-name "BUILD" root))
           ,@body)
       (delete-directory root t))))

(defun emacs-bazel-test--write-cpp-info (root relative-path info-alist)
  "Write INFO-ALIST as a .cpp_info.json file at RELATIVE-PATH under ROOT.
Creates parent directories as needed."
  (let ((abs-path (expand-file-name relative-path root)))
    (make-directory (file-name-directory abs-path) t)
    (with-temp-file abs-path
      (insert (json-serialize info-alist :false-object :json-false
                              :null-object :json-null)))))

;;;; Config tests

(ert-deftest emacs-bazel-test-default-config ()
  "Default config has expected structure."
  (let ((config (emacs-bazel--default-config)))
    (should (assq 'packages config))
    (should (assq 'configurations config))
    (should (assq 'activeConfiguration config))
    (should (assq 'mode config))
    (should (equal (alist-get 'activeConfiguration config) "default"))
    (should (equal (alist-get 'mode config) "per-directory"))))

(ert-deftest emacs-bazel-test-config-roundtrip ()
  "Config can be written and read back without data loss."
  (emacs-bazel-test--with-temp-workspace
    (let* ((config (emacs-bazel--default-config))
           (_ (emacs-bazel--write-config root config))
           (read-back (emacs-bazel--read-config root)))
      (should read-back)
      (should (equal (alist-get 'activeConfiguration read-back) "default"))
      (should (equal (alist-get 'mode read-back) "per-directory"))
      ;; Packages should roundtrip as empty
      (should (equal (emacs-bazel--packages read-back) nil)))))

(ert-deftest emacs-bazel-test-config-with-packages ()
  "Config preserves package list through write/read."
  (emacs-bazel-test--with-temp-workspace
    (let ((config (emacs-bazel--default-config)))
      (setf (alist-get 'packages config)
            (vconcat '("//foo/..." "//bar/...")))
      (emacs-bazel--write-config root config)
      (let ((read-back (emacs-bazel--read-config root)))
        (should (equal (emacs-bazel--packages read-back)
                       '("//foo/..." "//bar/...")))))))

(ert-deftest emacs-bazel-test-config-with-configurations ()
  "Config preserves multiple configurations."
  (emacs-bazel-test--with-temp-workspace
    (let ((config `((packages . [])
                    (configurations . ((debug . ["--config=clang" "--cxxopt=-g"])
                                       (release . ["--config=clang" "-c" "opt"])))
                    (activeConfiguration . "debug")
                    (mode . "per-directory")
                    (compileCommandsDir . :json-null))))
      (emacs-bazel--write-config root config)
      (let ((read-back (emacs-bazel--read-config root)))
        (should (equal (emacs-bazel--active-args read-back)
                       '("--config=clang" "--cxxopt=-g")))
        ;; Switch to release
        (setf (alist-get 'activeConfiguration read-back) "release")
        (should (equal (emacs-bazel--active-args read-back)
                       '("--config=clang" "-c" "opt")))))))

(ert-deftest emacs-bazel-test-active-args-missing-config ()
  "Active args returns nil for a missing configuration name."
  (let ((config `((configurations . ((debug . ["--cxxopt=-g"])))
                  (activeConfiguration . "nonexistent"))))
    (should (equal (emacs-bazel--active-args config) nil))))

(ert-deftest emacs-bazel-test-get-config-creates-default ()
  "get-config creates a default config.json if none exists."
  (emacs-bazel-test--with-temp-workspace
    (should-not (file-exists-p (emacs-bazel--config-file root)))
    (let ((config (emacs-bazel--get-config root)))
      (should config)
      (should (file-exists-p (emacs-bazel--config-file root)))
      (should (equal (alist-get 'activeConfiguration config) "default")))))

;;;; Config directory setup tests

(ert-deftest emacs-bazel-test-ensure-config-dir-creates-build ()
  "ensure-config-dir creates BUILD file for Bazel package."
  (emacs-bazel-test--with-temp-workspace
    (emacs-bazel--ensure-config-dir root)
    (let ((config-dir (emacs-bazel--config-dir root)))
      (should (file-directory-p config-dir))
      (should (file-exists-p (expand-file-name "BUILD" config-dir)))
      (should (file-exists-p (expand-file-name "cpp-info-aspect.bzl" config-dir))))))

(ert-deftest emacs-bazel-test-ensure-config-dir-deploys-aspect ()
  "ensure-config-dir copies the bundled aspect file."
  (emacs-bazel-test--with-temp-workspace
    (emacs-bazel--ensure-config-dir root)
    (let* ((deployed (expand-file-name "cpp-info-aspect.bzl"
                                       (emacs-bazel--config-dir root)))
           (bundled (emacs-bazel--aspect-source)))
      (should (file-exists-p deployed))
      ;; Content should match
      (should (equal (with-temp-buffer
                       (insert-file-contents deployed)
                       (buffer-string))
                     (with-temp-buffer
                       (insert-file-contents bundled)
                       (buffer-string)))))))

(ert-deftest emacs-bazel-test-ensure-config-dir-idempotent ()
  "Calling ensure-config-dir twice doesn't error."
  (emacs-bazel-test--with-temp-workspace
    (emacs-bazel--ensure-config-dir root)
    (emacs-bazel--ensure-config-dir root)
    (should (file-exists-p (expand-file-name "BUILD"
                                             (emacs-bazel--config-dir root))))))

;;;; Aspect path tests

(ert-deftest emacs-bazel-test-aspect-path ()
  "Aspect path is workspace-relative."
  (let ((path (emacs-bazel--aspect-path "/some/root/")))
    (should (equal path ".bazel-compdb/cpp-info-aspect.bzl"))
    (should-not (file-name-absolute-p path))))

;;;; compile_commands assembly tests

(ert-deftest emacs-bazel-test-parse-cpp-info ()
  "parse-cpp-info reads a .cpp_info.json correctly."
  (emacs-bazel-test--with-temp-workspace
    (let ((info `((label . "//pkg:target")
                  (compiler . "/usr/bin/gcc")
                  (args . ["-I/inc" "-DFOO=1"])
                  (files . ["pkg/main.cc" "pkg/util.h"]))))
      (emacs-bazel-test--write-cpp-info root "test.cpp_info.json" info)
      (let ((parsed (emacs-bazel--parse-cpp-info
                     (expand-file-name "test.cpp_info.json" root))))
        (should (equal (alist-get 'label parsed) "//pkg:target"))
        (should (equal (alist-get 'compiler parsed) "/usr/bin/gcc"))
        (should (equal (alist-get 'files parsed) '("pkg/main.cc" "pkg/util.h")))))))

(ert-deftest emacs-bazel-test-make-compile-entry ()
  "make-compile-entry assembles compiler and args into a command string."
  (let ((info `((compiler . "/usr/bin/clang++")
                (args . ("-std=c++17" "-I/inc" "-Wall")))))
    (should (equal (emacs-bazel--make-compile-entry "/root/" info)
                   "/usr/bin/clang++ -std=c++17 -I/inc -Wall"))))

(ert-deftest emacs-bazel-test-make-compile-entry-nil-compiler ()
  "make-compile-entry returns nil when compiler is missing."
  (let ((info `((compiler . nil) (args . ("-Wall")))))
    (should-not (emacs-bazel--make-compile-entry "/root/" info))))

(ert-deftest emacs-bazel-test-make-compile-entry-nil-args ()
  "make-compile-entry returns nil when args is missing."
  (let ((info `((compiler . "/usr/bin/gcc") (args . nil))))
    (should-not (emacs-bazel--make-compile-entry "/root/" info))))

(ert-deftest emacs-bazel-test-find-cpp-info-files ()
  "find-cpp-info-files discovers .cpp_info.json in bazel-bin/."
  (emacs-bazel-test--with-temp-workspace
    ;; Create fake bazel-bin structure
    (let ((info `((label . "//a:t") (compiler . "gcc") (args . ["-O2"]) (files . ["a.cc"]))))
      (emacs-bazel-test--write-cpp-info root "bazel-bin/a/t.cpp_info.json" info)
      (emacs-bazel-test--write-cpp-info root "bazel-bin/b/u.cpp_info.json" info)
      ;; Should not pick up non-matching files
      (with-temp-file (expand-file-name "bazel-bin/other.json" root)
        (insert "{}"))
      (let ((files (emacs-bazel--find-cpp-info-files root)))
        (should (= (length files) 2))
        (should (cl-every (lambda (f) (string-match-p "\\.cpp_info\\.json$" f))
                          files))))))

(ert-deftest emacs-bazel-test-find-cpp-info-files-no-bazel-bin ()
  "find-cpp-info-files returns nil when bazel-bin/ doesn't exist."
  (emacs-bazel-test--with-temp-workspace
    (should-not (emacs-bazel--find-cpp-info-files root))))

(ert-deftest emacs-bazel-test-write-compile-commands ()
  "write-compile-commands produces valid JSON array."
  (emacs-bazel-test--with-temp-workspace
    (let ((entries `(((directory . "/ws") (file . "/ws/a.cc") (command . "gcc -O2 a.cc"))
                     ((directory . "/ws") (file . "/ws/b.cc") (command . "gcc -O2 b.cc")))))
      (emacs-bazel--write-compile-commands root entries)
      (let* ((file (expand-file-name "compile_commands.json" root))
             (parsed (with-temp-buffer
                       (insert-file-contents file)
                       (json-parse-buffer :object-type 'alist :array-type 'list))))
        (should (= (length parsed) 2))
        (should (equal (alist-get 'file (car parsed)) "/ws/a.cc"))
        (should (equal (alist-get 'command (cadr parsed)) "gcc -O2 b.cc"))))))

(ert-deftest emacs-bazel-test-write-compile-commands-creates-dir ()
  "write-compile-commands creates the target directory if needed."
  (emacs-bazel-test--with-temp-workspace
    (let ((subdir (expand-file-name "deep/nested/dir" root))
          (entries `(((directory . "/ws") (file . "a.cc") (command . "gcc a.cc")))))
      (should-not (file-directory-p subdir))
      (emacs-bazel--write-compile-commands subdir entries)
      (should (file-exists-p (expand-file-name "compile_commands.json" subdir))))))

(ert-deftest emacs-bazel-test-write-compile-commands-nil-entries ()
  "write-compile-commands does nothing with nil entries."
  (emacs-bazel-test--with-temp-workspace
    (emacs-bazel--write-compile-commands root nil)
    (should-not (file-exists-p (expand-file-name "compile_commands.json" root)))))

(ert-deftest emacs-bazel-test-assemble-per-directory ()
  "assemble-compile-commands in per-directory mode writes to source dirs."
  (emacs-bazel-test--with-temp-workspace
    ;; Create config
    (let ((config `((packages . ["//pkg/..."])
                    (configurations . ((default . ["--keep_going"])))
                    (activeConfiguration . "default")
                    (mode . "per-directory")
                    (compileCommandsDir . :json-null))))
      (emacs-bazel--write-config root config))
    ;; Create source directories
    (make-directory (expand-file-name "pkg/sub" root) t)
    (with-temp-file (expand-file-name "pkg/sub/foo.cc" root)
      (insert "int main() {}"))
    ;; Create cpp_info.json in bazel-bin
    (emacs-bazel-test--write-cpp-info
     root "bazel-bin/pkg/sub/target.cpp_info.json"
     `((label . "//pkg/sub:target")
       (compiler . "/usr/bin/gcc")
       (args . ["-std=c++17" "-I/inc"])
       (files . ["pkg/sub/foo.cc"])))
    ;; Assemble
    (emacs-bazel--assemble-compile-commands root)
    ;; Check: compile_commands.json in pkg/sub/ (where foo.cc lives)
    (let ((compdb (expand-file-name "pkg/sub/compile_commands.json" root)))
      (should (file-exists-p compdb))
      (let ((parsed (with-temp-buffer
                      (insert-file-contents compdb)
                      (json-parse-buffer :object-type 'alist :array-type 'list))))
        (should (= (length parsed) 1))
        (should (string-match-p "gcc" (alist-get 'command (car parsed))))
        (should (string-match-p "foo\\.cc" (alist-get 'file (car parsed))))))))

(ert-deftest emacs-bazel-test-assemble-single-mode ()
  "assemble-compile-commands in single mode writes to configured dir."
  (emacs-bazel-test--with-temp-workspace
    (let ((out-dir (expand-file-name "output" root)))
      (make-directory out-dir t)
      (let ((config `((packages . ["//pkg/..."])
                      (configurations . ((default . ["--keep_going"])))
                      (activeConfiguration . "default")
                      (mode . "single")
                      (compileCommandsDir . "output"))))
        (emacs-bazel--write-config root config))
      ;; Create cpp_info.json files in different packages
      (emacs-bazel-test--write-cpp-info
       root "bazel-bin/a/t1.cpp_info.json"
       `((label . "//a:t1") (compiler . "gcc") (args . ["-O2"]) (files . ["a/x.cc"])))
      (emacs-bazel-test--write-cpp-info
       root "bazel-bin/b/t2.cpp_info.json"
       `((label . "//b:t2") (compiler . "gcc") (args . ["-O2"]) (files . ["b/y.cc"])))
      ;; Assemble
      (emacs-bazel--assemble-compile-commands root)
      ;; Single file in output/
      (let* ((compdb (expand-file-name "compile_commands.json" out-dir))
             (parsed (with-temp-buffer
                       (insert-file-contents compdb)
                       (json-parse-buffer :object-type 'alist :array-type 'list))))
        (should (= (length parsed) 2))))))

;;;; Workspace detection tests

(ert-deftest emacs-bazel-test-workspace-root-detection ()
  "emacs-bazel--workspace-root finds WORKSPACE file."
  (emacs-bazel-test--with-temp-workspace
    (make-directory (expand-file-name "pkg/sub" root) t)
    (with-temp-file (expand-file-name "pkg/sub/test.cc" root))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "pkg/sub/test.cc" root))
      (setq default-directory (expand-file-name "pkg/sub/" root))
      (let ((found (emacs-bazel--workspace-root)))
        (should (file-equal-p found root))))))

(ert-deftest emacs-bazel-test-workspace-root-no-workspace ()
  "emacs-bazel--workspace-root errors when not in a workspace."
  (let ((tmpdir (make-temp-file "no-workspace-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory tmpdir)
          (setq buffer-file-name nil)
          (should-error (emacs-bazel--workspace-root) :type 'user-error))
      (delete-directory tmpdir t))))

;;;; Debug helper tests

(ert-deftest emacs-bazel-test-gdb-command-simple ()
  "GDB command for a simple executable."
  (let ((emacs-bazel-gdb-path "gdb"))
    (should (equal (emacs-bazel-debug--make-gdb-command "/path/to/binary")
                   "gdb /path/to/binary"))))

(ert-deftest emacs-bazel-test-gdb-command-with-filter ()
  "GDB command with --gtest_filter."
  (let ((emacs-bazel-gdb-path "/usr/bin/gdb"))
    (should (string-match-p
             "--gtest_filter=.*MySuite\\.MyTest"
             (emacs-bazel-debug--make-gdb-command
              "/path/to/test_binary" "MySuite.MyTest")))))

(ert-deftest emacs-bazel-test-gdb-command-with-args ()
  "GDB command with gtest_filter uses --args."
  (let ((emacs-bazel-gdb-path "gdb"))
    (should (string-match-p
             "--args"
             (emacs-bazel-debug--make-gdb-command "/bin/test" "Foo.Bar")))))

(ert-deftest emacs-bazel-test-setup-environment-cc-test ()
  "setup-environment sets TEST_SRCDIR etc for cc_test."
  (emacs-bazel-test--with-temp-workspace
    (emacs-bazel-debug--setup-environment root "cc_test")
    (should (equal (getenv "TEST_SRCDIR") root))
    (should (equal (getenv "TEST_TMPDIR")
                   (expand-file-name "tmp" root)))
    (should (equal (getenv "RUNFILES_DIR") root))
    (should (file-directory-p (expand-file-name "tmp" root)))
    ;; Clean up env
    (setenv "TEST_SRCDIR" nil)
    (setenv "TEST_TMPDIR" nil)
    (setenv "RUNFILES_DIR" nil)))

(ert-deftest emacs-bazel-test-setup-environment-cc-binary ()
  "setup-environment does nothing for cc_binary."
  (emacs-bazel-test--with-temp-workspace
    (let ((orig-srcdir (getenv "TEST_SRCDIR")))
      (setenv "TEST_SRCDIR" nil)
      (emacs-bazel-debug--setup-environment root "cc_binary")
      (should-not (getenv "TEST_SRCDIR"))
      (setenv "TEST_SRCDIR" orig-srcdir))))

;;;; Packages helper tests

(ert-deftest emacs-bazel-test-packages-from-vector ()
  "packages extracts list from vector."
  (let ((config `((packages . ["//a/..." "//b/..."]))))
    (should (equal (emacs-bazel--packages config) '("//a/..." "//b/...")))))

(ert-deftest emacs-bazel-test-packages-from-list ()
  "packages works with list too (from json-parse with :array-type list)."
  (let ((config `((packages . ("//a/..." "//b/...")))))
    (should (equal (emacs-bazel--packages config) '("//a/..." "//b/...")))))

(ert-deftest emacs-bazel-test-packages-empty ()
  "packages returns nil for empty vector."
  (let ((config `((packages . []))))
    (should-not (emacs-bazel--packages config))))

(provide 'emacs-bazel-test)
;;; emacs-bazel-test.el ends here
