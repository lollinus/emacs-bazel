;;; emacs-bazel-debug.el --- GDB debug support for Bazel C++ targets -*- lexical-binding: t; -*-

;; Copyright 2026 lollinus
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Provides commands to build Bazel cc_test/cc_binary targets with debug
;; flags and launch GDB (via gud-gdb) with proper environment setup.
;;
;; Features:
;; - Builds with --compilation_mode=dbg (or custom debug config)
;; - Locates the built binary via `bazel cquery --output=files'
;; - Sets RUNFILES_DIR, TEST_SRCDIR, TEST_TMPDIR for cc_test targets
;; - Supports --gtest_filter via test-at-point detection
;; - Integrates with bazel.el's target completion

;;; Code:

(require 'emacs-bazel)
(require 'gud)

;;;; Customization

(defcustom emacs-bazel-gdb-path "gdb"
  "Path to the GDB executable."
  :type 'string
  :group 'emacs-bazel)

(defcustom emacs-bazel-debug-args '("--compilation_mode=dbg")
  "Additional Bazel args for debug builds.
These are appended to the active configuration args."
  :type '(repeat string)
  :group 'emacs-bazel)

;;;; Internal

(defun emacs-bazel-debug--query-executable (root target args)
  "Query the output file path for TARGET in ROOT with ARGS.
Returns the absolute path to the built binary."
  (let* ((default-directory root)
         (cmd (append bazel-command
                      (list "cquery" target "--output=files")
                      args))
         (output (with-temp-buffer
                   (let ((exit-code
                          (apply #'call-process (car cmd) nil t nil (cdr cmd))))
                     (unless (zerop exit-code)
                       (user-error "bazel cquery failed (exit %d): %s"
                                   exit-code (buffer-string)))
                     (string-trim (buffer-string))))))
    ;; cquery may return multiple files; take the first executable
    (let ((files (split-string output "\n" t)))
      (or (cl-find-if (lambda (f)
                        (let ((abs (expand-file-name f root)))
                          (and (file-exists-p abs)
                               (file-executable-p abs))))
                      files)
          (car files)))))

(defun emacs-bazel-debug--target-kind (root target args)
  "Query the kind (cc_test, cc_binary, etc.) of TARGET in ROOT with ARGS."
  (let* ((default-directory root)
         (cmd (append bazel-command
                      (list "query" target "--output=label_kind")
                      args))
         (output (with-temp-buffer
                   (apply #'call-process (car cmd) nil t nil (cdr cmd))
                   (string-trim (buffer-string)))))
    ;; Output looks like: "cc_test rule //pkg:target"
    (car (split-string output " "))))

(defun emacs-bazel-debug--build-target (root target args)
  "Build TARGET in ROOT with debug ARGS synchronously.
Returns non-nil on success."
  (let* ((default-directory root)
         (cmd (append bazel-command
                      (list "build" target)
                      args
                      emacs-bazel-debug-args))
         (buf (compile (mapconcat #'shell-quote-argument cmd " "))))
    buf))

(defun emacs-bazel-debug--make-gdb-command (executable &optional gtest-filter)
  "Construct a GDB command string for EXECUTABLE.
If GTEST-FILTER is non-nil, pass it as an arg to the program."
  (let ((args (if gtest-filter
                  (format " --args %s --gtest_filter=%s"
                          (shell-quote-argument executable)
                          (shell-quote-argument gtest-filter))
                (format " %s" (shell-quote-argument executable)))))
    (concat emacs-bazel-gdb-path args)))

(defun emacs-bazel-debug--setup-environment (root target-kind)
  "Set up environment variables for debugging.
ROOT is the workspace root.  TARGET-KIND determines which vars to set."
  (when (string= target-kind "cc_test")
    (let ((tmp-dir (expand-file-name "tmp" root)))
      (setenv "TEST_SRCDIR" root)
      (setenv "TEST_TMPDIR" tmp-dir)
      (setenv "RUNFILES_DIR" root)
      (unless (file-directory-p tmp-dir)
        (make-directory tmp-dir t)))))

;;;; Interactive commands

;;;###autoload
(defun emacs-bazel-debug-target (target)
  "Build and debug a Bazel TARGET with GDB.
Builds with debug flags, locates the binary, and launches GDB."
  (interactive
   (list (let ((default-directory (emacs-bazel--workspace-root)))
           (bazel--read-target-pattern "debug" nil))))
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root))
         (args (emacs-bazel--active-args config))
         (all-args (append args emacs-bazel-debug-args)))
    ;; Build first
    (message "Building %s with debug flags..." target)
    (let* ((default-directory root)
           (cmd (append bazel-command
                        (list "build" target)
                        all-args))
           (exit-code (apply #'call-process
                             (car cmd) nil "*emacs-bazel-build*" nil
                             (cdr cmd))))
      (unless (zerop exit-code)
        (pop-to-buffer "*emacs-bazel-build*")
        (user-error "Build failed (exit %d)" exit-code)))
    ;; Find the binary
    (let* ((executable-rel (emacs-bazel-debug--query-executable
                            root target all-args))
           (executable (expand-file-name executable-rel root))
           (target-kind (emacs-bazel-debug--target-kind root target args)))
      (unless (file-exists-p executable)
        (user-error "Binary not found: %s" executable))
      ;; Setup environment
      (emacs-bazel-debug--setup-environment root target-kind)
      ;; Launch GDB
      (let ((default-directory root)
            (gdb-cmd (emacs-bazel-debug--make-gdb-command executable)))
        (gud-gdb gdb-cmd)))))

;;;###autoload
(defun emacs-bazel-test-at-point-debug ()
  "Debug the GTest at point with GDB.
Detects TEST(Suite, Name) and passes --gtest_filter to GDB."
  (interactive)
  (let* ((root (emacs-bazel--workspace-root))
         (source-file (or buffer-file-name
                          (user-error "Buffer doesn't visit a file")))
         (directory (or (bazel--package-directory source-file root)
                        (user-error "Not in a Bazel package")))
         (package (or (bazel--package-name directory root)
                      (user-error "Not in a Bazel package")))
         (build-file (or (bazel--locate-build-file directory)
                         (user-error "No BUILD file found")))
         (relative-file (file-relative-name source-file directory))
         (case-fold (file-name-case-insensitive-p source-file))
         (target-name (or (bazel--consuming-target
                           build-file relative-file case-fold :only-tests)
                          (user-error "No test target for this file")))
         (target (bazel--canonical nil package target-name))
         (filter (or (bazel-c++-test-at-point)
                     (user-error "Point is not on a test case")))
         (config (emacs-bazel--get-config root))
         (args (append (emacs-bazel--active-args config)
                       emacs-bazel-debug-args)))
    ;; Build
    (message "Building %s for debugging..." target)
    (let* ((default-directory root)
           (cmd (append bazel-command (list "build" target) args))
           (exit-code (apply #'call-process
                             (car cmd) nil "*emacs-bazel-build*" nil
                             (cdr cmd))))
      (unless (zerop exit-code)
        (pop-to-buffer "*emacs-bazel-build*")
        (user-error "Build failed (exit %d)" exit-code)))
    ;; Find binary and launch GDB with filter
    (let* ((executable-rel (emacs-bazel-debug--query-executable
                            root target args))
           (executable (expand-file-name executable-rel root)))
      (emacs-bazel-debug--setup-environment root "cc_test")
      (let ((default-directory root)
            (gdb-cmd (emacs-bazel-debug--make-gdb-command executable filter)))
        (gud-gdb gdb-cmd)))))

(provide 'emacs-bazel-debug)
;;; emacs-bazel-debug.el ends here
