;;; emacs-bazel.el --- Bazel IDE features: compile_commands & debug -*- lexical-binding: t; -*-

;; Author: lollinus
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (bazel "0"))
;; Keywords: tools, languages, c
;; URL: https://github.com/lollinus/emacs-bazel

;; Copyright 2026 lollinus
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Extends bazel.el with IDE features inspired by vscode-bazel:
;;
;; - `emacs-bazel-refresh' — generate compile_commands.json via Bazel aspects
;; - `emacs-bazel-add-package' — add a package to the project config
;; - `emacs-bazel-select-configuration' — switch Bazel config (debug/release)
;; - `emacs-bazel-debug-target' — build and debug a cc_test/cc_binary with GDB
;; - `emacs-bazel-test-at-point' — run/debug the GTest at point
;;
;; Configuration is stored per-project in .bazel-compdb/config.json.
;; The Bazel aspect is deployed to .bazel-compdb/cpp-info-aspect.bzl.

;;; Code:

(require 'bazel)
(require 'json)
(require 'project)
(require 'compile)

;;;; Customization

(defgroup emacs-bazel nil
  "Bazel IDE features for Emacs."
  :group 'bazel
  :prefix "emacs-bazel-")

(defcustom emacs-bazel-config-dir ".bazel-compdb"
  "Directory name (relative to workspace root) for emacs-bazel config and aspect."
  :type 'string
  :group 'emacs-bazel)

(defcustom emacs-bazel-default-mode 'per-directory
  "Default mode for compile_commands.json generation.
`per-directory' writes one file per source package directory.
`single' writes one file at a configured location."
  :type '(choice (const :tag "Per directory" per-directory)
                 (const :tag "Single file" single))
  :group 'emacs-bazel)

;;;; Internal: workspace and config

(defun emacs-bazel--workspace-root ()
  "Return the Bazel workspace root for the current buffer, or signal error."
  (or (and buffer-file-name
           (bazel--repository-root buffer-file-name))
      (and default-directory
           (bazel--repository-root default-directory))
      (user-error "Not in a Bazel workspace")))

(defun emacs-bazel--config-dir (root)
  "Return the absolute path to the config directory under ROOT."
  (expand-file-name emacs-bazel-config-dir root))

(defun emacs-bazel--config-file (root)
  "Return the absolute path to config.json under ROOT."
  (expand-file-name "config.json" (emacs-bazel--config-dir root)))

(defun emacs-bazel--aspect-source ()
  "Return the path to the bundled cpp-info-aspect.bzl."
  (expand-file-name "aspects/cpp-info-aspect.bzl"
                    (file-name-directory (or load-file-name
                                            (locate-library "emacs-bazel")
                                            buffer-file-name))))

(defun emacs-bazel--ensure-config-dir (root)
  "Ensure the config directory and aspect file exist under ROOT.
Returns the config directory path."
  (let ((dir (emacs-bazel--config-dir root)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Deploy aspect if missing or outdated
    (let ((dest (expand-file-name "cpp-info-aspect.bzl" dir))
          (src (emacs-bazel--aspect-source)))
      (when (or (not (file-exists-p dest))
                (time-less-p (file-attribute-modification-time
                              (file-attributes dest))
                             (file-attribute-modification-time
                              (file-attributes src))))
        (copy-file src dest t)))
    dir))

(defun emacs-bazel--read-config (root)
  "Read and return the config alist from ROOT's config.json.
Returns nil if no config file exists."
  (let ((file (emacs-bazel--config-file root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-buffer :object-type 'alist :array-type 'list)))))

(defun emacs-bazel--write-config (root config)
  "Write CONFIG alist to ROOT's config.json."
  (let ((file (emacs-bazel--config-file root)))
    (emacs-bazel--ensure-config-dir root)
    (with-temp-file file
      (insert (json-serialize config :false-object :json-false
                              :null-object nil)))))

(defun emacs-bazel--default-config ()
  "Return a default config alist."
  `((packages . [])
    (configurations . ((default . ["--keep_going"])))
    (activeConfiguration . "default")
    (mode . ,(symbol-name emacs-bazel-default-mode))
    (compileCommandsDir . nil)))

(defun emacs-bazel--get-config (root)
  "Get config for ROOT, creating default if needed."
  (or (emacs-bazel--read-config root)
      (let ((config (emacs-bazel--default-config)))
        (emacs-bazel--write-config root config)
        config)))

(defun emacs-bazel--active-args (config)
  "Return the active Bazel command args list from CONFIG."
  (let* ((active (alist-get 'activeConfiguration config))
         (configurations (alist-get 'configurations config))
         (args (alist-get (intern active) configurations)))
    (if (vectorp args) (append args nil) (or args nil))))

(defun emacs-bazel--packages (config)
  "Return the package list from CONFIG."
  (let ((pkgs (alist-get 'packages config)))
    (if (vectorp pkgs) (append pkgs nil) (or pkgs nil))))

;;;; Core: aspect build and compile_commands assembly

(defun emacs-bazel--aspect-path (_root)
  "Return the workspace-relative path to the aspect .bzl file.
_ROOT is the workspace root (unused but kept for future per-project overrides)."
  (concat emacs-bazel-config-dir "/cpp-info-aspect.bzl"))

(defun emacs-bazel--build-aspects (root packages args)
  "Run bazel build with the aspect for PACKAGES using ARGS in ROOT.
Returns the compilation buffer."
  (let* ((default-directory root)
         (aspect-rel (emacs-bazel--aspect-path root))
         (cmd (append bazel-command
                      (list "build")
                      packages
                      (list (format "--aspects=%s%%cpp_info_aspect" aspect-rel)
                            "--output_groups=cpp_info_files"
                            "--keep_going")
                      args)))
    (compile (mapconcat #'shell-quote-argument cmd " "))))

(defun emacs-bazel--find-cpp-info-files (root)
  "Find all .cpp_info.json files under bazel-bin/ in ROOT."
  (let ((bazel-bin (expand-file-name "bazel-bin" root)))
    (when (file-directory-p bazel-bin)
      (directory-files-recursively bazel-bin "\\.cpp_info\\.json$"))))

(defun emacs-bazel--parse-cpp-info (file)
  "Parse a single .cpp_info.json FILE and return its alist."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(defun emacs-bazel--make-compile-entry (_root info)
  "Make a compile_commands.json entry from a parsed INFO alist.
ROOT is the workspace root used as the compilation directory."
  (let ((compiler (alist-get 'compiler info))
        (args (alist-get 'args info)))
    (when (and compiler args)
      (mapconcat #'identity (cons compiler args) " "))))

(defun emacs-bazel--assemble-compile-commands (root)
  "Assemble compile_commands.json from .cpp_info.json files in ROOT.
Respects the configured mode (per-directory or single)."
  (let* ((config (emacs-bazel--get-config root))
         (mode (intern (or (alist-get 'mode config) "per-directory")))
         (cpp-info-files (emacs-bazel--find-cpp-info-files root))
         (entries-by-dir (make-hash-table :test #'equal)))
    (dolist (file cpp-info-files)
      (condition-case err
          (let* ((info (emacs-bazel--parse-cpp-info file))
                 (source-files (alist-get 'files info))
                 (command (emacs-bazel--make-compile-entry root info)))
            (when command
              (dolist (src source-files)
                (let* ((abs-src (expand-file-name src root))
                       (dir (if (eq mode 'single)
                                root
                              (file-name-directory abs-src)))
                       (entry `((directory . ,root)
                                (file . ,abs-src)
                                (command . ,(concat command " " src)))))
                  (push entry (gethash dir entries-by-dir))))))
        (error (message "emacs-bazel: error parsing %s: %s" file err))))
    ;; Write compile_commands.json per directory (or single)
    (let ((compdb-dir (alist-get 'compileCommandsDir config))
          (count 0))
      (if (and (eq mode 'single) compdb-dir)
          ;; Single mode: write to configured dir
          (let ((all-entries nil))
            (maphash (lambda (_dir entries)
                       (setq all-entries (nconc entries all-entries)))
                     entries-by-dir)
            (emacs-bazel--write-compile-commands
             (expand-file-name compdb-dir root) all-entries)
            (setq count (length all-entries)))
        ;; Per-directory mode
        (maphash (lambda (dir entries)
                   (when dir
                     (emacs-bazel--write-compile-commands dir entries)
                     (setq count (+ count (length entries)))))
                 entries-by-dir))
      (message "emacs-bazel: wrote %d compile entries (%d directories)"
               count (hash-table-count entries-by-dir)))))

(defun emacs-bazel--write-compile-commands (dir entries)
  "Write ENTRIES as compile_commands.json in DIR."
  (when (and dir entries)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((file (expand-file-name "compile_commands.json" dir)))
      (with-temp-file file
        (insert (json-serialize (vconcat entries)
                                :false-object :json-false
                                :null-object nil))))))

;;;; Interactive commands

;;;###autoload
(defun emacs-bazel-refresh ()
  "Generate compile_commands.json for the current Bazel workspace.
Runs the aspect build, then assembles compile_commands.json from
the resulting .cpp_info.json files."
  (interactive)
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root))
         (packages (emacs-bazel--packages config))
         (args (emacs-bazel--active-args config)))
    (emacs-bazel--ensure-config-dir root)
    (unless packages
      (user-error "No packages configured.  Use `emacs-bazel-add-package' first"))
    ;; Set up sentinel to assemble after build completes
    (let ((buf (emacs-bazel--build-aspects root packages args)))
      (with-current-buffer buf
        (add-hook 'compilation-finish-functions
                  (lambda (_buf msg)
                    (when (string-match-p "finished" msg)
                      (emacs-bazel--assemble-compile-commands root)))
                  nil t)))))

;;;###autoload
(defun emacs-bazel-add-package (package)
  "Add PACKAGE to the project's emacs-bazel package list."
  (interactive
   (list (read-string "Bazel package pattern (e.g. //my/pkg/...): "
                      (when buffer-file-name
                        (let* ((root (emacs-bazel--workspace-root))
                               (dir (bazel--package-directory
                                     buffer-file-name root)))
                          (when dir
                            (concat "//"
                                    (bazel--package-name dir root)
                                    "/...")))))))
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root))
         (packages (emacs-bazel--packages config)))
    (unless (member package packages)
      (push package packages)
      (setf (alist-get 'packages config) (vconcat packages))
      (emacs-bazel--write-config root config)
      (message "Added %s (%d packages total)" package (length packages)))))

;;;###autoload
(defun emacs-bazel-remove-package ()
  "Remove a package from the project's emacs-bazel package list."
  (interactive)
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root))
         (packages (emacs-bazel--packages config))
         (choice (completing-read "Remove package: " packages nil t)))
    (setq packages (delete choice packages))
    (setf (alist-get 'packages config) (vconcat packages))
    (emacs-bazel--write-config root config)
    (message "Removed %s (%d packages remaining)" choice (length packages))))

;;;###autoload
(defun emacs-bazel-select-configuration ()
  "Select the active Bazel configuration for the current workspace."
  (interactive)
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root))
         (configurations (alist-get 'configurations config))
         (names (mapcar (lambda (c) (symbol-name (car c))) configurations))
         (choice (completing-read "Configuration: " names nil t)))
    (setf (alist-get 'activeConfiguration config) choice)
    (emacs-bazel--write-config root config)
    (message "Active configuration: %s → %s"
             choice (alist-get (intern choice) configurations))))

;;;###autoload
(defun emacs-bazel-add-configuration (name)
  "Add a new Bazel configuration NAME with interactively specified args."
  (interactive "sConfiguration name: ")
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root))
         (args-str (read-string "Bazel args (space-separated): "
                                "--config= --keep_going"))
         (args (split-string args-str)))
    (setf (alist-get 'configurations config)
          (cons (cons (intern name) (vconcat args))
                (alist-get 'configurations config)))
    (emacs-bazel--write-config root config)
    (message "Added configuration '%s'" name)))

;;;###autoload
(defun emacs-bazel-set-mode (mode)
  "Set compile_commands generation MODE for the current workspace."
  (interactive (list (intern (completing-read "Mode: "
                                             '("per-directory" "single")
                                             nil t))))
  (let* ((root (emacs-bazel--workspace-root))
         (config (emacs-bazel--get-config root)))
    (setf (alist-get 'mode config) (symbol-name mode))
    (when (eq mode 'single)
      (let ((dir (read-directory-name "compile_commands.json directory: " root)))
        (setf (alist-get 'compileCommandsDir config)
              (file-relative-name dir root))))
    (emacs-bazel--write-config root config)
    (message "Mode set to %s" mode)))

(provide 'emacs-bazel)
;;; emacs-bazel.el ends here
