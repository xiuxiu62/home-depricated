;;; init.el --- Start this puppy up      -*- lexical-binding: t no-byte-compile: t-*-

;; -----------------------------------------------------------------------------
;; ----------------------- XIUMACS --- by: Justin cremer -----------------------
;; ------------------ https://github.com/justincremer/.emacs.d -----------------
;; -----------------------------------------------------------------------------

;;; Commentary:
;;
;; Turns words into syscalls baby
;;

;;; Code:

;; Startup ---------------------------------------------------------------------

;; Set garbage collection threshold for performance reasons
;; (setq gc-cons-threshold (* 50 1000 1000))

(defvar xiu/gc-cons-threshold (if (display-graphic-p) 64000000 1600000))

(defvar xiu/gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000))

(defvar xiu/gc-timer (run-with-idle-timer 10 t #'garbage-collect))

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold xiu/gc-cons-upper-limit
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold xiu/gc-cons-threshold
                  gc-cons-percentage 0.1)

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold xiu/gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold xiu/gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path -------------------------------------------------------------------

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages  -------------------------------------------------------------------

(require 'init-package)
(require 'init-basic)
(require 'init-ui)
(require 'init-dashboard)

(require 'init-eshell)
(require 'init-shell)

(require 'init-perspective)
(require 'init-general)
(require 'init-treemacs)
(require 'init-ivy)
(require 'init-yasnippet)
(require 'init-evil)

(require 'init-projectile)
(require 'init-magit)
(require 'init-org)
(require 'init-lsp)
(require 'init-company)
(require 'init-dap)
(require 'init-docker)
(require 'init-lisp)
(require 'init-elisp)
(require 'init-clisp)
(require 'init-markdown)
(require 'init-web)
(require 'init-dart)
(require 'init-go)
(require 'init-haskell)

;;; init.el ends here
