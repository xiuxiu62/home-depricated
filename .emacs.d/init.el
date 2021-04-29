;; -----------------------------------------------------------------------------
;; ---------------------- XIU-MACS -- by: Justin cremer ------------------------
;; -----------------------------------------------------------------------------

;; Startup ---------------------------------------------------------------------

(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
							  (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Make sure correct char standard is used on Windows (thanks Bill)
(set-default-coding-systems 'utf-8)

;; Package Management ----------------------------------------------------------

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; Backups ---------------------------------------------------------------------

;; Sets a buffered backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq disabled-command-function nil)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Auto save -------------------------------------------------------------------

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Font ------------------------------------------------------------------------

(defvar default-font-size 110)

(set-face-attribute 'default nil
					:font "Fira Code Retina:antialias=subpixel"
					:height default-font-size)

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; Unicode Ligatures -----------------------------------------------------------

(defun replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                     (lambda (i) (string-equal (car i) block-name))
                     unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :custom (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config (unicode-fonts-setup))

;; Style -----------------------------------------------------------------------

(setq initial-frame-alist '((fullscreen . maximized)))

(unless nil
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 5)
  (electric-pair-mode 1)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 1)
  (setq use-dialog-box nil)
  (setq visible-bell t)
  (setq inhibit-startup-message t))

(dolist (mode '(org-mode-hook
				shell-mode-hook
				eshell-mode-hook
				term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))

(setq-default tab-width 4)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15)) 

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package diminish)

;; Which Key  ------------------------------------------------------------------

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

;; General ---------------------------------------------------------------------

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer leader-key-def
	:keymaps '(normal insert visual)
	:prefix "SPC"
	:global-prefix "C-SPC")
  (general-create-definer ctrl-c-keys
	:prefix "C-c"))

(leader-key-def
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

;; Ivy -------------------------------------------------------------------------

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
		 ("C-;" . comment-line)
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-alt-done)
		 ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t) (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path ; return file path relative to project root or `default-directory' if project is nil
					   (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
		 ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (setq ivy-initial-inputs-alist nil))

(use-package flx ;; Imroves sorting for fuzzy-matched results
  :defer t
  :after ivy
  :init (setq ivy-flx-limit 10000))

(use-package wgrep)
(use-package ripgrep)

(use-package ivy-posframe
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(leader-key-def
  "r"   '(ivy-resume :which-key "ivy resume")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "C-f" 'counsel-find-file
  "fr"  '(counsel-recentf :which-key "recent files")
  "fR"  '(revert-buffer :which-key "revert file")
  "fj"  '(counsel-file-jump :which-key "jump to file"))

;; Avy -------------------------------------------------------------------------

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(leader-key-def
  "j"   '(:ignore t :which-key "jump")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

;; Helpful ---------------------------------------------------------------------

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(leader-key-def
  "e"  '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer"))

(leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

;; Evil -------------------------------------------------------------------

(defun evil-hook ()
  (dolist (mode '(custom-mode
				  shell-mode
				  eshell-mode
				  term-mode
				  git-rebase-mode
				  erc--mode
				  circe-server-mode
				  circe-chat-mode
				  circe-query-mode
				  sauron-mode))
	(add-to-list 'evil-emacs-state-modes mode)))

(defun dont-use-arrows ()
  (interactive)
  (message "arrow keys make fingers go brrrrrr"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (add-hook 'evil-mode-hook 'evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "<left>") 'dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<right>") 'dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<down>") 'dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<up>") 'dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<left>") 'dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<right>") 'dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<down>") 'dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<up>") 'dont-use-arrows)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Hydra -----------------------------------------------------------------------

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(leader-key-def
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

;; Projectile ------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projetile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit -----------------------------------------------------------------------

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package forge
  :disabled)

(use-package magit-todos
  :defer t)

;; Org -------------------------------------------------------------------------

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun org-mode-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina:antialias=subpixel" :weight 'regular :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (org-mode-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

;; LSP Mode --------------------------------------------------------------------

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l") ;; or 'C-l'
  :config (lsp-enable-which-key-integration t))

;; Typescript ------------------------------------------------------------------

(use-package nvm
  :defer t)

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config (setq typescript-indent-level 2))

(defun set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(defun js2-config ()
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'set-js-indentation)
  (add-hook 'json-mode-hook #'set-js-indentation))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  (js2-config))

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

;; HTML ------------------------------------------------------------------------

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

;; Markdown --------------------------------------------------------------------

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun markdown-mode-hook ()
    (set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'markdown-mode-hook))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; Golang ----------------------------------------------------------------------

;; (use-package go-mode)

;; Common Lisp -----------------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/sbcl")

(use-package sly
  ;; :disabled
  :mode "\\.lisp\\'")

(use-package slime 
  :disabled
  :mode "\\.lisp\\'")

;; Flycheck --------------------------------------------------------------------

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; Yasnippet -------------------------------------------------------------------

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

;; Smart Parens ----------------------------------------------------------------

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; Misc  -----------------------------------------------------------------------
;; TODO: clean up
;; (use-package projectile :ensure t) ;; project management
;; (use-package lsp-ui :ensure t) ;; UI for LSP
;; (use-package company :ensure t) ;; Auto-complete
;; (use-package hover :ensure t) ;; run app from desktop without emulator
