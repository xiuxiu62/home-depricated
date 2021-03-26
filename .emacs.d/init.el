;; Package Manager Configuration  -------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)
(setq global-command-log-mode t)

;; Basic Configuration  -----------------------------------------------------------------------

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq disabled-command-function nil)
(setq custom-file "~/.emacs.d/custom.el")

;; Style Configuration  -----------------------------------------------------------------------

(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; opacity                               (focused unfocused)
(set-frame-parameter (selected-frame) 'alpha '(85 75))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 5)
(electric-pair-mode 1)

(setq visible-bell t)
(setq inhibit-startup-message t
      initial-scratch-message nil)

(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
				shell-mode-hook
				eshell-mode-hook
				term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package all-the-icons)
;; theme
(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))

(setq-default tab-width 4)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15)) 

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))




;; Font Configuration  ------------------------------------------------------------------------

(defvar xiumacs/default-font-size 110)

(set-face-attribute 'default nil :font "Fira Code Retina:antialias=subpixel" :height xiumacs/default-font-size)

;; Ivy Configuration  -------------------------------------------------------------------------

(use-package swiper)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Keybinding Configuration  ---------------------------------------------------------------------

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package general
  :config
  (general-create-definer xiu/leader-keys
	:keymaps '(normal insert visual)
	:prefix "SPC"
	:global-prefix "C-SPC")

  (xiu/leader-keys
	"t" '(:ignore t :which-key "toggles")
	"tt" '(counsel-load-theme :which-key "choose theme")))


;; EVIL MODE

(defun xiu/evil-hook ()
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

(defun xiu/dont-use-arrows ()
  (interactive)
  (message "arrow keys make fingies go brrrrrr"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (add-hook 'evil-mode-hook 'xiu/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'xiu/dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<right>") 'xiu/dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<down>") 'xiu/dont-use-arrows)
  (define-key evil-normal-state-map (kbd "<up>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<left>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<right>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<down>") 'xiu/dont-use-arrows)
  (evil-global-set-key 'motion (kbd "<up>") 'xiu/dont-use-arrows)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(xiu/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

;; Projectile Configuration  ---------------------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projetile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "D:/Development/")
  	(setq projectile-project-search-path '("D:/Development/")))
  (when (file-directory-p "D:/Development/Bootcamp/")
  	(setq projectile-project-search-path '("D:/Development/Bootcamp/")))
  (when (file-directory-p "D:/Development/4D_Technologies/CADLearning/")
  	(setq projectile-project-search-path '("D:/Development/4D_Technologies/CADLearning/")))
  (when (file-directory-p "D:/Development/4D_Technologies/Staccato/")
  	(setq projectile-project-search-path '("D:/Development/4D_Technologies/Staccato/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit Configuration  --------------------------------------------------------------------------

;; C-c g to activate
(use-package magit
;;  :commands (magit-status magit-get-current-brach)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(use-package forge)

;; Org Mode Configuration -------------------------------------------------------------------------

(defun xiu/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun xiu/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina:antialias=subpixel" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . xiu/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (xiu/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun xiu/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . xiu/org-mode-visual-fill))

;; LSP Mode Configuration  ------------------------------------------------------------------------

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l") ;; or 'C-l'
;;   :config
;;   (lsp-enable-which-key-integration t))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 0))

;; DART/FLUTTER INTEGRATION
;; (use-package lsp-mode
;;   :ensure t)
;; (setq lsp-mode t)
;; (use-package lsp-dart
;;   :ensure t
;;   :hook (dart-mode . lsp))

;; ;; Optional packages
;; (use-package projectile :ensure t) ;; project management
;; (use-package yasnippet
;;   :ensure t
;;   :config (yas-global-mode)) ;; snipets
;; (use-package lsp-ui :ensure t) ;; UI for LSP
;; (use-package company :ensure t) ;; Auto-complete

;; ;; Optional Flutter packages
;; (use-package hover :ensure t) ;; run app from desktop without emulator

(use-package chess)
