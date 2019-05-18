;;; init.el -- init file
;;; Commentary:

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; (add-to-list 'load-path "~/git/org-mode/lisp")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; load file
(load-file "~/.emacs.d/abh-fun.el")

;; packages
(use-package better-defaults
  :ensure t)

(use-package flycheck
:ensure t
:init
(global-flycheck-mode t))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20)
  ;; Default colors are awful - borrowed these from gocode (thanks!):
  ;; https://github.com/nsf/gocode/tree/master/emacs-company#color-customization
  (set-face-attribute
   'company-preview nil :foreground "black" :underline t)
  (set-face-attribute
   'company-preview-common nil :inherit 'company-preview)
  (set-face-attribute
   'company-tooltip nil :background "lightgray" :foreground "black")
  (set-face-attribute
   'company-tooltip-selection nil :background "steelblue" :foreground "white")
  (set-face-attribute
   'company-tooltip-common nil :foreground "darkgreen" :weight 'bold)
  (set-face-attribute
   'company-tooltip-common-selection nil :foreground "black" :weight 'bold))

(setq-local company-backend '(company-elisp))

(use-package company-quickhelp
  :after company
  :ensure t
  :bind (:map company-active-map
              ("C-c ?" . company-quickhelp-manual-begin)))

(use-package company-restclient
  :ensure t
  :after (company restclient))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package shackle
  :ensure t
  :config
  (shackle-mode 1)
  (setq shackle-rules '((compilation-mode :noselect t))
        shackle-default-rule '(:select t)))

(use-package gruvbox-theme
  :ensure t
  :config
  ;; (load-theme 'gruvbox t)
  )

(require 'dired)
(require 'dired-x)
(use-package dired
  :config
  ;; Make dired less verbose
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
  ;; Move files between split panes
  (setq dired-dwim-target t))

(use-package page-break-lines
  :ensure t
  :config
  (add-hook 'help-mode-hook 'page-break-lines-mode))

(use-package ledger-mode
  :ensure t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :custom (ledger-clear-whole-transactions t))

  (use-package flycheck-ledger :after ledger-mode :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output nil)
 '(custom-safe-themes
   '("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" default))
 '(eshell-scroll-to-bottom-on-input t)
 '(eshell-scroll-to-bottom-on-output nil)
 '(package-selected-packages
   '(flycheck-ledger ledger-mode page-break-lines form-feed flycheck multiple-cursors magit helpful zoom-window shackle ob-http gruvbox-theme theme-gruvbox emacs-theme-gruvbox eyebrowse company-jedi emacs-company-jedi markdown-mode docker-tramp exec-path-from-shell company-restclient company-quickhelp company-elisp company which-key paredit-everywhere ace-window paredit use-package better-defaults))
 '(pixel-scroll-mode nil)
 '(protect-buffer-bury-p nil)
 '(tramp-default-method "ssh"))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))
;; my settings
;; (load-theme 'leuven t nil)
(setq visible-bell nil)
(setq show-paren-delay 0)
(setq show-trailing-whitespace t)
(setq show-paren-style 'mixed)
(setq set-mark-command-repeat-pop t)
;; Set default font
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 160
                    :weight 'normal
                    :width 'normal)
;; org mode
(require 'org)
(require 'org-capture)
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (http . t)))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("b" "Budget" table-line (file+olp+datetree "~/Dropbox/neworg/budget.org" "Transactions")
         "| %^{Description} | %^{Cost} | %^{prompt|essentials|security|goals|lifestyle|discretionary} |" :prepend t :kill-buffer t :unnarrowed t)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(setq gc-cons-threshold 50000000)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(setq-default tab-width 4
              indent-tabs-mode nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)
(winner-mode 1)
;;; init.el ends here
