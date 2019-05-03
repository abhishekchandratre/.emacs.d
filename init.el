(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

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

(use-package paredit-everywhere
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1))))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))


    (progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends)))

  (global-company-mode 1))

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
  ;(eyebrowse-mode t)
  )

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

(use-package zoom-window
  :ensure t
  :config
  (global-set-key (kbd "s-f z") 'zoom-window-zoom))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

(use-package dired
  :config
  ;; Make dired less verbose
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
  ;; Move files between split panes
  (setq dired-dwim-target t)
  )

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
   (quote
    ("8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" default)))
 '(eshell-scroll-to-bottom-on-input t)
 '(eshell-scroll-to-bottom-on-output nil)
 '(package-selected-packages
   (quote
    (multiple-cursors magit helpful zoom-window shackle ob-http gruvbox-theme theme-gruvbox emacs-theme-gruvbox eyebrowse company-jedi emacs-company-jedi markdown-mode docker-tramp exec-path-from-shell company-restclient company-quickhelp company-elisp company which-key paredit-everywhere ace-window paredit use-package better-defaults)))
 '(pixel-scroll-mode t)
 '(protect-buffer-bury-p nil)
 '(tramp-default-method "ssh" nil (tramp)))

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
;; Set default font
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 160
                    :weight 'normal
                    :width 'normal)
;; org mode
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
