;;; init.el -- init file
;;; Commentary:

;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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

(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)))

;; packages
(use-package better-defaults
  :ensure t)

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package flycheck
:ensure t
:init
(global-flycheck-mode t))

(use-package projectile
  :ensure t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package eyebrowse
  :ensure t
  :bind (
         ("C-c w x" . eyebrowse-close-window-config)
         ("C-c w c" . eyebrowse-create-window-config)
         ("C-c w i" . eyebrowse-prev-window-config)
         ("C-c w o" . eyebrowse-next-window-config))
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c w"))
  (global-unset-key (kbd "C-c C-w"))
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))


(use-package paredit
  :ensure t
  :config
  (paredit-mode))

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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package dired
  :config
  (require 'dired-x)
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

(use-package flycheck-ledger
  :after ledger-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

;; my settings
(setq visible-bell t)
(setq show-paren-delay 0)
(setq show-trailing-whitespace t)
(setq show-paren-style 'mixed)
(setq set-mark-command-repeat-pop t)
(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(setq-default tab-width 4
              indent-tabs-mode nil)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)
(column-number-mode 1)

(use-package winner
  :unless noninteractive
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))


;; Set default font
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; org mode
(require 'org)
(require 'org-capture)
(require 'org-mouse)
(setq org-catch-invisible-edits t)
(use-package ob-http
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (http . t)))

(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" . "☐" ) prettify-symbols-alist)
   (push '("[X]" . "☑" ) prettify-symbols-alist)
   (push '("[-]" . "❍" ) prettify-symbols-alist)
   (prettify-symbols-mode)))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("b" "Budget" table-line (file+olp+datetree "~/Dropbox/neworg/budget.org" "Transactions")
         "| %^{Description} | %^{Cost} | %^{prompt|essentials|security|goals|lifestyle|discretionary} |" :prepend t :kill-buffer t :unnarrowed t)
        ("j" "Jira" entry (file+olp "~/Dropbox/neworg/projects.org" "Office" "JIRA")
         "* TODO %?\n  %i\n  %a")))

(use-package ispell
  :no-require t
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))

(use-package ivy
  :diminish
  :demand t
  :ensure t

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume))

  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("SPC"   . ivy-alt-done-or-space)
              ("C-d"   . ivy-done-or-delete-char)
              ("C-i"   . ivy-partial-or-done)
              ("C-r"   . ivy-previous-line-or-history)
              ("M-r"   . ivy-reverse-i-search))

  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill))

  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)

  :preface
  (defun ivy-done-or-delete-char ()
    (interactive)
    (call-interactively
     (if (eolp)
         #'ivy-immediate-done
       #'ivy-delete-char)))

  (defun ivy-alt-done-or-space ()
    (interactive)
    (call-interactively
     (if (= ivy--length 1)
         #'ivy-alt-done
       #'self-insert-command)))

  (defun ivy-switch-buffer-kill ()
    (interactive)
    (debug)
    (let ((bn (ivy-state-current ivy-last)))
      (when (get-buffer bn)
        (kill-buffer bn))
      (unless (buffer-live-p (ivy-state-buffer ivy-last))
        (setf (ivy-state-buffer ivy-last)
              (with-ivy-window (current-buffer))))
      (setq ivy--all-candidates (delete bn ivy--all-candidates))
      (ivy--exhibit)))

  ;; This is the value of `magit-completing-read-function', so that we see
  ;; Magit's own sorting choices.
  (defun my-ivy-completing-read (&rest args)
    (let ((ivy-sort-functions-alist '((t . nil))))
      (apply 'ivy-completing-read args)))

  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package counsel
  :after ivy
  :demand t
  :ensure t
  :diminish
  :custom (counsel-find-file-ignore-regexp
           (concat "\\(\\`\\.[^.]\\|"
                   (regexp-opt completion-ignored-extensions)
                   "\\'\\)"))
  :bind (("C-*"     . counsel-org-agenda-headlines)
         ("C-x C-f" . counsel-find-file)
         ("C-c e l" . counsel-find-library)
         ("C-c e q" . counsel-set-variable)
         ("C-h f"   . counsel-describe-function)
         ("C-x r b" . counsel-bookmark)
         ("M-x"     . counsel-M-x)
         ;; ("M-y"     . counsel-yank-pop)

         ("M-s f" . counsel-file-jump)
         ;; ("M-s g" . counsel-rg)
         ("M-s j" . counsel-dired-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config
  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date))

  (defun counsel-recoll-function (string)
    "Run recoll for STRING."
    (if (< (length string) 3)
        (counsel-more-chars 3)
      (counsel--async-command
       (format "recollq -t -b %s"
               (shell-quote-argument string)))
      nil))

  (defun counsel-recoll (&optional initial-input)
    "Search for a string in the recoll database.
  You'll be given a list of files that match.
  Selecting a file will launch `swiper' for that file.
  INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (counsel-require-program "recollq")
    (ivy-read "recoll: " 'counsel-recoll-function
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-git-grep-history
              :action (lambda (x)
                        (when (string-match "file://\\(.*\\)\\'" x)
                          (let ((file-name (match-string 1 x)))
                            (find-file file-name)
                            (unless (string-match "pdf$" x)
                              (swiper ivy-text)))))
              :unwind #'counsel-delete-process
              :caller 'counsel-recoll)))

(use-package docker-tramp
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/elpa/notmuch/")

(defvar abhi-notmuch-keymap nil "Notmuch Shortcuts.")
(setq abhi-notmuch-keymap (make-sparse-keymap))
(global-set-key (kbd "C-c n") abhi-notmuch-keymap)

(use-package notmuch
  :bind (
         :map abhi-notmuch-keymap
         ("n" . notmuch)
         ("s" . notmuch-search)
         ("z" . notmuch-tree))
  :config
  )

(use-package starttls
  :config
  (setq starttls-use-gnutls t))

(use-package smtpmail
  :config
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials (expand-file-name "~/.authinfo")
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-stream-type 'starttls smtpmail-smtp-service 587)
  (setq smtpmail-debug-info t)
  (setq smtpmail-debug-verb t))


(use-package spacemacs-common
  :ensure spacemacs-theme)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(comint-scroll-show-maximum-output nil)
 '(counsel-find-file-ignore-regexp
   "\\(\\`\\.[^.]\\|\\(?:\\.\\(?:aux\\|b\\(?:bl\\|in\\|lg\\|zr/\\)\\|c\\(?:lass\\|ps?\\)\\|d\\(?:\\(?:64fs\\|fs\\|x\\(?:\\(?:32\\|64\\)fs\\)?\\)l\\)\\|elc\\|f\\(?:asl?\\|mt\\|ns?\\|\\(?:x\\(?:\\(?:32\\|64\\)f\\)\\)?sl\\)\\|g\\(?:it/\\|[lm]o\\)\\|hg/\\|idx\\|kys?\\|l\\(?:bin\\|ib\\|o[ft]\\|x\\(?:\\(?:32\\|64\\)fsl\\)\\|[ano]\\)\\|m\\(?:em\\|o\\)\\|p\\(?:64fsl\\|fsl\\|gs?\\|y[co]\\)\\|s\\(?:o\\|parcf\\|vn/\\|x\\(?:\\(?:32\\|64\\)fsl\\)\\)\\|t\\(?:fm\\|oc\\|ps?\\)\\|ufsl\\|vrs?\\|wx\\(?:\\(?:32\\|64\\)fsl\\)\\|x86f\\|[ao]\\)\\|CVS/\\|_\\(?:\\(?:MTN\\|darcs\\)/\\)\\|~\\)\\'\\)")
 '(custom-safe-themes
   '("4e10cdf7d030fb41061cf57c74f6ddfc19db8d4af6c8e0723dc77f9922543a3d" default))
 '(ivy-dynamic-exhibit-delay-ms 200)
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil)
 '(ivy-magic-tilde nil)
 '(ivy-re-builders-alist '((t . ivy--regex-ignore-order)) t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(ledger-clear-whole-transactions t t)
 '(notmuch-search-oldest-first nil)
 '(package-selected-packages
   '(spacemacs-common minions doom-modeline eyebrowse counsel-projectile projectile doom-themes emacs-doom-themes spacemacs-theme notmuch docker-tramp counsel ivy which-key use-package paredit ob-http multiple-cursors magit ledger-mode flycheck-ledger exec-path-from-shell dashboard company-restclient company-quickhelp company-jedi better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
(put 'erase-buffer 'disabled nil)
