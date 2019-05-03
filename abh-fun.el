;;; Tmux like keybinding

(defun abh-eshell ()
  (eshell 'N))

(defun abh-split-below ()
  (interactive)
  (progn (split-window-below) (other-window 1) (abh-eshell)))

(defun abh-split-right ()
  (interactive)
  (progn (split-window-right) (other-window 1) (abh-eshell)))

(define-prefix-command 'abh-tmux-map)

(global-unset-key (kbd "s-f"))
(global-set-key (kbd "s-f") 'abh-tmux-map)
(define-key abh-tmux-map (kbd "|") 'abh-split-right)
(define-key abh-tmux-map (kbd "-") 'abh-split-below)
(define-key abh-tmux-map (kbd "h") 'windmove-left)
(define-key abh-tmux-map (kbd "j") 'windmove-down)
(define-key abh-tmux-map (kbd "k") 'windmove-up)
(define-key abh-tmux-map (kbd "l") 'windmove-right)
(define-key abh-tmux-map (kbd "b") 'rename-buffer)
(define-key abh-tmux-map (kbd "i") 'eyebrowse-prev-window-config)
(define-key abh-tmux-map (kbd "o") 'eyebrowse-next-window-config)
(define-key abh-tmux-map (kbd "c") 'eyebrowse-create-window-config)
(define-key abh-tmux-map (kbd ",") 'eyebrowse-rename-window-config)
(define-key abh-tmux-map (kbd "&") 'eyebrowse-close-window-config)
(define-key abh-tmux-map (kbd "x") 'kill-buffer-and-window)
(define-key abh-tmux-map (kbd "X") 'kill-current-buffer)
(define-key abh-tmux-map (kbd "d") 'delete-window)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(set-face-attribute 'fringe nil :background nil)

(setq eyebrowse-new-workspace t)
(setq eyebrowse-wrap-around t)
