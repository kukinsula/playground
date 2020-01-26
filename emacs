;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


;; Themes
(add-to-list 'custom-theme-load-path "/home/kuk/.emacs.d/themes")


;; Window bars
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; Line Number
(global-linum-mode t)
(setq linum-format "%d ")


;; Fringe size (border size)
(fringe-mode '(1 . 1))


;; Maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Set font size to 11
(set-face-attribute 'default nil :font "Mono 11")


;; Scroll 3 lines at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 3) ;; keyboard scroll one line at a time


;; Show whitespaces
(setq-default show-trailing-whitespace t)


;; Startup buffer to list files
(setq inhibit-splash-screen t)


;; Print cursor's line numbers/columns
(setq column-number-mode t)


;; Set frame title to opened buffer name
(setq frame-title-format
  (list (format "%s %%S: %%j " (system-name))
    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


;; Ask before close
(setq confirm-kill-emacs 'y-or-n-p)


;; Higlight matching parenthesis/brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)


;; Get to the next buffer with C-<TAB>
(global-set-key (kbd "<C-tab>") 'other-window)


;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 30)))
(setq dashboard-set-footer nil)


;; Powerline
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")


;; Multiple cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Code snippets
(add-to-list 'load-path "~/.emacs.d/plugins/snippets")
(require 'yasnippet)
(yas-global-mode 1)


;; Disable git inside emacs
(setq vc-handled-backends ())


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (blackboard)))
 '(custom-safe-themes
   (quote
    ("042b095e7ad996515b1037162100b9cd9d3c57f1fd2d7e70ac5c57770a01cc4d" "17c312391e3a908d761d42bd71367f3f9deb45df79b13b6f82ad57064ae9eebb" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(nil nil t)
 '(package-selected-packages
   (quote
    (projectile dashboard yasnippet-snippets yasnippet multiple-cursors company graphql-mode typescript-mode docker centaur-tabs vue-mode tide json-mode yaml-mode go-mode auto-complete)))
 '(tool-bar-mode nil)
 '(typescript-indent-level 2))


;; Company-mode
(add-hook 'after-init-hook 'global-company-mode) ;; Globally enable Company mode
(setq company-tooltip-align-annotations t) ;; Aligns annotation to the right hand side


;; C-d to dupplicate the cursor's current line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-d") 'duplicate-line)


;; Revert all buffers
(defun revert-buffer-all ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
	 (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
		 (not (buffer-modified-p buffer)))
	(set-buffer buffer)
	(revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))


;; Move region up or down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
	(transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)


;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))


;; Formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; JSON
(add-hook 'json-mode-hook #'flycheck-mode)
