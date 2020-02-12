;;; emacs.el --- My personnal Emacs configuration

;;; Commentary:
;;
;; Emacs dependencies: company dashboard multiple-cursors projectile tide typescript-mode yasnippet go-mode go-guru go-eldoc web-mode esup bug-hunter
;; External dependencies: multimarkdown go godef
;;
;; Symlinks
;;
;; $ ln -s /path/to/playground/emacs/emacs.el ~/.emacs
;; $ ln -s /path/to/playground/emacs/snippets ~/.emacs.d/snippets
;;
;; Daemon
;;
;; $ cp /path/to/playground/emacs/emacs.service ~/.config/systemd/user
;; $ systemct start emacs.service
;; $ emacsclient --create-frame --quiet -n

;;; TODO:
;;
;; Documentation
;;
;; Modes
;;   Markdown (with preview and style)
;;   LateX (with preview and style)
;;   ORG
;;
;; Profiler
;;
;; line-num seulement en programmation
;;
;; Compile dependencies

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;          General          ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Memory
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Answer y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Packages manager
(require 'package)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(defvar use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 30)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
	    (lambda () (message "I will update packages now"))))

;; Replace highlighted text
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;           DEBUG           ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq message-log-max t)

(use-package esup
  :ensure t
  :pin melpa
  :commands (esup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;         INTERFACE         ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Themes
(use-package base16-theme
  :config
  (load-theme 'base16-horizon-terminal-dark t))

;; Window bars
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current cursor's line
(global-hl-line-mode +1)

;; Line number
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)

;; Fringe size (border size)
(fringe-mode '(1 . 1))

;; Maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; Set frame title to opened buffer name
(setq frame-title-format
  (list (format "%%S %%j ")
    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Scroll
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-scroll-amount '(3 ((shift) . 3))
      scroll-step 3
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil)

;; Trailing whitespaces
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; Print cursor's line numbers/columns
(setq column-number-mode t)

;; Ask before close
(setq confirm-kill-emacs 'y-or-n-p)

;; Get to the next buffer with C-<TAB>
(global-set-key (kbd "<C-tab>") 'other-window)

;; Undo/Redo
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-z") 'undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo))

;; Desktop
;; (desktop-save-mode 1)
;; (desktop-save-frameset 1)
;; (desktop--check-dont-save 1)

;; Indentation hightlight
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (set-face-background 'highlight-indent-guides-odd-face "gray50")
  (set-face-background 'highlight-indent-guides-even-face "gray80")
  (set-face-foreground 'highlight-indent-guides-character-face "gray100"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;;         PACKAGES         ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable git mode
(setq vc-handled-backends ())

;; Minions
(use-package minions
  :config (minions-mode 1))

;; Highlight matching parenthesis
(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1c1e26" "#e95678" "#29d398" "#fac29a" "#26bbd9" "#ee64ac" "#26bbd9" "#cbced0"])
 '(ansi-term-color-vector
   [unspecified "#1c1e26" "#e95678" "#29d398" "#fac29a" "#26bbd9" "#ee64ac" "#26bbd9" "#cbced0"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (base16-horizon-terminal-dark)))
 '(custom-safe-themes
   (quote
    ("043c8375cad0cf1d5c42f5d85cbed601075caf09594da04a74712510e9437d2b" "679ee3b86b4b34661a68ba45bbd373eab0284caee6249139b2a090c9ddd35ce0" "7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" "042b095e7ad996515b1037162100b9cd9d3c57f1fd2d7e70ac5c57770a01cc4d" "17c312391e3a908d761d42bd71367f3f9deb45df79b13b6f82ad57064ae9eebb" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(nil nil t)
 '(package-selected-packages
   (quote
    (bug-hunter base16-theme sunburn-theme zenburn-theme latex-preview-pane auto-package-update markdown-mode flycheck dashboard flymake-go go-autocomplete auto-complete company-go exec-path-from-shell go-guru godoctor go-eldoc go-mode esup smartparens web-mode minions projectile yasnippet multiple-cursors company typescript-mode tide json-mode yaml-mode)))
 '(tool-bar-mode nil)
 '(typescript-indent-level 2))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)

  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 30) (projects . 10)))
  (setq dashboard-set-footer nil)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; Projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; Multiple cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c m c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;; YAS code snippets
(add-to-list 'load-path "~/.emacs.d/snippets")
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Company-mode
(use-package company
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)

  (custom-set-faces
   '(company-echo-common ((t (:underline t))))
   '(company-preview ((t (:inherit shadow))))
   '(company-preview-common ((t (:inherit company-preview :underline t))))
   '(company-scrollbar-bg ((t (:inherit company-tooltip :background "SteelBlue3"))))
   '(company-scrollbar-fg ((t (:background "DeepSkyBlue4"))))
   '(company-template-field ((t (:background "DeepSkyBlue3" :foreground "black"))))
   '(company-tooltip ((t (:background "LightSteelBlue1" :foreground "dark slate gray"))))
   '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "slate gray"))))
   '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :background "LightSteelBlue3"))))
   '(company-tooltip-common ((t (:inherit company-tooltip :underline t))))
   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :underline t))))
   '(company-tooltip-mouse ((t (:inherit company-tooltip-selection))))
   '(company-tooltip-selection ((t (:inherit company-tooltip :background "LightSteelBlue3")))))

  (add-hook 'after-init-hook 'global-company-mode))

;; Flycheck
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Persistent *scratch*
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;;         CUSTOM         ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-line()
  "Dupplicate the cursor's current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))
(global-set-key (kbd "C-d") 'duplicate-line)

(defun revert-buffer-all ()
  "Refreshes all open buffers from their respective files."
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

(defun move-text-internal (arg)
  "Move region ARG up or down."
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
  "Move region or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;         LANGUAGES         ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
	    (font-lock-add-keywords nil
	     '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
		1 font-lock-warning-face t)))))

;; Typescript
(use-package tide
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)

  (defun setup-tide-mode ()
    "Set up a full typescript environment."
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; Golang
(use-package go-mode
  :config
  (defun setup-go-mode ()
    "Define function to call on go-mode."

    (setenv "GOROOT" (shell-command-to-string ". ~/.zshrc; echo -n $GOROOT"))
    (setenv "GOPATH" (shell-command-to-string ". ~/.zshrc; echo -n $GOPATH"))

    (add-hook 'before-save-hook 'gofmt-before-save)
    (defvar gofmt-command "goimports")
    (if (not (string-match "go" compile-command))
	(set (make-local-variable 'compile-command)
	     "go build -v && go vet -v"))

    (go-guru-hl-identifier-mode)

    (setq tab-width 2)
    (setq indent-tabs-mode 1)

    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (local-set-key (kbd "M-p") 'compile)
    (local-set-key (kbd "M-P") 'recompile)
    (local-set-key (kbd "M-[") 'previous-error)
    (local-set-key (kbd "M-]") 'next-error)

    (set (make-local-variable 'company-backends) '(company-go))

    (go-eldoc-setup))

  (add-hook 'go-mode-hook 'setup-go-mode))

;; Web-mode (Javascript/PHP/HTML/CSS)
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-indentation t))

;; JSON
(add-hook 'json-mode-hook (defvar js-indent-level 2))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml"))

;; Markdown
(use-package markdown-mode
  :mode
  (("README\\.md\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;; LateX
(latex-preview-pane-enable)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo-common ((t (:underline t))))
 '(company-preview ((t (:inherit shadow))))
 '(company-preview-common ((t (:inherit company-preview :underline t))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "SteelBlue3"))))
 '(company-scrollbar-fg ((t (:background "DeepSkyBlue4"))))
 '(company-template-field ((t (:background "DeepSkyBlue3" :foreground "black"))))
 '(company-tooltip ((t (:background "LightSteelBlue1" :foreground "dark slate gray"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "slate gray"))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :background "LightSteelBlue3"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :underline t))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :underline t))))
 '(company-tooltip-mouse ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:inherit company-tooltip :background "LightSteelBlue3")))))

;;; emacs.el ends here
