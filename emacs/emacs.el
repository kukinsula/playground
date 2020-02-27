;;; emacs.el --- My personnal Emacs configuration

;;; Commentary:
;;
;; Emacs dependencies: company dashboard multiple-cursors projectile tide typescript-mode yasnippet go-mode go-guru go-eldoc web-mode esup bug-hunter
;; External dependencies: multimarkdown go godef
;;
;; * Symlinks
;;
;; $ ln -s /path/to/playground/emacs/emacs.el ~/.emacs
;; $ ln -s /path/to/playground/emacs/snippets ~/.emacs.d/snippets
;;
;; * Daemon
;;
;; $ cp /path/to/playground/emacs/emacs.service ~/.config/systemd/user
;; $ systemct start emacs.service
;; $ emacsclient --create-frame --quiet -n

;;; TODO:
;;
;; Code documentation
;;
;; M-x package-install RET list-of-packages-to-install (d'une trève)
;;
;; LSP => tester emacs27
;;
;; Which key ou remind-bindings
;;
;; use-package : utiliser proprement les tags :config, :bind, :hook, ...
;;
;; minions VS use-package's diminish or delight
;;
;; company-quickhelp
;;
;; web-mode VS HTML/CSS/PHP modes
;;
;; KBD C-S-e/C-S-o separation vertical/horizontal

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

;; Save mini buffer history
(savehist-mode 1)
(defvar savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Transparently open compressed files
(auto-compression-mode t)

;; UTF-8
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; PATH
(defvar exec-path-from-shell-check-startup-files nil)
(exec-path-from-shell-initialize)

;; Disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;           DEBUG           ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq message-log-max t)

;; (use-package esup
;;   :ensure t
;;   :pin melpa
;;   :commands (esup))

;; (use-package bug-hunter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;         INTERFACE         ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Themes
(use-package base16-theme
	:init
  (setq base16-highlight-mode-line 'contrast)
  :config
  (load-theme 'base16-horizon-terminal-dark t))

;; Window bars
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current cursor's line
(global-hl-line-mode +1)

;; Line number
(add-hook 'prog-mode-hook '(lambda () (linum-on)))
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

;; Ask before close
(setq confirm-kill-emacs 'y-or-n-p)

;; Undo/Redo
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
	:bind (("C-z" . 'undo)
				 ("C-S-z" . 'redo)))

;; Desktop
;; (desktop-save-mode 1)
;; (desktop-save-frameset 1)
;; (desktop--check-dont-save 1)

;; Indentation hightlight
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
	:hook
	(prog-mode . highlight-indent-guides-mode))

;; Tabs
(defvar custom-tab-width 2)

(defun enable-tabs  ()
  "Enable TABs."
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(add-hook 'prog-mode-hook 'enable-tabs)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab
(setq backward-delete-char-untabify-method 'hungry)

;; Show pairing parenthesis and brackets
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;;         PACKAGES         ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Disable git mode
(setq vc-handled-backends ())

;; Minions
(use-package minions
  :config (minions-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#1c1e26" "#e95678" "#29d398" "#fac29a" "#26bbd9" "#ee64ac" "#26bbd9" "#cbced0"])
 '(ansi-term-color-vector
	 [unspecified "#1c1e26" "#e95678" "#29d398" "#fac29a" "#26bbd9" "#ee64ac" "#26bbd9" "#cbced0"])
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes (quote (base16-horizon-terminal-dark)))
 '(custom-safe-themes (quote (default base16-horizon-terminal-dark)))
 '(nil nil t)
 '(package-selected-packages
	 (quote
		(company-css company-web-html company-web go-eldoc bug-hunter org-bullets py-autopep8 pip-requirements elpy auto-autopep8 base16-bug bullets-company company company-cursors dashboard-eldoc elpy-elpygen-esup exec-exec from from-go go-go go guru-hunter jedi jedi-markdown minions-mode mode-mode mode-mode mode mode-multiple org-package pane-path-path-persistent pip-preview-preview projectile py-pyenv-rainbow-requirements scratch shell shell smex themelatex-tide typescript update-web-yaml-yasnippet)))
 '(tool-bar-mode nil)
 '(typescript-indent-level 2))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 30) (projects . 10)))
  (setq dashboard-set-footer nil)
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
	(dashboard-modify-heading-icons '((recents . "file-text")
																		(bookmarks . "book"))))
;; Icons
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
	:init
	(add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Projectile
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)
				 ("C->" . mc/mark-next-like-this)
				 ("C-<" . mc/mark-previous-like-this)
				 ("C-c C-<" . mc/mark-all-like-this)
				 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; YAS code snippets
(use-package yasnippet
  :init
	(add-to-list 'load-path "~/.emacs.d/snippets")
	:config
  (yas-global-mode 1))

;; Company-mode
(use-package company
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
	(global-company-mode +1))

;; Persistent *scratch*
(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default))

;; Common Lisp
(use-package cl)

;; Sets background color to strings that match color names, e.g. #0000ff
(use-package rainbow-mode
  :hook prog-mode)

(use-package smex
  :init
  (defvar smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :config
  (smex-initialize)
	:bind (("M-x" . smex)
				 ("M-X" . smex-major-mode-commands)))

;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode))

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

(defun reset-session ()
  "Kill all buffers except *Messages* *dashboard* *scratch*."
  (interactive)
  (mapc 'kill-buffer
				(remove-if
				 (lambda (x)
					 (or
						(string-equal "*Messages*" (buffer-name x))
						(string-equal "*dashboard*" (buffer-name x))
						(string-equal "*scratch*" (buffer-name x))))
				 (buffer-list)))
  (delete-other-windows nil)
  (delete-other-frames nil))

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

;; Copy filename to clipboard
(defun copy-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
											default-directory
										(buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; file size
(defun file-size-human-readable (file-size &optional flavor)
  "Produce a string showing FILE-SIZE in human-readable form.

Optional second argument FLAVOR controls the units and the display format:

 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
									 1024.0
								 1000.0))
				(post-fixes
				 ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
				 (list "" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr post-fixes))
      (setq file-size (/ file-size power)
						post-fixes (cdr post-fixes)))
    (format "%.0f%s%s" file-size
						(if (and (eq flavor 'iec) (string= (car post-fixes) "k"))
								"K"
							(car post-fixes))
						(if (eq flavor 'iec) "iB" ""))))

(defun get-filesize ()
  "Prompt user to enter a file name, with completion and history support."
  (interactive)
  (let* ((data (file-attributes (read-file-name "Enter file name:")))
				 (d (nth 7 data)))
    (message "Size is %s" (file-size-human-readable d))))

(defun sort-words (reverse beg end)
	"Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse."
	(interactive "*P\nr")
	(sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;           KE YBINDINGS           ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Get to the next buffer with C-<TAB>
(global-set-key (kbd "<C-tab>") 'other-window)

(global-set-key (kbd "C-d") 'duplicate-line)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;;         PROGRAMMING          ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook
					(lambda ()
						(font-lock-add-keywords nil
																		'(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
																			 1 font-lock-warning-face t)))))

(defun setup-tide-mode ()
	"Set up a full typescript environment."
	(interactive)
	(tide-setup)
	(eldoc-mode +1)
	(tide-hl-identifier-mode +1)
	(company-mode +1))

;; Typescript/JavaScript
(use-package tide
  :hook ((typescript-mode . tide-setup)
				 (js-mode . tide-setup)
         (before-save . tide-format-before-save)))

;; Golang
(use-package go-mode
	:config
	(use-package company-go)
	(use-package go-eldoc)
	(use-package go-guru)

	(defun setup-go-mode ()
		"Define function to call on go-mode."
		(add-hook 'before-save-hook 'gofmt-before-save)
		(defvar gofmt-command "goimports")
		(if (not (string-match "go" compile-command))
				(set (make-local-variable 'compile-command)
						 "go build -v && go vet -v"))

		(go-guru-hl-identifier-mode)

		(local-set-key (kbd "M-.") 'godef-jump)
		(local-set-key (kbd "M-,") 'pop-tag-mark)
		(local-set-key (kbd "M-p") 'compile)
		(local-set-key (kbd "M-P") 'recompile)
		(local-set-key (kbd "M-[") 'previous-error)
		(local-set-key (kbd "M-]") 'next-error)

		(set (make-local-variable 'company-backends) '(company-go))

		(go-eldoc-setup))

	:hook ((go-mode . setup-go-mode)
         (before-save . gofmt-before-save)))

(use-package company-web
  :hook
	(web-mode . (lambda ()
								(set (make-local-variable 'company-backends) '(company-web-html)))))

;; Web-mode (PHP/HTML/CSS)
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-indentation t))

;; JSON
(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
	;; (add-hook 'json-mode-hook
	;; 					(lambda ()
	;; 						(add-hook 'before-save-hook 'json-pretty-print-buffer nil t)))
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2))

;; YAML
(use-package yaml-mode
  :mode (".yml" ".yaml"))

;; Markdown
(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . markdown-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:config
	(local-set-key (kbd "M-p") 'markdown-preview-mode)
	(defvar markdown-preview-stylesheets
		(list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
					"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css")))

;; LateX
(use-package latex-preview-pane
	:init
	(add-hook 'doc-view-mode-hook 'auto-revert-mode)
	:config
	(latex-preview-pane-enable))

;; Python
(use-package python
  :mode
  ("\\.py" . python-mode)
	:config
	(setq indent-tabs-mode nil)
	(defvar python-indent-guess-indent-offset nil)
	(defvar python-indent-offset 2)

	(use-package elpy
		:init
		(advice-add 'python-mode :before 'elpy-enable)
		:mode
		("\\.py$" . python-mode)
		:config
		(defvar elpy-rpc-backend "jedi")
		(add-hook 'elpy-mode-hook
							(lambda ()
								(add-hook 'before-save-hook 'elpy-format-code nil t)))

		(eval-after-load "elpy"
			'(cl-dolist (key '("C-<up>" "C-<down>" "C-<left>" "C-<right>" "M-<up>" "M-<down>" "M-<left>" "M-<right>"))
				 (define-key elpy-mode-map (kbd key) nil)))

		:bind
		(:map elpy-mode-map
					("M-." . elpy-goto-definition)
					("M-," . pop-tag-mark)))

	(setq auto-mode-alist
				(append '(("SConstruct\\'" . python-mode)
									("SConscript\\'" . python-mode))
								auto-mode-alist))

	(use-package pip-requirements
		:hook
		(pip-requirements-mode-hook . pip-requirements-auto-complete-setup))

	(use-package py-autopep8)

	(use-package pyvenv))

;; Org
(use-package org
  :config
	(setq org-hide-emphasis-markers t)
	(font-lock-add-keywords 'org-mode
													'(("^ *\\([-]\\) "
														 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

	(let* ((variable-tuple
					(cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
								((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
								((x-list-fonts "Verdana")         '(:font "Verdana"))
								((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
								(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
				 (base-font-color     (face-foreground 'default nil 'default))
				 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

		(custom-theme-set-faces
		 'user
		 `(org-level-8 ((t (,@headline ,@variable-tuple))))
		 `(org-level-7 ((t (,@headline ,@variable-tuple))))
		 `(org-level-6 ((t (,@headline ,@variable-tuple))))
		 `(org-level-5 ((t (,@headline ,@variable-tuple))))
		 `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
		 `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
		 `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
		 `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
		 `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

	(add-hook 'org-mode-hook 'visual-line-mode))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#cbced0" :family "Sans Serif")))))

;;; emacs.el ends here
