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
;;
;; * Setup
;;
;; - M-x all-the-icons-install-fonts
;;
;;; TODO:
;;
;; Code documentation
;;
;; LSP => tester emacs27
;;
;; checker tout de qui est enable en global (voir si c'est nécessaire ?)
;;
;; use-package : ensure-system-package
;;   :ensure-system-package (tern . "npm install -g tern")
;;
;; C-c C-c compile pour tous les modes

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
(use-package exec-path-from-shell
  :init
	(defvar exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; Disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Automatically revert all buffers
(global-auto-revert-mode t)

;; Disable bell
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

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
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep t)
  (doom-themes-org-config))

;; Window bars
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight
(global-hl-line-mode +1)
(defvar global-hl-line-sticky-flag t)

;; Line number
(add-hook 'prog-mode-hook '(lambda () (linum-on)))
(column-number-mode t)

;; Fringe size (border size)+
(fringe-mode '(1 . 1))

;; Maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Set frame title to opened buffer name
(setq frame-title-format
			(list (format "%%S %%j ")
						'(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Scroll
(use-package smooth-scrolling
  :init
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 1))

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
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
	(setq highlight-indent-guides-auto-enabled nil)

	;; (set-face-background 'highlight-indent-guides-odd-face "dimgray")
	(set-face-background 'highlight-indent-guides-odd-face "Gray22")
	(set-face-background 'highlight-indent-guides-even-face "Gray22")
	(set-face-foreground 'highlight-indent-guides-character-face "Gray22")

	:hook (prog-mode . highlight-indent-guides-mode))

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
 '(custom-enabled-themes '(doom-challenger-deep))
 '(custom-safe-themes
	 '("3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" base16-horizon-terminal-dark))
 '(fci-rule-color "#4E4E4E")
 '(jdee-db-active-breakpoint-face-colors (cons "#D0D0E3" "#009B7C"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#D0D0E3" "#005F00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#D0D0E3" "#4E4E4E"))
 '(nil nil t)
 '(objed-cursor-color "#D70000")
 '(package-selected-packages
	 '(cl dired-subtree smooth-scrolling prettier-js helm-projectile helm dockerfile-mode tickscript-mode base16-theme go-mode tide flycheck rainbow-mode persistent-scratch yasnippet multiple-cursors dashboard doom-themes all-the-icons-dired ac-php php-mode company-quickhelp go-guru company-go latex-preview-pane markdown-mode yaml-mode json-mode company-css company-web-html company-web go-eldoc bug-hunter org-bullets py-autopep8 pip-requirements elpy auto-autopep8 base16-bug bullets-company company company-cursors dashboard-eldoc elpy-elpygen-esup exec-exec from from-go go-go go guru-hunter jedi jedi-markdown minions-mode mode-mode mode-mode mode mode-multiple org-package pane-path-path-persistent pip-preview-preview projectile py-pyenv-rainbow-requirements scratch shell shell smex themelatex-tide typescript update-web-yaml-yasnippet))
 '(pdf-view-midnight-colors (cons "#0F1019" "#F5F5F9"))
 '(rustic-ansi-faces
	 ["#F5F5F9" "#D70000" "#005F00" "#AF8700" "#1F55A0" "#AF005F" "#007687" "#0F1019"])
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-annotate-background "#F5F5F9")
 '(vc-annotate-color-map
	 (list
		(cons 20 "#005F00")
		(cons 40 "#3a6c00")
		(cons 60 "#747900")
		(cons 80 "#AF8700")
		(cons 100 "#bc7900")
		(cons 120 "#c96c00")
		(cons 140 "#D75F00")
		(cons 160 "#c93f1f")
		(cons 180 "#bc1f3f")
		(cons 200 "#AF005F")
		(cons 220 "#bc003f")
		(cons 240 "#c9001f")
		(cons 260 "#D70000")
		(cons 280 "#b41313")
		(cons 300 "#922727")
		(cons 320 "#703a3a")
		(cons 340 "#4E4E4E")
		(cons 360 "#4E4E4E")))
 '(vc-annotate-very-old-color nil))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-footer nil)
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
  (setq dashboard-items '((projects		 . 5)
													(bookmarks	 . 5)
													(recents		 . 30))))

(use-package all-the-icons-dired)

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

;; Company Quickhelp
(use-package company-quickhelp
	:config
	(company-quickhelp-mode)
	(setq company-quickhelp-color-background "LightSteelBlue1")
	(setq company-quickhelp-color-foreground "dark slate gray"))

;; Persistent *scratch*
(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default)
	:config
	(setq initial-scratch-message "")
	(local-set-key (kbd "C-x C-s") 'persistent-scratch-save))

;; Common Lisp
(use-package cl)

;; Sets background color to strings that match color names, e.g. #0000ff
(use-package rainbow-mode
  :hook prog-mode)

;; Helm
(use-package helm
	:config
	(set-face-attribute 'helm-selection nil
											:background "purple"
											:foreground "black")

	(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

	(add-to-list 'display-buffer-alist
							 `(,(rx bos "*helm" (* not-newline) "*" eos)
								 (display-buffer-in-side-window)
								 (inhibit-same-window . t)
								 (window-height . 0.4)))

	(use-package helm-projectile)

	:bind (("M-x" . helm-M-x)
				 ("M-b" . helm-buffers-list)
				 ("C-x C-f" . helm-find-files)
				 ("C-o" . helm-find-files)
				 ("C-x p f" . helm-projectile)
				 ("C-x p a" . helm-projectile-ack)))

;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Dired-subtree
(use-package dired-subtree
  :config
  (setq dired-listing-switches "-laGh1v --group-directories-first")

  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)
	           ("<tab>" . dired-subtree-toggle)))

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

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

(defun revert-all-buffers ()
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
	"REVERSE Sort words in region alphabetically from BEG to END."
	(interactive "*P\nr")
	(sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
				 (filename (buffer-file-name))
				 (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;           KE YBINDINGS           ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-;") 'comment-line)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "S-<f5>") 'revert-all-buffers)

;; Get to the next buffer with C-<TAB>
(global-set-key (kbd "<C-tab>") 'other-window)

(global-set-key (kbd "C-d") 'duplicate-line)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Split windows
(global-set-key (kbd "C-S-e") 'split-window-horizontally)
(global-set-key (kbd "C-S-o") 'split-window-vertically)

;; Text sclae increase/decrease
(global-unset-key (kbd "<C-mouse-4>"))
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-unset-key (kbd "<C-mouse-5>"))
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

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

(use-package prettier-js
	:config
	(defvar prettier-js-args '(
														 "--print-width" "100"
														 "--tab-width" "2"
														 "--use-tabs" "false"
														 "--no-semi" "false"
														 "--single-quote" "true"
														 "--quote-props" "as-needed"
														 "--trailing-comma" "all"
														 "--bracket-spacing" "true"
														 "--arrow-parens" "avoid")))

;; Compilation
(use-package ansi-color)

(defun colorize-compilation-buffer ()
	"Apply ANSI colors to *compilation* buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package compile
	:config
	(setq compilation-scroll-output t)
	(setq	compilation-always-kill t))

;; Typescript
(use-package tide
	:config
	(defun setup-tide-mode ()
		(interactive)
		(tide-setup)
		(eldoc-mode +1)
		(tide-hl-identifier-mode +1))

	:bind (:map tide-mode-map
							("C-c C-c" . (lambda ()
														 (interactive)
														 (compile "cd ~/info/rest-console && npm run build")))

							("C-c C-d" . (lambda ()
														 (interactive)
														 (compile "cd ~/info/rest-console && npm run dev -- -- --config ./config/local.yml")))

							("C-c C-t" . (lambda ()
														 (interactive)
														 (compile "cd ~/info/rest-console && npm run test:cmd -- --config ./config/test.yml")))

							("C-c C-l" . (lambda ()
														 (interactive)
														 (compile "cd ~/info/rest-console && npm run lint")))

							("C-c C-p" . prettier-js))

	:hook ((typescript-mode . setup-tide-mode)
         (before-save . tide-format-before-save)))

;; Golang
(use-package go-mode
	:config
	(use-package company-go)
	(use-package go-eldoc)
	(use-package go-guru)

	(defun setup-go-mode ()
		"Define function to call on go-mode."
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

;; HTML
(use-package sgml-mode
  :config
  (setq sgml-basic-offset 2)
	(setq sgml-quick-keys 'close)
  (add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode))

;; PHP
(use-package php-mode
	:config
	(defun setup-php-mode ()
		(use-package company-php)
		(ac-php-core-eldoc-setup)

		(set (make-local-variable 'company-backends)
				 '((company-ac-php-backend company-dabbrev-code)
					 company-capf company-files))

		(define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
		(define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back))

	:hook (php-mode . setup-php-mode))

;; JSON
(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'")

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
	(defun org-mode-setup ()
		(setq org-hide-emphasis-markers t)
		(font-lock-add-keywords 'org-mode
														'(("^ *\\([-]\\) "
															 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

		(let* ((variable-tuple
						(cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
									(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
					 (base-font-color     (face-foreground 'default nil 'default))
					 (headline           `(:inherit default :foreground, base-font-color)))

			(custom-set-faces
			 'user
			 `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.3))))
			 `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.3))))
			 `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.3))))
			 `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.3))))
			 `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.3))))
			 `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.4))))
			 `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.4))))
			 `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.4))))
			 `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

		(visual-line-mode))

	(local-unset-key (kbd "<C-tab>"))

	(use-package org-bullets
		:config
		(add-hook 'org-mode-hook 'org-bullets-mode))

	:hook	(after-make-frame-functions . 'org-mode-setup))

;; *scratch* file with org-mode
(defun my-scratch-hook ()
	"Start 'org-mode' for *scratch* file."
	(with-current-buffer "*scratch*" (org-mode)))
(add-hook 'after-init-hook 'my-scratch-hook)

(use-package tickscript-mode)

;;; emacs.el ends here
