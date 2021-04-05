;;; emacs.el --- My personnal Emacs configuration

;;; Commentary:
;;
;; * Setup
;;
;; * Symlinks
;;
;;   $ ln -s /path/to/playground/emacs/emacs.el ~/.emacs
;;
;; * Daemon
;;
;;   $ mkdir -p ~/.config/systemd/user
;;   $ cp /path/to/playground/emacs/emacs.service ~/.config/systemd/user
;;   $ systemct --user start emacs.service
;;
;;   Text mode: emacsclient --create-frame --quiet -nw
;;   GUI mode: emacsclient --create-frame --quiet
;;
;; * all-the-icons-install
;;
;;   - M-x all-the-icons-install-fonts
;;
;;; TODO:
;;
;; LSP: JS/TSX/JSX, Golang, HTML, PHP, LateX, Python, Org
;;
;; transformer en fichier ORG avec des insert src elisp
;;
;; tester https://github.com/felipeochoa/rjsx-mode
;;
;; compile: aller correctement à l'erreur s'il y'en a
;;
;; Tide (tsserver): améliorer temps de chargement d'un gros projet
;;
;; ELisp format on save
;;
;; Color picker
;;
;; livterm / Shell / EShell / Tern / ZSH / ...
;;
;; Magit: pull --rebase / push / status / commit / diff / log
;;
;; Tester:
;;   electrum: https://github.com/raxod502/selectrum
;;   DAP https://github.com/emacs-lsp/dap-mode
;;
;; ElDoc
;; Prisme.el
;; DAP
;;
;; flycheck-checker-error-threshold
;;
;; Tide: que le serveur démarre dès qu'un fichier TS est ouvert
;;
;; use-package:
;;   :type github
;;   :load-path
;;
;; Désactiver pleins de trucs si on est en mode text
;;
;; Conserver la text scale après un revert-buffer/revert-all-buffers F5
;;
;; all-this-icons: https://github.com/domtronn/all-the-icons.el/issues/120
;;
;; Popper.el

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;;         GENERAL         ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Memory
(setq gc-cons-threshold (* 100 1024 1024)
      large-file-warning-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Answer y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Packages manager
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Common Lisp
(use-package cl-lib
	:ensure t
	:diminish)

;; Encoding
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "fr_FR.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; Default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Disable auto-save and auto-backup
(setq auto-save-default nil
      make-backup-files nil)

;; Automatically revert all buffers
(use-package autorevert
  :ensure nil
  :diminish
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interva 5)
  :init
  (global-auto-revert-mode))

;; Disable bell
(setq ring-bell-function 'ignore
      visible-bell nil)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode))

(setq scroll-error-top-bottom t)
(setq track-eol t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Ask before close
(setq confirm-kill-emacs 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;         INTERFACE         ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Window bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight
(global-hl-line-mode t)
(defvar global-hl-line-sticky-flag t)

;; Enable line numbers for some modes
(use-package display-line-numbers
  :ensure nil
  :hook (((text-mode prog-mode conf-mode) . display-line-numbers-mode)
				 (display-line-numbers-mode . column-number-mode)))

;; line:column in modeline
(line-number-mode 1)

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'line-number nil :height 97)
(set-face-attribute 'line-number-current-line nil :height 97)

;; Display the size of the buffer
(size-indication-mode t)

;; Fringe size (border size)
(defvar set-fringe-mode 0)

;; Maximize windo
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 115
                    :weight 'normal
                    :width 'normal)

;; Set frame title to opened buffer name
(setq frame-title-format
      (list (format "%%S %%j ")
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Themes
(use-package doom-themes
  :ensure t
  :diminish
  :config
  (load-theme 'doom-challenger-deep t)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding nil)
  (inhibit-compacting-font-caches t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15))

(use-package minions
  :ensure t
  :diminish
  :init
  (minions-mode)
  :custom
  (minions-mode-line-lighter ""))

(use-package diminish
  :ensure t
  :diminish)

;; Dashboard
(use-package dashboard
  :ensure t
  :diminish
  :config
  (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-startup-banner 'logo)
  (dashboard-set-footer nil)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (dashboard-page-separator "\n\n")
  (dashboard-items '((projects . 10)
										 (bookmarks . 5)
										 (recents . 20)))
	:bind ("C-S-d" . (lambda ()
										 (interactive)
										 (switch-to-buffer "*dashboard*"))))

;; Indentation hightlight
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "#2c203b")
  (set-face-background 'highlight-indent-guides-even-face "#2c203b")
  (set-face-foreground 'highlight-indent-guides-character-face "#2c203b")
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?\|)
  (highlight-indent-guides-auto-enabled nil))

;; Sets background color to strings that match color names, e.g. #0000ff
(use-package rainbow-mode
  :ensure t
  :diminish
  :hook
  (prog-mode))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
				 ("C-r" . ivy-previous-line-or-history)

				 ;; TODO
				 ;; ("TAB" . ivy-partial)
				 ;; ("tab" . ivy-partial)
				 )
  :init
  (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "【%d/%d】")
  (enable-recursive-minibuffers t)
  (ivy-dynamic-exhibit-delay-ms 250))

(use-package ivy-hydra
  :ensure t
  :diminish
	:requires	(ivy hydra))

(use-package ivy-rich
  :ensure t
  :diminish
	:requires	(ivy-mode)
  :init (ivy-rich-mode))

(use-package counsel-projectile
  :ensure t)

(use-package counsel
  :ensure t
  :diminish
  :commands (counsel-linux-app-format-function-name-only)
  :bind (("M-x" . counsel-M-x)
         ("M-b" . counsel-ibuffer))
  :custom (ivy-initial-inputs-alist nil))

(use-package swiper
  :ensure t
  :diminish
	:requires	(ivy))

;; Improves sorting for fuzzy-matched results
(use-package flx
  :ensure t
  :diminish
	:requires	(ivy)
  :custom (ivy-flx-limit 10000))

(use-package prescient
  :ensure t
  :diminish
	:requires	(ivy))

(use-package ivy-prescient
  :ensure t
  :diminish
  :custom (ivy-prescient-sort-commands :sort)
	:requires	(ivy))

(use-package company-prescient
  :ensure t
  :diminish)

(use-package smex
  :ensure t
  :diminish
	:requires	(ivy))

(use-package which-key
  :ensure t
  :diminish
  :init (which-key-mode)
  :custom (which-key-idle-delay 0.3))

;; Colors
(use-package ansi-color
  :ensure t
  :commands (ansi-color-apply-on-region))

;; Get to the next window
(global-set-key (kbd "<C-tab>") 'other-window)
;; Get to the previsou window with C-<TAB>
(define-key global-map (kbd "<C-iso-lefttab>") (lambda ()
																								 (interactive)
																								 (other-window -1)))

;; Move to directionnal window
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)

;; Create a new frame
(global-unset-key (kbd "C-S-n"))
(global-set-key (kbd "C-S-n") 'make-frame-command)

;; Jump to bookmark
(global-set-key (kbd "C-S-b") 'bookmark-jump)

;; Split windows
(global-set-key (kbd "C-S-e") 'split-window-horizontally)
(global-set-key (kbd "C-S-o") 'split-window-vertically)

(global-set-key (kbd "C-S-w") (lambda ()
																(interactive)
																(kill-buffer (current-buffer))))

;; Text sclae increase/decrease
(defvar text-scale-mode-step 1.1)

(global-set-key (kbd "<C-x =>") 'text-scale-increase)
(global-unset-key (kbd "<C-mouse-4>"))
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-unset-key (kbd "<C-mouse-5>"))
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;          EDITION          ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace selected region
(delete-selection-mode 1)

;; Scroll
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))	;; One line at a time
      mouse-wheel-progressive-speed nil							;; Don't accelerate scrolling
      mouse-wheel-follow-mouse t										;; Scroll window under mouse
      scroll-step 1)																;; Keyboard scroll one line at a time

(fast-scroll-config)
(fast-scroll-mode 1)

;; Trailing whitespaces
;; (setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; Tabs
(defvar custom-tab-width 2)
(add-hook 'prog-mode-hook (lambda ()
														(setq indent-tabs-mode t
																	tab-width custom-tab-width)))

;; Making electric-indent behave sanely
(electric-indent-mode +1)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char))
  :config (smart-hungry-delete-add-default-hooks))

;; Show pairing parenthesis and brackets
(show-paren-mode t)

;; Undo/Redo
(use-package undo-fu
  :ensure t
  :diminish
	:custom
	(undo-fu-allow-undo-in-region t)
	:bind (("C-z" . 'undo-fu-only-undo)
				 ("C-S-z" . 'undo-fu-only-redo)))

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :diminish
  :bind (("C-c m c" . mc/edit-lines)
				 ("C->" . mc/mark-next-like-this)
				 ("C-<" . mc/mark-previous-like-this)
				 ("C-c C-<" . mc/mark-all-like-this)
				 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

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

(defun duplicate-line()
  "Duplicate the cursor's current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-k") (lambda ()
			      (interactive)
			      (kill-line 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;;          PROGRAMMING         ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Company-mode
(use-package company
  :ensure t
  :diminish
  :bind (("C-c <tab>" . company-complete))
  :config (global-company-mode 1)
  :custom
	(company-idle-delay 0)
	(company-echo-delay 0)
	(company-tooltip-align-annotations t)
	(company-minimum-prefix-length 2)
	(company-selection-wrap-around t)
	(company-transformers '(company-sort-by-occurrence
													company-sort-by-backend-importance))
	(company-dabbrev-downcase nil)
	(company-dabbrev-code-everywhere t)
	(company-dabbrev-code-modes t)
	(company-dabbrev-code-ignore-case t)
	(completion-ignore-case t))

(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :preface
  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  :custom
	(company-box-backends-colors nil)
	(company-box-show-single-candidate t)
  :config
  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))

;; Show quick tooltip
(use-package company-quickhelp
  :defines company-quickhelp-delay
  :hook (global-company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 1))

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish
  :init (global-flycheck-mode)
	:custom	(flycheck-check-syntax-automatically '(idle-change save)))

;; YAS code snippets
(use-package yasnippet
  :ensure t
  :diminish
  :commands (yas-global-mode yas-reload-all)
  :config
	(yas-global-mode 1)
	(yas-reload-all))

(defun colorize-compilation-buffer ()
  "Apply ANSI colors to *compilation* buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Compilation
(use-package compile
  :ensure t
  :custom
	(compilation-scroll-output t)
	(compilation-always-kill t))

;; Colorise some keywords
(add-hook 'prog-mode-hook
					(lambda ()
						(font-lock-add-keywords nil
																		'(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\|NOTE\\)"
																			 1 font-lock-warning-face t)))))

;; Version Control
;;
;; Disable VC
(setq vc-handled-backends nil)
;;
;; (use-package vc
;;   :ensure nil
;;   :diminish
;;   :custom
;; 	(auto-revert-check-vc-info t)
;; 	(vc-follow-symlinks nil)
;; 	(vc-refresh-state))

;; (use-package magit
;;   :ensure t
;;   :diminish t
;;   :commands (magit-status
;; 						 magit-get-current-branch
;; 						 magit-display-buffer-same-window-except-diff-v1)
;;   :bind (("C-c g s" . magit-status)
;; 				 ("C-c g l" . magit-log-all))
;;   :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Projectile
(use-package projectile
  :ensure t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-S-f" . projectile-find-file)
				 ("C-S-s" . projectile-grep)
				 ("C-S-P" . projectile-switch-project)
				 ("C-S-r" . projectile-run-project))
  :commands (projectile-register-project-type)
  :config
	(projectile-register-project-type 'npm '("package.json")
																		:project-file "package.json"
																		:test "npm test"
																		:run "npm start"
																		:compilation-dir "packages/api"
																		:compile "npm run build")

	(setq projectile-globally-ignored-directories '("log"
																									"logs"
																									"node_modules"
																									"dist")))

(use-package prettier-js
  :ensure t
  :diminish
  :custom (prettier-js-show-errors nil))

(use-package add-node-modules-path
  :ensure t
	:defer t)

(setq compilation-error-regexp-alist-alist
      (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                   1 ;; file
                   2 ;; line
                   3 ;; column
                   )
            compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'node compilation-error-regexp-alist))

;; Typescript
(use-package tide
  :ensure t
  :diminish
	:custom ((typescript-indent-level 2)
					 (tide-completion-ignore-case 1)
					 (tide-server-max-response-length 1048576)
					 (tide-hl-identifier-idle-time 0.1))
	:hook ((typescript-mode . tide-setup)
				 (typescript-mode . tide-hl-identifier-mode)
				 (typescript-mode . company-mode)
				 (before-save . tide-format-before-save))
	:commands (tide-rename-symbol tide-rename-file tide-references prettier-js)
	:bind (("C-c C-t r s" . tide-rename-symbol)
				 ("C-c C-t r f" . tide-rename-file)
				 ("C-c C-t f r" . tide-references)
				 ("C-c C-t i j" . tide-jsdoc-template)
				 ("C-c C-t e" . tide-project-errors)
				 ("C-c C-p" . prettier-js)))

(use-package npm-mode
  :ensure t
  :diminish
  :defer t
  :hook ((typescript-mode . npm-mode)
         (javascript-mode . npm-mode)))

;; JSON
(use-package json-mode
  :ensure t
  :hook (json-mode . prettier-js-mode)
  :custom
	(make-local-variable 'js-indent-level)
	(js-indent-level 2))

;; YAML
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml")
  :hook ((yaml-mode . prettier-js-mode)
				 (yaml-mode . highlight-indent-guides-mode)))

;; CSV
(use-package csv-mode
  :ensure t
  :custom (csv-separators '("," ";" "|" "\t")))

(use-package org
  :ensure nil
  :config (define-key org-mode-map (kbd "<C-tab>") 'other-window)
  :hook ((org-mode . org-indent-mode)
				 (org-mode . visual-line-mode))
  :custom (org-hide-emphasis-markers t))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
	(org-superstar-remove-leading-stars t)
	(org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile"))

(use-package docker-compose-mode
	:ensure t
	:mode ("docker-compose\\*.yaml")
	:hook ((docker-compose-mode . company-mode)
				 (docker-compose-mode . yaml-mode)))

(use-package systemd
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;;          UTILITIES          ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-package-update
(use-package auto-package-update
  :ensure t
  :diminish
  :commands (auto-package-update-maybe)
  :config
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
						(lambda () (message "I will update packages now")))
  :custom
	(auto-package-update-delete-old-versions t)
	(auto-package-update-hide-results t)
	(auto-package-update-interval 30))

;; Save mini buffer history
(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :custom
	(savehist-additional-variables '(kill-ring
																	 search-ring
																	 regexp-search-ring
																	 npm-mode-npm-run
																	 package-install
																	 package-delete)))

;; Environment variables
(use-package exec-path-from-shell
  :ensure t
  :diminish
  :config (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-check-startup-files nil))

;; Emacs Start Up Profiler
(use-package esup
  :ensure t
  :diminish
  :defer t
  :commands (esup)
  :custom (message-log-max t))

;; Init fil debugger
(use-package bug-hunter
  :ensure t
  :diminish
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :diminish
	:requires	(all-the-icons))

;; Icons: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package recentf
  :ensure t
  :diminish
  :custom
	(recentf-max-menu-items 100)
	(recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'"))
	:config (recentf-mode 1))

;; Dired-subtree
(use-package dired-subtree
  :ensure t
  :custom (dired-listing-switches "-laGh1v --group-directories-first")
  :bind ("<tab>" . dired-subtree-toggle)
  :hook (dired-mode . auto-revert-mode))

(use-package restclient
  :ensure t
	:defer t)

(use-package dimmer
  :ensure t
  :diminish
  :config (dimmer-mode)
  :custom
	(dimmer-adjustment-mode :foreground)
	(dimmer-fraction 0.1))

(use-package writeroom-mode
  :ensure t
  :diminish
	:defer t
  :commands (writeroom-adjust-width)
  :preface
  (defun writeroom-toggle-on  ()
    (writeroom-adjust-width 40) ;; Width = 80 + 40
    (display-line-numbers-mode -1)
    (highlight-indent-guides-mode -1)
    (text-scale-increase 1)
    (text-scale-increase 1))
  (defun writeroom-toggle-off  ()
    (display-line-numbers-mode 1)
    (highlight-indent-guides-mode 1)
    (text-scale-decrease 1)
    (text-scale-decrease 1))
  :bind ("<f6>" . writeroom-mode)
  :custom (writeroom-restore-window-config 1)
  :config
  (add-hook 'writeroom-mode-enable-hook 'writeroom-toggle-on)
  (add-hook 'writeroom-mode-disable-hook 'writeroom-toggle-off))

(use-package uuidgen
  :ensure t
	:defer t)

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

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "S-<f5>") 'revert-all-buffers)

(defun reset-session ()
	"Kill all buffers except *Messages* *scratch*."
	(interactive)
	(mapc 'kill-buffer
				(remove-if
				 (lambda (x)
					 (or
						(string-equal "*Messages*" (buffer-name x))
						(string-equal "*scratch*" (buffer-name x))))
				 (buffer-list)))
	(delete-other-windows nil)
	(delete-other-frames nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Golang
;; (use-package go-mode
;;     :config
;;     (use-package company-go)
;;     (use-package go-eldoc)
;;     (use-package go-guru)

;;     (defun setup-go-mode ()
;;             "Define function to call on go-mode."
;;             (defvar gofmt-command "goimports")
;;             (if (not (string-match "go" compile-command))
;;                             (set (make-local-variable 'compile-command)
;;                                              "go build -v && go vet -v"))

;;             (go-guru-hl-identifier-mode)

;;             (local-set-key (kbd "M-.") 'godef-jump)
;;             (local-set-key (kbd "M-,") 'pop-tag-mark)
;;             (local-set-key (kbd "M-p") 'compile)
;;             (local-set-key (kbd "M-P") 'recompile)
;;             (local-set-key (kbd "M-[") 'previous-error)
;;             (local-set-key (kbd "M-]") 'next-error)

;;             (set (make-local-variable 'company-backends) '(company-go))

;;             (go-eldoc-setup))

;;     :hook ((go-mode . setup-go-mode)
;;          (before-save . gofmt-before-save)))

;; HTML
;; (use-package sgml-mode
;;   :config
;;   (setq sgml-basic-offset 2)
;;     (setq sgml-quick-keys 'close)
;;   (add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode))

;; PHP
;; (use-package php-mode
;;     :config
;;     (defun setup-php-mode ()
;;             (use-package company-php)
;;             (ac-php-core-eldoc-setup)

;;             (set (make-local-variable 'company-backends)
;;                              '((company-ac-php-backend company-dabbrev-code)
;;                                      company-capf company-files))

;;             (define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
;;             (define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back))

;;     :hook (php-mode . setup-php-mode))

;; Markdown
;; (use-package markdown-mode
;;   :init
;;   (setq markdown-command "multimarkdown")
;;   :mode (("README\\.md\\'" . markdown-mode)
;;                              ("\\.md\\'" . markdown-mode)
;;                              ("\\.markdown\\'" . markdown-mode))
;;     :config
;;     (local-set-key (kbd "M-p") 'markdown-preview-mode)
;;     (defvar markdown-preview-stylesheets
;;             (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
;;                                     "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css")))

;; LateX
;; (use-package latex-preview-pane
;;     :init
;;     (add-hook 'doc-view-mode-hook 'auto-revert-mode)
;;     :config
;;     (latex-preview-pane-enable))

;; Python
;; (use-package python
;;   :mode
;;   ("\\.py" . python-mode)
;;     :config
;;     (setq indent-tabs-mode nil)
;;     (defvar python-indent-guess-indent-offset nil)
;;     (defvar python-indent-offset 2)

;;     (use-package elpy
;;             :init
;;             (advice-add 'python-mode :before 'elpy-enable)
;;             :mode
;;             ("\\.py$" . python-mode)
;;             :config
;;             (defvar elpy-rpc-backend "jedi")
;;             (add-hook 'elpy-mode-hook
;;                                                     (lambda ()
;;                                                             (add-hook 'before-save-hook 'elpy-format-code nil t)))

;;             (eval-after-load "elpy"
;;                     '(cl-dolist (key '("C-<up>" "C-<down>" "C-<left>" "C-<right>" "M-<up>" "M-<down>" "M-<left>" "M-<right>"))
;;                              (define-key elpy-mode-map (kbd key) nil)))

;;             :bind
;;             (:map elpy-mode-map
;;                                     ("M-." . elpy-goto-definition)
;;                                     ("M-," . pop-tag-mark)))

;;     (setq auto-mode-alist
;;                             (append '(("SConstruct\\'" . python-mode)
;;                                                                     ("SConscript\\'" . python-mode))
;;                                                             auto-mode-alist))

;;     (use-package pip-requirements
;;             :hook
;;             (pip-requirements-mode-hook . pip-requirements-auto-complete-setup))

;;     (use-package py-autopep8)

;;     (use-package pyvenv))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#dddddd"])
 '(custom-safe-themes
	 '("a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" default))
 '(fci-rule-color "#505050")
 '(jdee-db-active-breakpoint-face-colors (cons "#1b1d1e" "#fc20bb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1b1d1e" "#60aa00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1b1d1e" "#505050"))
 '(objed-cursor-color "#d02b61")
 '(package-selected-packages
	 '(fast-scroll nlinum ob-mongo cl-libify org-superstar esup uuidgen writeroom-mode dimmer restclient dired-subtree all-the-icons-dired exec-path-from-shell auto-package-update docker-compose-mode dockerfile-mode csv-mode yaml-mode json-mode npm-mode tide add-node-modules-path prettier-js magit yasnippet flycheck company-box multiple-cursors aggressive-indent rainbow-delimiters undo-tree smart-hungry-delete which-key smex company-prescient ivy-prescient prescient flx counsel-projectile ivy-rich ivy-hydra ivy rainbow-mode highlight-indent-guides dashboard diminish minions doom-modeline doom-themes use-package))
 '(pdf-view-midnight-colors (cons "#dddddd" "#1b1d1e"))
 '(rustic-ansi-faces
	 ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#dddddd"])
 '(vc-annotate-background "#1b1d1e")
 '(vc-annotate-color-map
	 (list
		(cons 20 "#60aa00")
		(cons 40 "#859f0d")
		(cons 60 "#aa931a")
		(cons 80 "#d08928")
		(cons 100 "#d38732")
		(cons 120 "#d6863d")
		(cons 140 "#da8548")
		(cons 160 "#ce8379")
		(cons 180 "#c281aa")
		(cons 200 "#b77fdb")
		(cons 220 "#bf63b2")
		(cons 240 "#c74789")
		(cons 260 "#d02b61")
		(cons 280 "#b0345c")
		(cons 300 "#903d58")
		(cons 320 "#704654")
		(cons 340 "#505050")
		(cons 360 "#505050")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family "Iosevka"))))
 '(dashboard-items-face ((t nil)))
 '(doom-modeline-bar ((t (:background "black"))))
 '(hl-line ((t (:extend t :background "#24213b"))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "magenta" :weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background "#292F37" :foreground "magenta" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :foreground "magenta" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :foreground "magenta" :weight bold))))
 '(org-document-title ((t :height 2.0)))
 '(org-level-1 ((t :inherit outline-1 :weight extra-bold :height 1.5)))
 '(org-level-2 ((t :inherit outline-2 :weight bold :height 1.3)))
 '(org-level-3 ((t :inherit outline-3 :weight bold :height 1.1)))
 '(org-level-4 ((t :inherit outline-4 :weight bold :height 1.0)))
 '(org-level-5 ((t :inherit outline-5 :weight semi-bold :height 1.0)))
 '(org-level-6 ((t :inherit outline-6 :weight semi-bold :height 1.0)))
 '(org-level-7 ((t :inherit outline-7 :weight semi-bold)))
 '(org-level-8 ((t :inherit outline-8 :weight semi-bold)))
 '(tide-hl-identifier-face ((t (:background "gray11" :underline t :weight bold)))))

(provide 'emacs)
;;; emacs ends here
