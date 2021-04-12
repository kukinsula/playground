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
;; Flycheck ESLint
;;
;; LSP: JS/TSX/JSX, Golang, HTML, PHP, LateX, Python, Org
;;
;; transformer en fichier ORG avec des insert src elisp
;;
;; tester https://github.com/felipeochoa/rjsx-mode
;;
;; Tide (tsserver): améliorer temps de chargement d'un gros projet
;;
;; ELisp format on save
;;
;; vterm
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
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			                   ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; use-package
(setq-default use-package-expand-minimally t
              use-package-verbose nil
              use-package-enable-imenu-support t
              use-package-compute-statistics t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Common Lisp
(with-no-warnings
  (require 'cl))

;; Encoding
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)
;; (set-locale-environment "fr_FR.UTF-8")
;; (set-locale-environment "en_EN.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      save-buffer-coding-system 'utf-8-unix
      process-coding-system-alist (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; Default browser
;; (setq browse-url-browser-function 'google-chrome-stable)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

;; Automatically revert all buffers
(use-package autorevert
  :ensure nil
  :diminish
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interva 5)
  :config
  (global-auto-revert-mode))

;; Disable bell
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Startup messages.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

(setq scroll-error-top-bottom t)
(setq track-eol t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Ask before close
(setq confirm-kill-emacs 'y-or-n-p)

;; Auto save files
(setq auto-save-default t
      auto-save-interval 300
      auto-save-timeout 30
      auto-save-list-file-prefix "~/.emacs.d/auto-saves/saves-"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(global-eldoc-mode t)

;; Helpful
(use-package helpful
  :ensure t
  :diminish
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(setq find-file-visit-truename t)

;; TODO
;;
;; (use-package flyspell
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook #'flyspell-prog-mode)
;;   (flyspell-mode)
;;   :custom
;;   (ispell-program-name "aspell")
;;   (ispell-list-command "--list"))

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; Resize windows
(global-set-key (kbd "<M-S-up>") 'shrink-window)
(global-set-key (kbd "<M-S-down>") 'enlarge-window)
(global-set-key (kbd "<M-S-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<M-S-right>") 'enlarge-window-horizontally)

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
(use-package hl-line
  :hook (prog-mode . hl-line-mode)
  :custom (global-hl-line-sticky-flag t)
  :custom-face
  (hl-line ((t (:extend t :background "#24213b")))))

;; Indentation hightlight
(use-package highlight-indent-guides
  :ensure t
  :disabled t
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "#474747")
  (set-face-background 'highlight-indent-guides-even-face "#474747")
  (set-face-foreground 'highlight-indent-guides-character-face "#474747")
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides--guide-region nil))

;; Sets background color to strings that match color names, e.g. #0000ff
(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode))

;; line:column in modeline
(line-number-mode 1)
(column-number-mode 1)
(setq size-indication-mode t)

;; Display the size of the buffer
(size-indication-mode t)

;; Fringe size (LEFT . RIGHT)
(set-fringe-mode '(2 . 2))

;; Maximize windo
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set font
(use-package font-lock
  :ensure nil
  :config
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 115
                      :weight 'normal
                      :width 'normal)
  :custom-face
  (font-lock-warning-face ((t (:inherit warning :foreground "sandy brown" :weight bold)))))

;; Set frame title to opened buffer name
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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
  :init (doom-modeline-mode)
  :config
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info buffer-position selection-info)
    '(misc-info minor-modes major-mode process checker "  "))
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding nil)
  (inhibit-compacting-font-caches t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-bar-width 5)
  (doom-modeline-height 25)
  :commands (doom-modeline-def-modeline)
  :custom-face
  '(doom-modeline-bar ((t (:background "#906CFF")))))

(use-package minions
  :ensure t
  :diminish
  :config (minions-mode)
  :custom (minions-mode-line-lighter ""))

(use-package diminish
  :ensure t
  :diminish)

;; Dashboard
(use-package dashboard
  :ensure t
  :diminish
  :config (dashboard-setup-startup-hook)
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
  (dashboard-set-init-info nil)
  (dashboard-banner-logo-title
   (format "Emacs ready in %.3f seconds."
           (float-time
            (time-subtract after-init-time before-init-time))))
  :bind ("C-S-d" . (lambda ()
		                 (interactive)
		                 (switch-to-buffer "*dashboard*")))
  :custom-face
  (dashboard-banner-logo-title ((t (:inherit default :foreground "slate gray" :slant italic :weight light))))
  (dashboard-items-face ((t nil))))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
	       ("M-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
	       ("C-r" . ivy-previous-line-or-history)
	       ("<tab>" . ivy-next-line)
	       ("<backtab>" . ivy-previous-line))
  :config (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "【%d/%d】")
  (enable-recursive-minibuffers nil)
  (ivy-dynamic-exhibit-delay-ms 250)
  :custom-face
  (ivy-current-match ((t (:foreground "#CBE3E7" :weight bold :background "#39374E"))))
  (ivy-minibuffer-match-face-1 ((t (:foreground "hot pink" :weight bold :background nil))))
  (ivy-minibuffer-match-face-2 ((t (:foreground "hot pink" :weight bold :background nil))))
  (ivy-minibuffer-match-face-3 ((t (:foreground "hot pink" :weight bold :background nil))))
  (ivy-minibuffer-match-face-4 ((t (:foreground "hot pink" :weight bold :background nil)))))

(use-package ivy-hydra
  :ensure t
  :diminish
  :after (ivy hydra))

(use-package counsel-projectile
  :ensure t)

(use-package all-the-icons-ivy-rich
  :ensure t
  :config (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :diminish
  :config (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :diminish
  :commands (counsel-linux-app-format-function-name-only)
  :bind (("M-x" . counsel-M-x)
	       ("C-x C-f" . counsel-find-file))
  :custom (ivy-initial-inputs-alist nil))

(define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
(define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)

(use-package swiper
  :ensure t
  :diminish
  :after (ivy)
  :custom-face
  (swiper-background-match-face-1 ((t (:foreground "hot pink" :weight bold :background nil))))
  (swiper-background-match-face-2 ((t (:foreground "hot pink" :weight bold :background nil))))
  (swiper-background-match-face-3 ((t (:background "hot pink" :weight bold :background nil))))
  (swiper-background-match-face-4 ((t (:foreground "hot pink" :weight bold :background nil))))
  (swiper-line-face ((t (:background "#39374E" :foreground "#CBE3E7" :weight bold))))
  (swiper-match-face-1 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
  (swiper-match-face-2 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
  (swiper-match-face-3 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
  (swiper-match-face-4 ((t (:background nil :foreground "#CBE3E7" :weight bold)))))

;; Improves sorting for fuzzy-matched results
(use-package flx
  :ensure t
  :diminish
  :after (ivy)
  :custom (ivy-flx-limit 10000))

(use-package prescient
  :ensure t
  :diminish
  :after (ivy))

(use-package ivy-prescient
  :ensure t
  :diminish
  :custom (ivy-prescient-sort-commands :sort)
  :after (ivy))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode)
  :custom (which-key-idle-delay 0.3))

;; Get to the next window
(global-set-key (kbd "<C-tab>") 'other-window)
;; Get to the previsou window with C-<TAB>
(define-key global-map (kbd "<C-iso-lefttab>") (lambda ()
						                                     (interactive)
						                                     (other-window -1)))

;; Move to directionnal window
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)

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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                  ;; scroll window under mouse
      scroll-step 1)                               ;; keyboard scroll one line at a time

(setq-default scroll-preserve-screen-position t)

;; Trailing whitespaces
;; (setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; Tabs
(defvar custom-tab-width 2)

(defun disable-tabs ()
  "Disable tabs."
  (setq indent-tabs-mode nil))

(defun enable-tabs  ()
  "Enable tabs."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

(add-hook 'prog-mode-hook 'enable-tabs)
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; backspace properly erase the tab.
(setq backward-delete-char-untabify-method 'hungry)

;; Visualize tabs as a pipe character - "|".
(defvar whitespace-style '(face tabs tab-mark trailing))

;; This will also show trailing characters as they are useful to spot.
(defvar whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9])))

(global-whitespace-mode)

;; Show pairing parenthesis and brackets
(show-paren-mode t)

(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; Undo/Redo
(use-package undo-fu
  :ensure t
  :diminish
  :custom
  (undo-fu-allow-undo-in-region t)
  :bind (("C-z" . 'undo-fu-only-undo)
	       ("C-S-z" . 'undo-fu-only-redo)))

(setq undo-limit 20000000
      undo-strong-limit 40000000)

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

(use-package move-text
  :ensure t
  :defer 0.5
  :config
  (move-text-default-bindings))

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

;; *scratch* file
(setq initial-scratch-message nil)

(add-to-list 'auto-mode-alist '("*scratch*" . text-mode))

(use-package persistent-scratch
  :ensure t
  :diminish
  :config
  (persistent-scratch-setup-default)
  (local-set-key (kbd "C-x C-s") 'persistent-scratch-save))

;; Hide/show blocks of code.
(use-package hs-minor-mode
  :ensure nil
  :diminish
  :hook (prog-mode . hs-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;;          PROGRAMMING         ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit
(use-package magit
  :ensure t
  :diminish
  :config
  (define-key magit-mode-map (kbd "C-TAB") nil)
  (define-key magit-mode-map (kbd "C-<tab>") nil))

;; Company-mode
(use-package company
  :ensure t
  :diminish
  :config
  (global-company-mode 1)

  ;; Trigger auto completion on tab or indent based on context
  (define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
  (define-key company-mode-map (kbd "<tab>") 'company-indent-or-complete-common)

  ;; Cycle through completions on hitting tab. No need to use arrows
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)

  ;; Select previous completion on shift + tab
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)

  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence
			                    company-sort-by-backend-importance))
  (completion-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-ignore-case t)
  (company-eclim-auto-save nil)
  :custom-face
  (company-tooltip ((t (:inherit tooltip :background nil :family "Source Code Pro")))))

(use-package company-prescient
  :ensure t
  :diminish)

(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode)
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
  (company-box-icons-alist 'company-box-icons-all-the-icons)
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
  :after company
  :defines company-quickhelp-delay
  :hook (global-company-mode . company-quickhelp-mode)
  :custom (company-quickhelp-delay 1))

;; Company statistics
(use-package company-statistics
  :ensure t
  :diminish
  :after company
  :hook (global-company-mode . company-statistics-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish
  :custom (flycheck-check-syntax-automatically '(idle-change save))
  :config
  (flycheck-add-mode 'javascript-eslint 'tide-mode)
  (global-flycheck-mode)
  :commands (flycheck-add-mode))

;; Which Function
(which-function-mode)
(setq which-func-unknown "∅")

;; YAS code snippets
(use-package yasnippet
  :disabled t
  :hook (prog-mode . yas-minor-mode)
  :commands (yas-reload-all)
  :config
  (yas-reload-all))

;; Colors
(use-package ansi-color
  :ensure t
  :commands (ansi-color-apply-on-region)
  :config
  (add-hook 'compilation-filter-hook
            (lambda () (ansi-color-apply-on-region (point-min)
                                                   (point-max)))))

;; Compilation
(use-package compile
  :ensure t
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)

  (compilation-error-regexp-alist-alist
   (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                1 ;; file
                2 ;; line
                3 ;; column
                )
         compilation-error-regexp-alist-alist))
  (compilation-error-regexp-alist (cons 'node compilation-error-regexp-alist))
  :bind (("C-c C-c" . compile)
	       ("C-c C-r" . recompile)))

;; Colorise some keywords
(add-hook 'prog-mode-hook
	        (lambda ()
	          (font-lock-add-keywords nil
				                            '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\|NOTE\\)"
				                               1 font-lock-warning-face t)))))

;; Disable Version Control
(setq vc-handled-backends nil)

;; Projectile
(use-package projectile
  :ensure t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-S-f" . projectile-find-file)
	       ("C-S-s" . projectile-ag)
	       ("C-S-P" . projectile-switch-project)
	       ("C-S-r" . projectile-run-project))
  :commands (projectile-register-project-type)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  (defvar projetile-indexing-method 'alien)
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t))

(use-package ag
  :ensure t
  :diminish
  :after projectile
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :custom-face
  (ag-match-face ((t (:background nil :foreground "hot pink" :weight bold)))))

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
	       ("C-c C-p" . prettier-js))
  :custom-face
  (tide-hl-identifier-face ((t (:background "gray11" :underline t :weight bold)))))

(use-package npm-mode
  :ensure t
  :diminish
  :defer t
  :hook ((typescript-mode . npm-mode)
         (javascript-mode . npm-mode))
  :commands (npm-mode-npm-run)
  :config
  (local-set-key (kbd "C-c C-c") (lambda()
				                           (interactive)
				                           (npm-mode-npm-run "build"))))

(use-package prettier-js
  :ensure t
  :diminish
  :custom (prettier-js-show-errors nil))

(use-package add-node-modules-path
  :ensure t
  :defer t
  :after tide)

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
  :hook ((yaml-mode . prettier-js-mode)))

;; CSV
(use-package csv-mode
  :ensure t
  :custom (csv-separators '("," ";" "|" "\t")))

(use-package org
  :ensure nil
  :config (define-key org-mode-map (kbd "<C-tab>") 'other-window)
  :hook ((org-mode . org-indent-mode)
	       (org-mode . visual-line-mode))
  :custom (org-hide-emphasis-markers t)
  :custom-face
  (org-document-title ((t :height 2.0)))
  (org-level-1 ((t :inherit outline-1 :weight extra-bold :height 1.5)))
  (org-level-2 ((t :inherit outline-2 :weight bold :height 1.3)))
  (org-level-3 ((t :inherit outline-3 :weight bold :height 1.1)))
  (org-level-4 ((t :inherit outline-4 :weight bold :height 1.0)))
  (org-level-5 ((t :inherit outline-5 :weight semi-bold :height 1.0)))
  (org-level-6 ((t :inherit outline-6 :weight semi-bold :height 1.0)))
  (org-level-7 ((t :inherit outline-7 :weight semi-bold)))
  (org-level-8 ((t :inherit outline-8 :weight semi-bold))))

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
  :config (savehist-mode)
  :custom
  (history-length 128)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring
				                           search-ring
				                           regexp-search-ring
				                           npm-mode-npm-run
				                           package-install
				                           package-delete
				                           projectile-run-project
				                           compile)))

;; Environment variables
(use-package exec-path-from-shell
  :ensure t
  :diminish
  :custom (exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))

;; Emacs Start Up Profiler
(use-package esup
  :ensure t
  :diminish
  :defer t
  :commands (esup)
  :custom (message-log-max t))

;; Init file debugger
(use-package bug-hunter
  :ensure t
  :diminish
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :diminish
  :after (all-the-icons))

;; Icons: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-scale-factor 1.1)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

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

(use-package writeroom-mode
  :ensure t
  :diminish
  :defer t
  :commands (writeroom-adjust-width)
  :preface
  (defun writeroom-toggle-on  ()
    (writeroom-adjust-width 40) ;; Width = 80 + 40
    (text-scale-increase 1)
    (text-scale-increase 1))
  (defun writeroom-toggle-off  ()
    (text-scale-decrease 1)
    (text-scale-decrease 1))
  :bind ("<f6>" . writeroom-mode)
  :custom (writeroom-restore-window-config 1)
  :config
  (add-hook 'writeroom-mode-enable-hook 'writeroom-toggle-on)
  (add-hook 'writeroom-mode-disable-hook 'writeroom-toggle-off))

(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value `(,temporary-file-directory))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package sudo-edit
  :ensure t
  :config (sudo-edit-indicator-mode)
  :bind (:map ctl-x-map
              ("M-s" . sudo-edit)))

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
  "Kill all buffers except *Messages* *scratch* and *dashboard**."
  (interactive)
  (mapc 'kill-buffer
	      (remove-if
	       (lambda (x)
	         (or
	          (string-equal "*Messages*" (buffer-name x))
	          (string-equal "*scratch*" (buffer-name x))
	          (string-equal "*dashboard*" (buffer-name x))))
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

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#dddddd"])
 '(custom-safe-themes '(default))
 '(fci-rule-color "#505050")
 '(jdee-db-active-breakpoint-face-colors (cons "#1b1d1e" "#fc20bb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1b1d1e" "#60aa00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1b1d1e" "#505050"))
 '(objed-cursor-color "#d02b61")
 '(package-selected-packages
   '(highlight-indent-guides stripe-buffer vterm company-statistics magit persistent-scratch yasnippet writeroom-mode which-key uuidgen use-package undo-fu tide systemd smex smart-hungry-delete rainbow-mode rainbow-delimiters prettier-js org-superstar npm-mode multiple-cursors move-text minions json-mode ivy-prescient ivy-hydra helpful flx exec-path-from-shell esup doom-themes doom-modeline dockerfile-mode docker-compose-mode dired-subtree diminish dashboard csv-mode counsel-projectile company-quickhelp company-prescient company-box bug-hunter auto-package-update all-the-icons-ivy-rich all-the-icons-dired aggressive-indent ag add-node-modules-path))
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
 '(ag-match-face ((t (:background nil :foreground "hot pink" :weight bold))))
 '(company-tooltip ((t (:inherit tooltip :background nil :family "Source Code Pro"))))
 '(dashboard-banner-logo-title ((t (:inherit default :foreground "slate gray" :slant italic :weight light))))
 '(dashboard-items-face ((t nil)))
 '(doom-modeline-bar ((t (:background "#906CFF"))))
 '(font-lock-warning-face ((t (:inherit warning :foreground "sandy brown" :weight bold))))
 '(hl-line ((t (:extend t :background "#24213b"))))
 '(ivy-current-match ((t (:foreground "#CBE3E7" :weight bold :background "#39374E"))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(ivy-minibuffer-match-face-3 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(ivy-minibuffer-match-face-4 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(mode-line ((t (:background "#161424" :box nil))))
 '(mode-line-inactive ((t (:background "#191729" :foreground "#858FA5" :box nil))))
 '(org-document-title ((t :height 2.0)))
 '(org-level-1 ((t :inherit outline-1 :weight extra-bold :height 1.5)))
 '(org-level-2 ((t :inherit outline-2 :weight bold :height 1.3)))
 '(org-level-3 ((t :inherit outline-3 :weight bold :height 1.1)))
 '(org-level-4 ((t :inherit outline-4 :weight bold :height 1.0)))
 '(org-level-5 ((t :inherit outline-5 :weight semi-bold :height 1.0)))
 '(org-level-6 ((t :inherit outline-6 :weight semi-bold :height 1.0)))
 '(org-level-7 ((t :inherit outline-7 :weight semi-bold)))
 '(org-level-8 ((t :inherit outline-8 :weight semi-bold)))
 '(quote (doom-modeline-bar ((t (:background "#906CFF")))))
 '(region ((t (:extend t :background "#333146"))))
 '(swiper-background-match-face-1 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(swiper-background-match-face-2 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(swiper-background-match-face-3 ((t (:background "hot pink" :weight bold :background nil))))
 '(swiper-background-match-face-4 ((t (:foreground "hot pink" :weight bold :background nil))))
 '(swiper-line-face ((t (:background "#39374E" :foreground "#CBE3E7" :weight bold))))
 '(swiper-match-face-1 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
 '(swiper-match-face-2 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
 '(swiper-match-face-3 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
 '(swiper-match-face-4 ((t (:background nil :foreground "#CBE3E7" :weight bold))))
 '(whitespace-tab ((t (:foreground "#636363")))))

(provide 'emacs)
;;; emacs ends here
