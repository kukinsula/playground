;; Themes
(add-to-list 'custom-theme-load-path "/home/kuk/.emacs.d/themes")


;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


;; Window bars
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse    
(setq scroll-step 3) ;; keyboard scroll one line at a time


;; Line Number
(global-linum-mode t)
(setq linum-format "%d ")


;; Maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Set font size to 10
(set-face-attribute 'default nil :font "Mono 10")


;; Startup buffer to list files
(setq inhibit-splash-screen t)
(dired "/home/kuk/info")


;; Print line numbers
(setq column-number-mode t)


;; Set frame title to opened buffer name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Ask before close
(setq confirm-kill-emacs 'y-or-n-p)


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
    ("4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(package-selected-packages (quote (json-mode yaml-mode go-mode auto-complete)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Golang mode
(setq go-mode-hook
'(lambda ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (setq tab-width 2)
  )
)


;; Tide TypeScript mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
