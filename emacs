(add-to-list 'custom-theme-load-path "/home/kuk/.emacs.d/themes")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Window
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse    
(setq scroll-step 3) ;; keyboard scroll one line at a time

;; Colors
;; (add-to-list 'default-frame-alist '(background-color . "#1023ff"))

;; Line Number
(global-linum-mode t)
(setq linum-format "%d ")

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
    ("f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(package-selected-packages (quote (json-mode yaml-mode go-mode auto-complete)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq go-mode-hook
'(lambda ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (setq tab-width 2)
  )
)

(set-face-attribute 'default nil :height 120)

(setq column-number-mode t)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
