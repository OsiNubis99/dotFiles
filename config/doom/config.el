;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; PERSONAL INFORMATION
(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

;; PERSONAL KEY BINDING
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-,") 'save-buffer)
(global-set-key (kbd "C-/") 'comment-line)
(map! :leader
      :desc "Format Buffer" "/" '+format/buffer
      :desc "Multi cursor" "m c" 'mc/mark-all-words-like-this
      :desc "Open ibufer on other window" "v" 'ibuffer-other-window
      :desc "Open File Folder" "SPC" 'dired-jump
      :desc "Open Project Folder" "." 'projectile-dired)

;; FONTS CONFIG
(setq doom-font (font-spec :family "monospace" :size 14)
      doom-big-font (font-spec :family "monospace" :size 16))

;; THEME
(setq-default doom-theme 'doom-badger)

;; DECORATORS
(modify-all-frames-parameters
 '((undecorated . t)
   (fullscreen . maximized)
   (alpha . 90)))
(use-package emojify
  :hook (after-init . global-emojify-mode))
(setq-default display-line-numbers-type 'relative)
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1)
(beacon-mode 1)

;; RANGER
(setq ranger-hide-cursor nil)
(setq ranger-show-hidden t)
(setq ranger-modify-header t)

;; INDENT CONFIG
(editorconfig-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default electric-indent-mode 1)
(setq backward-delete-char-untabify-method 'hungry)

;; FAST FILES
(map! :leader
      (:prefix ("-" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/dotFiles/Org/agenda.org"))
       :desc "Edit doom config.el" "c" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/config.el"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/init.el"))
       :desc "Edit doom config.org" "o" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/config.org"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/packages.el"))
       :desc "Edit xmonad config file" "x" #'(lambda () (interactive) (find-file "~/dotFiles/config/xmonad/xmonad.hs"))))

;; WAKATIME
(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

;; DART & FLUTTER
(add-hook 'dart-mode-hook 'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))
(use-package dart-mode
  :custom dart-sdk-path "/opt/flutter/bin/cache/dart-sdk/")
(use-package hover
  :after dart-mode
  :bind (:map hover-minor-mode-map
         ("C-M-z" . #'hover-run-or-hot-reload)
         ("C-M-x" . #'hover-run-or-hot-restart))
  :init
  (setq hover-command-path "/home/andres/go/bin/hover"
        hover-hot-reload-on-save t
        hover-clear-buffer-on-hot-restart t)
  (hover-minor-mode 1))
