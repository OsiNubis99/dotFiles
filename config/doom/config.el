;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; PERSONAL INFORMATION
(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

;; PERSONAL KEY BINDING
(global-set-key (kbd "M-d") 'evil-multiedit-match-all)
(global-set-key (kbd "C-,") 'save-buffer)
(global-set-key (kbd "C-/") 'comment-line)
(map! :leader
      :desc "Open ibufer on other window" "v" 'ibuffer-other-window)
(map! :leader
      :desc "Format Buffer" "/" '+format/buffer)
(map! :leader
      :desc "Open File Folder" "SPC" 'dired-jump)
(map! :leader
      :desc "Open Project Folder" "." 'projectile-dired)

;; FONTS CONFIG
(setq doom-font (font-spec :family "monospace" :size 14)
      doom-big-font (font-spec :family "monospace" :size 16))

;; THEME
(setq-default doom-theme 'doom-badger)

;; DECORATORS
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

;; PROJECTILE
(setq! projectile-project-search-path '("~/dotFiles/"))

;; WAKATIME
(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)
