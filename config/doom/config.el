;; [[file:config.org::+BEGIN_SRC emacs-lisp][No heading:1]]
(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

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

(setq doom-font (font-spec :family "monospace" :size 14)
      doom-big-font (font-spec :family "monospace" :size 16))
(setq-default doom-theme 'doom-badger)

(modify-all-frames-parameters
 '((undecorated . t)
   (fullscreen . maximized)))

(use-package emojify
  :hook (after-init . global-emojify-mode))
(setq-default display-line-numbers-type 'relative)
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1)
(beacon-mode 1)
(setq ranger-hide-cursor nil)
(setq ranger-show-hidden t)
(setq ranger-modify-header t)
(editorconfig-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default electric-indent-mode 1)
(setq backward-delete-char-untabify-method 'hungry)

(map! :leader
      (:prefix ("-" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/dotFiles/Org/agenda.org"))
       :desc "Edit doom config.el" "c" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/config.el"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/init.el"))
       :desc "Edit doom config.org" "o" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/config.org"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/packages.el"))
       :desc "Edit xmonad config file" "x" #'(lambda () (interactive) (find-file "~/dotFiles/config/xmonad/xmonad.hs"))))

(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

(add-hook 'dart-mode-hook 'lsp)
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
;; No heading:1 ends here
