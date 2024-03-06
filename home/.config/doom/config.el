(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-/") 'comment-line)

(map! :leader
      :desc "Open definition other window" "d" 'xref-find-definitions-other-window
      :desc "Open ibufer on other window" "v" 'ibuffer-other-window
      :desc "Next Tab" "k" 'centaur-tabs-forward-tab
      :desc "Prev Tab" "j" 'centaur-tabs-backward-tab
      :desc "Next Tab Group" "l k" 'centaur-tabs-forward-group
      :desc "Prev Tab Group" "l j" 'centaur-tabs-backward-group
      :desc "Sort Tabs Groups by Project" "l l" 'centaur-tabs-group-by-projectile-project
      :desc "Format Buffer" "/" '+format/buffer
      :desc "Multi cursor" "m c" 'mc/mark-all-words-like-this
      :desc "Open File Folder" "SPC" 'dired-jump
      :desc "Open File" "." 'projectile-find-file
      (:prefix ("-" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/dotFiles/Org/agenda.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/init.el"))
       :desc "Edit doom config.el" "c" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/config.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/dotFiles/config/doom/packages.el"))
       :desc "Edit xmonad config file" "x" #'(lambda () (interactive) (find-file "~/dotFiles/config/xmonad/xmonad.hs"))))

(setq doom-font (font-spec :family "monospace" :size 16)
      doom-big-font (font-spec :family "monospace" :size 18))
(setq-default doom-theme 'doom-badger)

(modify-all-frames-parameters
 '((undecorated . t)
   (fullscreen . maximized)))

(use-package emojify
  :hook (after-init . global-emojify-mode))
(setq-default display-line-numbers-type 'relative)
(beacon-mode 1)
(setq ranger-hide-cursor nil)
(setq ranger-show-hidden t)
(setq ranger-modify-header t)
(editorconfig-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default electric-indent-mode 1)
(setq backward-delete-char-untabify-method 'hungry)
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "*node-modules")
  (add-to-list 'projectile-globally-ignored-directories "*dist"))

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
