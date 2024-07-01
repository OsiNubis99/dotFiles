(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

;; Fish
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Shortcuts
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "M-p") 'clipboard-yank)

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
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/dotFiles/home/.config/doom/init.el"))
       :desc "Edit doom config.el" "c" #'(lambda () (interactive) (find-file "~/dotFiles/home/.config/doom/config.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/dotFiles/home/.config/doom/packages.el"))))

;; Style
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
;; (setq ranger-show-hidden t)
;; (setq ranger-modify-header t)
(editorconfig-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default electric-indent-mode 1)
(setq backward-delete-char-untabify-method 'hungry)

;; Projectile
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "node-modules")
  (add-to-list 'projectile-globally-ignored-directories "dist"))
(setq projectile-project-search-path
      '("/home/andres/backup/Repos/Web"
        "/home/andres/backup/Repos/Bots"
        "/home/andres/backup/Repos/Julio"))

;; Jest
(use-package jest)
(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

;; Wakatime
(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

;; Dart
(add-hook 'dart-mode-hook 'lsp)
;; (use-package dart-mode
;;   :custom dart-sdk-path "/opt/flutter/bin/cache/dart-sdk/")
;; (use-package hover
;;   :after dart-mode
;;   :bind (:map hover-minor-mode-map
;;               ("C-M-z" . #'hover-run-or-hot-reload)
;;               ("C-M-x" . #'hover-run-or-hot-restart))
;;   :init
;;   (setq hover-command-path "/home/andres/go/bin/hover")
;;   hover-hot-reload-on-save t
;;   hover-clear-buffer-on-hot-restart t
;;   (hover-minor-mode 1))

;; Completion
(use-package codeium
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable (lambda (api) (not) (memq api '(CancelRequest Heartbeat AcceptCompletion))))

  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t))

;; (use-package company
;;   :defer 0.1
;;   :config
;;   (global-company-mode t)
;;   (setq-default
;;    company-idle-delay 0.05
;;    company-require-match nil
;;    company-minimum-prefix-length 0
;;    company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)))
