;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; PERSONAL INFORMATION
(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

;; PERSONAL KEY BINDING
(global-set-key (kbd "M-d") 'evil-multiedit-match-and-next)
(global-set-key (kbd "M-u") 'evil-multiedit-match-and-prev)
(global-set-key (kbd "C-,") 'save-buffer)
(global-set-key (kbd "C-/") 'comment-line)
(map! :leader
      :desc "Open Dired" "." 'dired-jump)
(map! :leader
      :desc "Open Dired" "SPC" 'projectile-dired)
(map! :leader
      :desc "Format Buffer" "/" '+format/buffer)

;; FONTS CONFIG
(setq doom-font (font-spec :family "monospace" :size 14)
      doom-big-font (font-spec :family "monospace" :size 16))

;; INTERFACE
(setq-default doom-theme 'doom-old-hope)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)
(use-package emojify
  :hook (after-init . global-emojify-mode))
(setq-default display-line-numbers-type 'relative)

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

;; ORG
(setq org-directory "~/dotFiles/Org/")
(defun my/org-mode/load-prettify-symbols () "Prettify org mode keywords"
       (interactive)
       (setq prettify-symbols-alist
             (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                     '(("#+begin_src" . ?)
                       ("#+end_src" . ?)
                       ("#+begin_example" . ?)
                       ("#+end_example" . ?)
                       ("#+DATE:" . ?⏱)
                       ("#+AUTHOR:" . ?✏)
                       ("[ ]" .  ?☐)
                       ("[X]" . ?☑ )
                       ("[-]" . ?❍ )
                       ("lambda" . ?λ)
                       ("#+header:" . ?)
                       ("#+name:" . ?﮸)
                       ("#+results:" . ?)
                       ("#+call:" . ?)
                       (":properties:" . ?)
                       (":logbook:" . ?))))
       (prettify-symbols-mode 1))
(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq org-superstar-headline-bullets-list
        '("⁖" "◉" "○" "✸" "✿"))
  (setq org-directory "~/dotFiles/Org/"
        org-agenda-files '("~/dotFiles/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-journal-dir "~/dotFiles/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-ellipsis " ▼ "
        org-log-done 'time
        org-hide-emphasis-markers t
        org-todo-keywords
        '((sequence
           "TODO(t)"
           "BLOG(b)"
           "GYM(g)"
           "PROJ(p)"
           "WAIT(w)"
           "|"
           "DONE(d)"
           "CANCELLED(c)"))))
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; PROJECTILE
(setq! projectile-project-search-path '("~/dotFiles/"))

;; WAKATIME
(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
