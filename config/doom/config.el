;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq-default user-full-name "Andrés David Hurtado Fernández"
              user-mail-address "OsiNubis99@PM.me")

;PERSONAL KEY BINDING
(global-set-key (kbd "M-d") 'evil-multiedit-match-and-next)
(global-set-key (kbd "M-u") 'evil-multiedit-match-and-prev)
(global-set-key (kbd "C-;") 'save-buffer)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key [M-left] 'previous-buffer)
(global-set-key [M-right] 'next-buffer)
(map! :leader
      :desc "Open Ranger" "." 'ranger)

;INTERFACE
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq-default doom-theme 'doom-old-hope)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(setq-default display-line-numbers-type 'relative)

;RANGER
(setq ranger-show-hidden t)
(setq ranger-width-preview 0.35)
(setq ranger-width-parents 0.25)
(setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
(setq ranger-max-preview-size 5)
(setq ranger-cleanup-eagerly t)
(setq ranger-dont-show-binary t)

;TABS CONFIG
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default electric-indent-mode 1)
(setq backward-delete-char-untabify-method 'hungry)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/dotFiles/Org/")

(map! :leader
      (:prefix ("-" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/dotFiles/Org/agenda.org"))
       :desc "Edit doom config.el" "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.el"))
       :desc "Edit doom config.org" "o" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))

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
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/dotFiles/Org/"
        org-agenda-files '("~/dotFiles/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/dotFiles/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        '((sequence
           "TODO(t)"           ; A task that is ready to be tackled
           "BLOG(b)"           ; Blog writing assignments
           "GYM(g)"            ; Things to accomplish at the gym
           "PROJ(p)"           ; A project that contains other tasks
           "VIDEO(v)"          ; Video assignments
           "WAIT(w)"           ; Something is holding up this task
           "|"                 ; The pipe necessary to separate "active" states and "inactive" states
           "DONE(d)"           ; Task has been completed
           "CANCELLED(c)" )))) ; Task has been cancelled

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

(setq
 projectile-project-search-path '("~/dotFiles/"))

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
