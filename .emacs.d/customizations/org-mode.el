;; (setq org-todo-keywords '((sequence "TODO" "IN_PROGRESS" "WAITING" "DONE")))

;;;Org mode를 살리고 잘 되도록 만들어야 함... 지금은 주석처리함 
(use-package org
  :ensure t
  :bind
  (("\C-cl" . org-store-link)
   ("\C-ca" . org-agenda)
   ("\C-cc" . org-capture)
   ("\C-cb" . org-iswitchb))
  :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-agenda-files (file-expand-wildcards "~/org/agenda/*.org"))
  (setq org-default-notes-file "~/org/agenda/index.org")
  (setq org-mobile-inbox-for-pull "~/org/agenda/index.org")
  ;; (setq org-mobile-directory "~/Dropbox/앱/MobileOrg")
  (setq org-capture-templates '(("t" "Task" entry
                                 (file+headline "~/org/agenda/index.org" "Task")
                                 "* TODO %i%? %^G")
                                ("l" "Task with link" entry
                                 (file+headline "~/org/agenda/index.org" "Task")
                                 "* TODO %i%? %^G\n%a")
                                ("q" "Task with category" entry
                                 (file+headline "~/org/agenda/index.org" "Task")
                                 "* TODO %i%? %^G\n:PROPERTIES:\n:CATEGORY: %^{PROMPT|MISC|PROJECT|SPROJECT|STUDY}\n:END:")
                                ("o" "Task @office" entry
                                 (file+headline "~/org/agenda/index.org" "Task")
                                 "* TODO %i%? :@office:")
                                ("n" "Note" entry
                                 (file+headline "~/org/agenda/index.org" "Note")
                                 "* %i%?")))

  (setq org-refile-targets '((org-agenda-files :level . 1)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w@/!)" "NEXT(n!)" "|" "HOLD(h@/!)" "DONE(d)" "CANCELLED(c@/!)" "MOVED(m@/!)")))

  (setq org-agenda-custom-commands
        '(("o" "Office View"
           ((agenda "")
            (tags-todo "@office")
            (todo "WAITING")))
          ("sa" "Agenda search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/agenda/*.org"))))
          ("sd" "Document search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/note/*.org"))))))

  (setq org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (emacs-lisp . t)
     (clojure . t)
     ;; (typescript . t)
     (plantuml . t)
     ;; (swift . t)
     (rust . t)
     (restclient . t)))

  (setq org-agenda-restore-windows-after-quit t)

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-strip-leading-and-trailing-blank-lines t)
  (setq org-log-done t)
  (setq org-edit-src-content-indentation 0)
  (setq org-adapt-indentation nil)

  (eval-after-load "org"
    '(require 'ox-gfm nil t))
  (setq org-plantuml-jar-path
        (expand-file-name "~/plantuml/plantuml.jar"))

  ;;yasnippet 하고 tab 충돌 해결
  (defun yas/org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas-expand-from-trigger-key)
              (setq yas-expand-from-trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas-next-field)))

  ;; org에서 linewrap 되게
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  :config
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "M-j") 'org-return-indent)
  (define-key org-mode-map (kbd "<return>") 'org-return-indent)
)

;; Have to install
(use-package org-tree-slide
 :ensure t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
