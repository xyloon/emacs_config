;;; Set up package
(require 'package)
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

;; (use-package use-package-chords
;; 	     :ensure t
;; 	     :config (key-chord-mode 1))

(use-package try
	:ensure t)


(defvar packages-to-install
  '(
   wttrin
   undo-tree
   elpy
   flycheck
   magit
   ;; personal adding
   ansible
   4clojure
   ssh
   ein
   company-jedi
   google-translate
   iedit
   yaml-mode
   ;; makes handling lisp expressions much, much easier
   ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
   paredit
   ;; key bindings and code colorization for Clojure
   ;; https://github.com/clojure-emacs/clojure-mode
   clojure-mode
   ;; extra syntax highlighting for clojure
   clojure-mode-extra-font-locking
   ;; integration with a Clojure REPL
   ;; https://github.com/clojure-emacs/cider
   cider
   ;; allow ido usage in as many contexts as possible. see
   ;; customizations/navigation.el line 23 for a description
   ;; of ido
   ;; ido-ubiquitous
   ;; Enhances M-x to allow easier execution of commands. Provides
   ;; a filterable list of possible commands in the minibuffer
   ;; http://www.emacswiki.org/emacs/Smex
   smex
   ;; project navigation
   projectile
   ;; colorful parenthesis matching
   rainbow-delimiters
   ;; edit html tags like sexps
   tagedit
   ;; for clojure figwheel
   inf-clojure
   ;; [] () 닫기
   smartparens
   ;; rust mode
   rust-mode
   ;; multiple terminal management
   multi-term
   ;;
   pyenv-mode-auto
   ;; requirement for coverage-mode 
   ov
   ;; for python test
   python-pytest
   ;; yasnippet
   yasnippet-snippets
   org-tree-slide
   ob-rust
   ob-restclient
   ox-gfm
   ivy
   swiper
   ace-window
   counsel
   git-gutter
   py-autopep8
   smart-mode-line-powerline-theme
   sphinx-doc
   pyimport
   )
  )

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'packages-to-install 'exec-path-from-shell)) 

   
(dolist (p packages-to-install)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/org_based_init.org"))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(coffee-tab-width 2)
 '(company-auto-complete (quote ignore))
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes
   (quote
    ("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-autodoc elpy-module-sane-defaults)))
 '(elpy-shell-use-project-root t)
 '(elpy-test-pytest-runner-command (quote ("py.test" "-s")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(package-selected-packages
   (quote
    (pyimport counsel swiper ace-window tabbar yasnippet-snippets flymake pyenv-mode-auto wttrin undo-tree elpy magit 4clojure use-package-ensure-system-package use-package)))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
