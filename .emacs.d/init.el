;;; Set up package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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

(use-package which-key
	:ensure t 
	:config
	(which-key-mode))


;; Korean Setting
;;(set-language-environment "Korean")
;;(setq default-korean-keyboard "3")
(setq input-method-verbose-flag nil input-method-highlight-flag nil)


(setq initial-scratch-message nil)
(setq column-number-mode t)
(set-keyboard-coding-system nil)


;;; package --- Summary
;;; Commentary:
;;; Begin initialization
;;; Turn off mouse interface early in startup to avoid momentary display
;;; Code:

;; 에러시 디버그모드
;; (setq debug-on-error t)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq ad-redefinition-action 'accept) ;; 함수 redefine으로 인한 경고 생략

(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(setq echo-keystrokes 0.001) ;; 키입력시 에코창에 표시되는 딜레이 타임, 거이 없게 설정

(setq tab-width 2)

(set-variable 'cursor-type 'bar)


;;; Scroll setup
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-conservatively 200) ;; 스크롤 도중에 센터로 커서 이동하지 않도록
(setq scroll-margin 3) ;; 스크롤시 남기는 여백

;; 백업들 끄기
;;(setq backup-inhibited t)
;;(setq make-backup-files nil)
;;(setq auto-save-default nil)

;; No popup frame(새버퍼열때 현재 프레임에서 열기)
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;; 소리 끄고 비쥬얼벨로
(setq visible-bell t)


;;; Paste setup
(defun copy-from-osx ()
  "Copy from osx."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))




;; split smart!
(defun split-smart ()
  (if (< (window-pixel-width) (window-pixel-height))
      (with-selected-window (selected-window)
        (split-window-vertically))
    (with-selected-window (selected-window)
      (split-window-horizontally))))


(defcustom split-window-preferred-function 'split-smart
  "Split smart."
  :type 'function
  :version "25.1"
  :group 'windows)

(when (and window-system (eq system-type 'darwin))
  ;;(set-face-attribute 'default nil :family "Source Code Pro" :height 130 :weight 'ultra-light)
  (set-face-attribute 'default nil :family "D2Coding" :height 130 :weight 'ultra-light)
  ;;(set-face-attribute 'default nil :family "Hack" :height 130 :weight 'light)
  ;;(set-fontset-font t 'hangul (font-spec :name "나눔고딕코딩"))
  (set-fontset-font t 'hangul (font-spec :family "D2Coding"))
  ;; 풀스크린키 변경
  (define-key global-map (kbd "C-M-f") 'toggle-frame-fullscreen)

  ;; 저장키 변경
  ;;(define-key global-map (kbd "M-s") 'save-buffer)
  ;;;;; Only for OSX
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (unless window-system
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))
)

(when (and window-system (eq system-type 'gnu/linux))
  (set-face-attribute 'default nil :family "D2Coding" :height 130 :weight 'ultra-light)
  ;;(set-face-attribute 'default nil :family "Hack" :height 130 :weight 'light)
  ;;(set-fontset-font t 'hangul (font-spec :name "나눔고딕코딩"))
  (set-fontset-font t 'hangul (font-spec :family "D2Coding"))
)

(when (and window-system (eq system-type 'windows-nt))
  (set-face-attribute 'default nil :family "D2Coding" :height 130 :weight 'ultra-light)
  ;;(set-face-attribute 'default nil :family "Hack" :height 130 :weight 'light)
  ;;(set-fontset-font t 'hangul (font-spec :name "나눔고딕코딩"))
  (set-fontset-font t 'hangul (font-spec :family "D2Coding"))
)

;;; default modes
;; unset some default keybinding for my custom key bindings
(define-key global-map (kbd "C-j") nil)

;; dired
(put 'dired-find-alternate-file 'disabled nil)

;; hippie-expand
;; (global-set-key "\M-n" 'hippie-expand)

;; delete highlighted region before yank
(delete-selection-mode 1)


;; (add-to-list 'load-path "~/dotfiles/my_emacs_packages")
;; (require 'upbo)

(defvar packages-to-install
  '(
   wttrin
   undo-tree
   elpy
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


;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")
;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")
;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")
;; These customizations make editing a bit nicer.
(load "editing.el")
;; Hard-to-categorize customizations
(load "misc.el")
;; For editing lisps
(load "elisp-editing.el")
;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "org-mode.el")
(load "jedi-config.el")

(use-package ein
  :ensure t)

;; 이부분은 ssh 연결에 사용함
(require 'ssh)
(add-hook 'ssh-mode-hook
          (lambda ()
            (setq ssh-directory-tracking-mode t)
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))


(require 'projectile)
(projectile-global-mode)


;;(add-to-list 'exec-path "/usr/local/bin")

;; for custom themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(add-to-list 'load-path "~/.emacs.d/themes")  


;;; markdown mode
(use-package markdown-mode
 :ensure t
 :commands (markdown-mode gfm-mode)
 :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "multimarkdown"))

;;; Utilities
(use-package google-translate
 :ensure t
 :init
 (require 'google-translate)
 (require 'google-translate-smooth-ui)
 (setq google-translate-translation-directions-alist
       '(("en" . "ko") ("ko" . "en")))
 (setq google-translate-pop-up-buffer-set-focus t)
 (setq google-translate-output-destination 'echo-area)
 (setq max-mini-window-height 0.5)
 :bind
 ("C-c n" . google-translate-smooth-translate))

;;(use-package beacon
;;  :ensure t
;;  :diminish beacon-mode
;;  :config
;;  (beacon-mode 1))

;;; Tools

;;;have to install
;;(use-package ox-reveal
;;  :ensure t
;;  :init
;;  (setq org-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.3.0/"))


;;;have to install
;;(use-package ob-restclient
;;  :ensure t)



;;; multi term이 사용 가능하도록 확인 (shell로 zsh를 써야 하는지?)
;; (use-package multi-term
;;   :ensure t
;;   :init
;;   (setq multi-term-program "/bin/zsh")
;;   :bind
;;   ("C-c i" . multi-term))

;; terminal(멀티텀포함)에서 C-j를 글로벌 맵이용하도록 훅
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (define-key term-raw-map (kbd "C-j")
;;                (lookup-key (current-global-map) (kbd "C-j")))))


;;Magit과 함께 이 부분을 살려야 함
(defun auto-commit-files (list)
  (interactive
   (list (list (buffer-file-name (current-buffer)))))
  "LIST to be auto commit"
  (while list
    (let* ((file (car list))
           (file-buffer (get-file-buffer file)))
      (when file-buffer
        (set-buffer file-buffer)
        (when (magit-anything-modified-p nil file)
          (magit-call-git "add" file)
          (magit-call-git "commit" "-m" (concat file " update"))
          (magit-call-git "push" "origin")
          (magit-refresh)
          (print (concat file " is pushed!!!")))))
    (setq list (cdr list))))

(use-package magit
  :commands magit-get-top-dir
  :diminish auto-revert-mode
  :ensure t
  :init
  ;; magit 오토 리버트시 버퍼의 브랜치명까지 갱신하도록
  (setq auto-revert-check-vc-info t)
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/site-lisp/magit/Documentation/"))
  ;;; 이맥스가 기본적으로 제공하는 Git 백엔드를 켜두면 매우 느려진다. magit만 쓴다.
  (setq vc-handled-backends nil)
  :config
  (setq vc-follow-symlinks t)
  (setq find-file-visit-truename t)
  (setq magit-refresh-status-buffer 'switch-to-buffer)
  (setq magit-rewrite-inclusive 'ask)
  (setq magit-save-some-buffers t)
  (setq magit-set-upstream-on-push 'askifnotset)
  :bind
  ("C-c m" . magit-status))


;;;
;; (use-package prodigy
;;   :ensure t
;;   :bind
;;   ("C-c f" . prodigy)
;;   :init
;;   (prodigy-define-service
;;     :name "tui.chart dev server"
;;     :command "npm"
;;     :cwd "~/masterpiece/tui.chart"
;;     :args '("run" "dev")
;;     :port 8080
;;     :stop-signal 'sigkill
;;     :kill-process-buffer-on-stop t
;;     :tags '(webpack-server))

;;   (prodigy-define-service
;;     :name "tui.chart test"
;;     :command "npm"
;;     :cwd "~/masterpiece/toast-beuaty"
;;     :args '("run" "test")
;;     :stop-signal 'sigkill
;;     :kill-process-buffer-on-stop t
;;     :tags '(karma))

;;   (prodigy-define-service
;;     :name "wysiwyg contents editor"
;;     :command "npm"
;;     :cwd "~/masterpiece/wce"
;;     :args '("run" "dev")
;;     :port 8080
;;     :stop-signal 'sigkill
;;     :kill-process-buffer-on-stop t
;;     :tags '(webpack-server))

;;   (prodigy-define-service
;;     :name "toast drive dev server"
;;     :command "npm"
;;     :cwd "~/masterpiece/toast-drive-web-service"
;;     :args '("run" "dev")
;;     :port 3000
;;     :stop-signal 'sigkill
;;     :kill-process-buffer-on-stop t
;;     :tags '(webpack-server))

;;   (prodigy-define-service
;;     :name "toast drive alpha server"
;;     :command "npm"
;;     :cwd "~/masterpiece/toast-drive-web-service"
;;     :args '("run" "alpha")
;;     :port 3000
;;     :stop-signal 'sigkill
;;     :kill-process-buffer-on-stop t
;;     :tags '(webpack-server))

;;   (prodigy-define-tag
;;     :name 'webpack-server
;;     :ready-message "Http://0.0.0.0:[0-9]+/webpack-dev-server/")

;;   (prodigy-define-tag
;;     :name 'karma
;;     :ready-message " Executed [0-9]+ of [0-9]+ .+")

;;   (prodigy-define-tag
;;     :name 'gulp-watch
;;     :ready-message "Finished 'watch'")

;;   (prodigy-define-tag
;;     :name 'tomcat
;;     :ready-message "Running war on http://localhost:[0-9]+/"))

(use-package wttrin
  :ensure t
  :init
  (setq wttrin-default-accept-language '("Accept-Language" . "ko-KR")))

;; (use-package restclient
;;   :ensure t)

(provide 'init)
;;; init.el ends here
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(hi-yellow ((t (:foreground nil :background nil :underline t)))))


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-names-vector
;;    ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
;;  '(custom-enabled-themes (quote (wheatgrass)))
;;  '(package-selected-packages
;;    (quote
;;     (uuidgen json-mode upbo org lsp-vue diminish company-lsp lsp-javascript-typescript lsp-mode use-package-chords system-packages writeroom-mode parinfer suggest spaceline spacemacs-theme prettier-js helpful org-gcal org-bullets beacon vue-mode indent-guide buffer-move company-sourcekit flycheck-swift swift-mode company-tern google-translate dash-at-point undo-tree dumb-jump highlight-thing highlight-parentheses yasnippet smooth-scroll org-tree-slide hydra autopair paredit iedit ace-window markdown-mode ox-epub ox-gfm counsel-projectile swiper zenburn-theme cyberpunk-theme base16-theme tern-auto-complete tern js-doc js2-mode web-mode goto-last-change rainbow-delimiters expand-region git-timemachine git-gutter flycheck-clojure flycheck-package multi-term 4clojure fiplr visual-regexp multiple-cursors prodigy magit ace-jump-buffer evalator-clojure coin-ticker pyenv-mode elpy projectile ac-cider clojure-mode))))
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
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(package-selected-packages
   (quote
    (yasnippet-snippets flymake pyenv-mode-auto wttrin undo-tree elpy magit 4clojure use-package-ensure-system-package use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
;; (defun cljs-node-repl ()
;;   (interactive)
;;   (inf-clojure "lein trampoline run -m clojure.main /Users/jungseungyang/.emacs.d/repl.clj"))

(defun figwheel-repl ()
  (interactive)
  (inf-clojure "lein figwheel"))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(add-to-list 'load-path "~/.emacs.d/elib")
(load "coverage-mode")

;; code checking via flymake
;; set code checker here from "epylint", "pyflakes"
;; (setq pycodechecker "pyflakes")
;; (when (load "flymake" t)
;;   (defun flymake-pycodecheck-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list pycodechecker (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pycodecheck-init)))
