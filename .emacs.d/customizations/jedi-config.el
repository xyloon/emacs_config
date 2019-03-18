;;company-mode global activation
(add-hook 'after-init-hook 'global-company-mode)

;;(pyenv-mode)
(elpy-enable)


(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;; enabled this block
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)

;;this two line will fix some key bidning error in elpy(in youtuble)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")

(defun jedi-config:setup-keys ()
  (local-set-key (kbd "M-.") 'jedi:key-goto-definition)
  (local-set-key (kbd "M-,") 'jedi:key-goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (local-set-key (kbd "M-/") 'jedi:get-in-function-call)
)

(add-hook 'python-mode-hook 'jedi-config:setup-keys)
(setq jedi:get-in-function-call-timeout 10000000)
