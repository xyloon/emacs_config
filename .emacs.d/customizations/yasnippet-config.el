(use-package yasnippet
  :ensure t
  :config 
  ( use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
)

(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
