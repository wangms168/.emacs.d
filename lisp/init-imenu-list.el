;; imenu-list
(use-package imenu-list
  :ensure t
  :defer t
  :init
  (setq imenu-list-position 'right)
  (setq imenu-list-size 0.15)
  ;; (setq imenu-list-auto-resize t)
  (setq imenu-list-focus-after-activation t)
  (global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
  :config
  ;; https://blog.jft.rocks/emacs/treemacs-icons.html
  ;; (add-hook 'imenu-list-major-mode-hook #'hide-mode-line-mode)
  (add-hook 'imenu-list-major-mode-hook '(lambda ()
  					  (setq-local header-line-format nil)    ;; 没有tabbar
					  (setq-local mode-line-format nil)      ;; 没有mode-line
					  ;; (display-line-numbers-mode -1)
					  ))

  )



(provide 'init-imenu-list)

;;; init-neotree.el ends here
