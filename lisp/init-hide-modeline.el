(use-package hide-mode-line
  :ensure t
  :hook
  ((Help-mode neotree-mode imenu-list-minor-mode minimap-mode) . (lambda ()(let ((hide-mode-line-format '("%b")))
									     (hide-mode-line-mode +1)))))


(provide 'init-hide-modeline)
