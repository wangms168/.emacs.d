(unless (and (fboundp 'daemonp) (daemonp))
  (use-package xresources-theme
    :ensure t
    :config
    (load-theme 'xresources t)
    ))


(provide 'init-xresources-theme)
