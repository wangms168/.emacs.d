;; -*- coding: utf-8 -*-
(use-package all-the-icons
  :ensure t
  )

(use-package all-the-icons-dired
  :ensure t
  ;; :config
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(provide 'init-icons)
;;; init-modeline.el ends here
