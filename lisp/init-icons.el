;; -*- coding: utf-8 -*-
(use-package all-the-icons)

(use-package all-the-icons-dired
  ;; :config
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(provide 'init-icons)
;;; init-modeline.el ends here
