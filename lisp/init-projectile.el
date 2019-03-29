;;; init-projectile.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; projectile
;;
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap ("\C-c p" . projectile-command-map)
  :init
  (setq projectile-git-submodule-command nil)
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (use-package counsel-projectile
    :ensure t)
  )

(use-package ag
  :ensure t)

(use-package ripgrep
  :ensure t)

;; (use-package rg
;;   :ensure t)



(provide 'init-projectile)

;;; init-projectile.el ends here

