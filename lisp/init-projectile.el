;;; init-projectile.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
;; projectile
;;
(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("\C-c p" . projectile-command-map)
  :init
  (use-package counsel-projectile)
  (setq projectile-git-submodule-command nil)
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  )

(use-package ag)

(use-package ripgrep)



(provide 'init-projectile)

;;; init-projectile.el ends here
