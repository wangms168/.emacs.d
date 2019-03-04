;;; init-visual.el --- Initialize editor configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode)
  ;; :config
  ;; (add-hook 'after-init-hook 'global-page-break-lines-mode)
  )



(provide 'init-visual)

;;; init-visual.el ends here
