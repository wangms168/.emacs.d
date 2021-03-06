;;; init-key.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helm-descbinds
  :init
  (define-key help-map (kbd "b") 'helm-descbinds)              ;; describe-bindings
  :config
  (helm-descbinds-mode)
  )

(use-package which-key
  :config
  (which-key-mode)
  )

;; (use-package guide-key
;;   :config
;;   (setq guide-key/guide-key-sequence t)
;;   (guide-key-mode 1)  ; Enable guide-key-mode
;;   )

(provide 'init-key)

;;; init-key.el ends here
