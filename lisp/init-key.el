(use-package helm-descbinds
  :ensure t
  :init
  (define-key help-map (kbd "b") 'helm-descbinds)              ;; describe-bindings
  :config
  (helm-descbinds-mode)
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

;; (use-package guide-key
;;   :ensure t
;;   :config
;;   (setq guide-key/guide-key-sequence t)
;;   (guide-key-mode 1)  ; Enable guide-key-mode
;;   )


(provide 'init-key)
;;; init-modeline.el ends here
