(use-package dumb-jump
  :ensure t
  :init
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  ;; (global-set-key (kbd "M-g") 'dumb-jump-hydra/body)
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  )



(provide 'init-dumb-jump)

;;; init-ivy.el ends here
