;; imenu-list
(use-package imenu-list
  :ensure t
  :hook (imenu-list-major-mode-hook . (lambda ()
					(setq-local header-line-format nil)    ;; 没有tabbar
					(setq-local mode-line-format nil)      ;; 没有mode-line
					;; (display-line-numbers-mode -1)
					))
  :bind
  ("<f10>" . imenu-list-smart-toggle)
  :custom-face
  (imenu-list-entry-face-1 ((t (:foreground "white"))))
  :custom
  (imenu-list-focus-after-activation t)
  ;; (imenu-list-auto-resize nil)
  (imenu-list-size 0.15)
  (imenu-list-position 'right)
  )

;; https://blog.jft.rocks/emacs/treemacs-icons.html
;; (add-hook 'imenu-list-major-mode-hook #'hide-mode-line-mode)

(provide 'init-imenu-list)

;;; init-neotree.el ends here
