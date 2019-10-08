;; (use-package gtags
;;   :ensure nil
;;   :hook (emacs-lisp-mode . gtags-mode)
;;   :custom
;;   (gtags-suggested-key-mapping t)
;;   (gtags-auto-update t)
;;   :config
;;   (add-hook 'gtags-mode-hook
;; 	    '(lambda ()
;; 	       ;; [Setting to use vi style scroll key]
;; 	       ;; Local customization (overwrite key mapping)
;; 	       (define-key gtags-mode-map "\C-f" 'scroll-up)
;; 	       (define-key gtags-mode-map "\C-b" 'scroll-down)
;; 	       ))
;;   ;; [Setting to make 'Gtags select mode' easy to see]
;;   (add-hook 'gtags-select-mode-hook
;; 	    '(lambda ()
;; 	       (setq hl-line-face 'underline)
;; 	       (hl-line-mode 1)
;; 	       ))
;;   )


(use-package ggtags
  :ensure t
  :hook (emacs-lisp-mode . ggtags-mode)
  :config
  (define-key ggtags-mode-map (kbd "M-.") nil)     ;; 取消原来M-.对ggtags-find-tag-dwim的绑定。
  (define-key ggtags-mode-map (kbd "C-.") 'ggtags-find-tag-dwim)
  ;; (add-hook 'c-mode-common-hook
  ;; 	    (lambda ()
  ;; 	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
  ;; 		(ggtags-mode 1))))
  )



(provide 'init-ggtags)
