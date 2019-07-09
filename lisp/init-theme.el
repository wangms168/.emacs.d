;;; init-theme.el --- modeline configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (load-theme 'misterioso  t)   ;;  wheatgrass \ manoj-dark \ sanityinc-tomorrow-bright \ misterioso

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :config
;;   (load-theme 'atom-one-dark t)
;;   )

;; (use-package color-theme
;;   :config
;;   (eval-after-load "color-theme"
;;     '(progn
;;        (color-theme-initialize)
;;        ;; (color-theme-hober)
;;        ;; (color-theme-clarity-and-beauty)
;;        (color-theme-charcoal-black)    ;hober\Calm Forest \Charcoal Black
;;        ;; (color-theme-calm-forest)
;;        )))
;; 用M-x color-theme-select来选择你喜欢的颜色主题了

;; (use-package color-theme-sanityinc-tomorrow
;;   :hook (emacs-startup . (lambda () (color-theme-sanityinc-tomorrow-night) ))
;;   )

(use-package monokai-theme
  :hook (emacs-startup . (lambda () (load-theme 'monokai t) ))
  )

;; (use-package color-theme-sanityinc-tomorrow
;;   :hook (emacs-startup . (lambda ()  (color-theme-sanityinc-tomorrow--define-theme bright)))
;;   ;; :config
;;   ;; (add-hook 'emacs-startup-hook (lambda ()  (color-theme-sanityinc-tomorrow--define-theme bright)))
;;   )

;; (use-package afternoon-theme
;;   :hook (emacs-startup . (lambda ()  (load-theme 'afternoon)))
;;   ;; :config
;;   ;; (add-hook 'emacs-startup-hook (lambda () (load-theme 'afternoon)))
;;   )

;; (use-package naquadah-theme
;;   :hook (emacs-startup . (lambda ()  (load-theme 'naquadah)))
;;   ;; :config
;;   ;; ;; Load my favourite theme.
;;   ;; (add-hook 'emacs-startup-hook (lambda () (load-theme 'naquadah)))
;;  )


(provide 'init-theme)

;;; init-theme.el ends here
