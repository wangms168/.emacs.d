;;; init-theme.el --- modeline configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun theme-graphic-p (theme)
  (if (display-graphic-p)
      (progn
	(message "-->>>> theme-graphic")
	(load-theme 'theme t)
 	)
    (message "-->>>> theme-terminal")
    ))

;; https://emacs-china.org/t/topic/5387/15
;; https://www.cnblogs.com/darwin/archive/2011/05/26/2059282.html
(defun my-load-theme (&optional theme)
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              `(lambda (frame)
                (with-selected-frame (or frame (selected-frame))
		  (message "-->>>> theme-daemon")
                  (theme-graphic-p ',theme)
		  )))
  (message "-->>>> theme-no-daemon")
  `(theme-graphic-p ',theme)
 ))

;; (my-load-theme 'misterioso)   ;;  wheatgrass \ manoj-dark \ sanityinc-tomorrow-bright \ misterioso

(use-package monokai-theme
  :hook (emacs-startup . (lambda () (my-load-theme 'monokai)))
  )

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



(provide 'init-theme)

;;; init-theme.el ends here
