;;; init-tabbar.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tabbar
(use-package tabbar
  :ensure t
  :config
  (progn
    (setq tabbar-background-color "#333333")
    (setq tabbar-separator '(0.5))
    (set-face-attribute
     'mode-line-inactive nil
     :box nil)
    (set-face-attribute
     'tabbar-default nil
     :height 1.0)
    (set-face-attribute
     'tabbar-highlight nil
     :foreground "black"
     :background "orange"
     :underline nil
     :box nil)
    (set-face-attribute
     'tabbar-button nil
     :box nil)
    (set-face-attribute
     'tabbar-unselected nil
     :foreground "black"
     :box '(:line-width 1 :color "white" :style sunken))
    (set-face-attribute
     'tabbar-selected nil
     :background "#BFBFBF"
     :foreground "black"
     :box '(:line-width 1 :color "white" :style sunken))
    (set-face-attribute
     'tabbar-separator nil
     :background "#1F1E1F"
     :box nil)
    (tabbar-mode)
    ))

;; (use-package awesome-tab
;;   :load-path "~/home/wangms/.emacs.d/default/site-lisp/awesome-tab/"
;;   ;; :straight (awesome-tab
;;   ;; 	     :type git
;;   ;; 	     :host github
;;   ;; 	     :repo "manateelazycat/awesome-tab")
;;   :config
;;   (setq awesome-tab-style 'alternate)
;;   (awesome-tab-mode t))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
