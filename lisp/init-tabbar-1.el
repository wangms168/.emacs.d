;; init-tabbar.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tabbar
(defun tabbar-ruler-config ()
  (use-package tabbar-ruler
    :ensure t
    :init
    (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; this is the problem line
    (setq tabbar-ruler-global-tabbar t)    ; get tabbar
    ;; (setq tabbar-ruler-global-ruler t)     ; get global ruler
    (setq tabbar-ruler-popup-menu t)       ; get popup menu.
    (setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
    (setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
    ;; (global-set-key (kbd "C-c t") 'tabbar-ruler-move)
    (global-set-key (kbd "C-<left>") 'tabbar-ruler-backward)
    (global-set-key (kbd "C-<right>") 'tabbar-ruler-forward)
    (custom-set-faces
     '(tabbar-key-binding ((t (:background "gray12" :foreground "gainsboro"))))
     '(tabbar-default ((t (:background "gray12" :height 1.0 :weight bold))))  ;;:weight bold
     '(tabbar-button ((t (:box nil :style released-button))))
     '(tabbar-highlight ((t (:underline t  :foreground "black" :background "orange" :box nil))))
     '(tabbar-button-highlight ((t (:inherit tabbar-highlight))))
     '(tabbar-selected ((t (:background "#242424" :box nil))))
     '(tabbar-unselected ((t (:background "#444444" :box nil))))
     '(tabbar-separator ((t (:background "white" :box nil)))))))


(tabbar-ruler-config)

(provide 'init-tabbar-1)

;;; init-tabbar.el ends here
