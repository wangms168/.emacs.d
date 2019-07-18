;;; init-tabbar.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tabbar

(defun tabbar-ruler-seting ()
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
     '(tabbar-key-binding ((t (:background "gray12" :foreground "gainsboro" ))))
     '(tabbar-default ((t (:background "gray12" :foreground "gainsboro" :height 1.0 :weight bold))))  ;;:weight bold
     '(tabbar-button ((t (:inherit tabbar-default :foreground "white" :style released-button))))
     '(tabbar-button-highlight ((t (:inherit tabbar-default))))
     '(tabbar-highlight ((t (:underline t))))
     '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59" :weight bold :line-width 7 :height 25))))
     '(tabbar-selected ((t (:inherit tabbar-default :background "black" :foreground "orange"))))
     '(tabbar-unselected ((t (:inherit tabbar-default))))
     )
    )
  )

(defun tabbar-seting ()
  (use-package tabbar
    :ensure t
    :config
    (global-set-key (kbd "C-<left>") 'tabbar-backward)
    (global-set-key (kbd "C-<right>") 'tabbar-forward)
    ;; 设置tabbar底色。
    ;; tabbar的box边框是mode-line-inactive的face继承mode-line的face中的box得来的
    ;; 将mode-line-inactive的face的:box设为nil即可取消tanbar的边框box。
    (setq tabbar-background-color "#333333")
    (set-face-attribute
     'mode-line-inactive nil
     :box nil)

    ;; 使分隔底色同tabbar底色
    (setq tabbar-separator '(0.2))
    (set-face-attribute
     'tabbar-separator nil
     :background "#333333"
     :box nil)

    ;; 使字体粗园
    (set-face-attribute
     'tabbar-default nil
     :height 1.0)

    (set-face-attribute
     'tabbar-button nil
     :box nil)

    (set-face-attribute
     'tabbar-highlight nil
     :foreground "black"
     :background "orange"
     :underline nil
     :box nil)

    (set-face-attribute
     'tabbar-unselected nil
     :foreground "black"
     :background nil
     :box '(:line-width 1 :color "white" :style sunken)
     )

    (set-face-attribute
     'tabbar-selected nil
     ;; :foreground "block"
     :background "#BFBFBF"
     :box '(:line-width 1 :color "white" :style sunken)
     )

    (set-face-attribute
     'tabbar-modified nil
     :foreground "orange red"
     :background "gray25"
     :box '(:line-width 1 :color "gray19"))

    (set-face-attribute
     'tabbar-selected-modified nil
     :foreground "orange red"
     :background "gray19"
     :box '(:line-width 1 :color "gray19"))

    :init
    (tabbar-mode 1)
    ))

(defun tabbar-graphic-p ()
  (if (display-graphic-p)
      (progn
	(message "--> tabbar-graphic")
	(tabbar-ruler-seting)
	)
    (progn
      (message "--> tabbar-terminal")
      (tabbar-seting)
      )
    ))

;; https://emacs-china.org/t/topic/5387/15
;; https://www.cnblogs.com/darwin/archive/2011/05/26/2059282.html
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame (or frame (selected-frame))
		  (message "--> tabbar-daemon")
                  (tabbar-graphic-p)
		  )))
  (message "--> tabbar-no-daemon")
  (tabbar-graphic-p)
  )

;; (demacro daemon-p (&rest body)
;;   (if (and (fboundp 'daemonp) (daemonp))
;;       (add-hook 'after-make-frame-functions
;; 		`(lambda (frame)
;; 		   (with-selected-frame (or frame (selected-frame))
;; 		     (message "-->tabbar-daemon")
;; 		     ',body
;; 		     )))
;;     (progn
;;       (message "--> tabbar-no-daemon")
;;       body
;;       )
;;     ))
;; (daemon-p (tabbar-graphic-p))

;; https://emacs.stackexchange.com/questions/16464/emacs-server-init-when-called-without-file
;; https://emacs.stackexchange.com/questions/46541/running-emacs-as-a-daemon-does-not-load-custom-set-faces
;; (defun my-frame-tweaks (&optional frame)
;;   "Make frame- and/or terminal-local changes."
;;   (with-selected-frame (or frame (selected-frame))
;;     (my-display-graphic-p)
;;     ))
;; ;; For the case that the init file runs after the frame has been created
;; ;; Call of emacs without --daemon option.
;; (my-frame-tweaks)
;; ;; For the case that the init file runs before the frame is created.
;; ;; Call of emacs with --daemon option.
;; (add-hook 'after-make-frame-functions 'my-frame-tweaks t)


;; (defun tabbar/console-frame-setup ()
;;   (message "wangms-terminal")
;;   (tabbar-seting))
;; (add-hook 'after-make-console-frame-hooks 'tabbar/console-frame-setup)

;; (defun tabbar/window-system-frame-setup ()
;;   (message "wangms-graphic")
;;   (tabbar-ruler-seting))
;; (add-hook 'after-make-window-system-frame-hooks 'tabbar/window-system-frame-setup)

;; http://tigersoldier.is-programmer.com/2010/2/5/tips-on-emacs-daemon.15404.html
(setq window-system-default-frame-alist
      '(
        ;; if frame created on x display
        (x
	 (message "x display")
	 )
        ;; if on term
        (nil
	 (message "terminal")
	 )
	))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
