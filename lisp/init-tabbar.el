;; init-tabbar.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Tab Bar
;; https://amitp.blogspot.com/2018/10/emacs-prettier-tabbar.html
;; (require 'tabbar)
;; (customize-set-variable 'tabbar-background-color "gray20")
;; (customize-set-variable 'tabbar-separator '(0.5))
;; (customize-set-variable 'tabbar-use-images nil)
;; (tabbar-mode 1)

;; ;; My preferred keys
;; (define-key global-map [(alt j)] 'tabbar-backward)
;; (define-key global-map [(alt k)] 'tabbar-forward)

;; ;; Colors
;; (set-face-attribute 'tabbar-default nil
;; 		    :background "gray20" :foreground "gray60"
;; 		    :distant-foreground "gray50"
;; 		    :family "Helvetica Neue"
;; 		    :box nil
;; 		    :height 1.2
;; 		    ;; 使字体粗园
;; 		    )
;; (set-face-attribute 'tabbar-unselected nil
;; 		    :background "gray80" :foreground "black" :box nil)
;; (set-face-attribute 'tabbar-modified nil
;; 		    :foreground "red4" :box nil
;; 		    :inherit 'tabbar-unselected)
;; (set-face-attribute 'tabbar-selected nil
;; 		    :background "#4090c0" :foreground "white" :box nil)
;; (set-face-attribute 'tabbar-selected-modified nil
;; 		    :inherit 'tabbar-selected :foreground "GoldenRod2" :box nil)
;; (set-face-attribute 'tabbar-button nil
;; 		    :box nil)

;; Use Powerline to make tabs look nicer
;; (this needs to run *after* the colors are set)
;; (require 'powerline)
;; (defvar my/tabbar-height 20)
;; (defvar my/tabbar-left (powerline-wave-right 'tabbar-default nil my/tabbar-height))
;; (defvar my/tabbar-right (powerline-wave-left nil 'tabbar-default my/tabbar-height))
;; (defun my/tabbar-tab-label-function (tab)
;;   (powerline-render (list my/tabbar-left
;;                           (format " %s  " (car tab))
;;                           my/tabbar-right)))
;; (setq tabbar-tab-label-function #'my/tabbar-tab-label-function)


;; tabbar
(defun awesome-tab-config ()
  (use-package awesome-tab
    :ensure nil
    :load-path "site-lisp/awesome-tab"

    ;; :commands (hydra-tab/body)                 ;; 配合hydra的:pre、:post开启。
    ;; :functions (hydra-tab)

    :init
    (set-face-attribute
     'header-line nil
     :box nil)
    (setq awesome-tab-style "alternate")

    (defhydra hydra-tab (:color pink :hint nil
				;; :pre (awesome-tab-mode t)
				;; :post (awesome-tab-mode -1)
				)
      "
   ^^Tab                   Group^^               Other^^                  Search
  -^^^^--------------------+-^^------------------+-^^---------------------+-^^----------------
    ^_H_^    move to left  | _p_   prev group    | _d_   kill buffer      | _b_ search buffer
  _h_   _l_  switch tab    | _n_   next group    | _K_   kill-all-buffers | _g_ search group
   ^ _L_^    move to right | _s_   switch group  | _C-h_ backward window  | ^^
  ^^0 ~ 9^^  select window | ^^                  | _C-l_ forward window   | ^^
  -^^^^--------------------+-^^------------------+-^^---------------------+-^^----------------
"
      ;; Tab
      ("h" awesome-tab-backward-tab)
      ("l" awesome-tab-forward-tab)
      ("H" awesome-tab-move-current-tab-to-left)
      ("L" awesome-tab-move-current-tab-to-right)
      ;; Group
      ("p" awesome-tab-backward-group)
      ("n" awesome-tab-forward-group)
      ("s" awesome-tab-switch-group)
      ;; Other
      ("d" spacemacs/kill-this-buffer)
      ("K" awesome-tab-kill-all-buffers-in-current-group)
      ;; ("b" awesome-tab-select-beg-tab)
      ;; ("e" awesome-tab-select-end-tab)
      ("C-h" awesome-tab-backward-tab-other-window)
      ("C-l" awesome-tab-forward-tab-other-window)
      ;; Search
      ("b" ivy-switch-buffer)
      ("g" awesome-tab-counsel-switch-group)
      q      ("q" nil "quit"))
    (global-set-key (kbd "M-t") 'hydra-tab/body)

    :config
    (awesome-tab-mode t)
    ))

;; (defun awesome-tab-config ()
;;   (use-package awesome-tab
;;     :ensure nil
;;     :load-path"site-lisp/awesome-tab"
;;     :init
;;     (setq awesome-tab-style "alternate")
;;     (global-set-key (kbd "C-<left>") 'awesome-tab-backward-tab)
;;     (global-set-key (kbd "C-<right>") 'awesome-tab-forward-tab)
;;     :config
;;     (awesome-tab-mode t)
;;     ))

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


(defun tabbar-config ()
  (use-package tabbar
    :ensure t
    :config
    (global-set-key (kbd "C-<left>") 'tabbar-backward)
    (global-set-key (kbd "C-<right>") 'tabbar-forward)
    ;; 设置tabbar底色。
    ;; tabbar的box边框是mode-line-inactive的face继承mode-line的face中的box得来的
    ;; 将mode-line-inactive的face的:box设为nil即可取消tanbar的边框box。
    (setq tabbar-background-color "gray12")
    ;; (set-face-attribute
    ;;  'mode-line-inactive nil            ; 默认情况下，未选择窗口的模式行显示在另一个面上，称为mode-line-inactive(inactive待用的意思)。
    ;; 					; 只有选定的窗口显示在mode-line脸部。
    ;;  :box nil)

    (set-face-attribute
     'header-line nil
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
     :background "#444444"
     :box nil)

    (set-face-attribute
     'tabbar-highlight nil
     :foreground "black"
     :background "orange"
     :underline nil
     :box nil)

    (set-face-attribute
     'tabbar-unselected nil
     ;; :foreground "white"
     :background "#444444"
     :box nil)
    ;; :box '(:line-width 1 :color "white" :style sunken)

    (set-face-attribute
     'tabbar-selected nil
     :foreground "white"
     :background "#242424"
     :box nil)
    ;; :box '(:line-width 1 :color "white" :style sunken)

    ;; (set-face-attribute
    ;;  'tabbar-modified nil
    ;;  :foreground "orange red"
    ;;  :background "gray25"
    ;;  :box '(:line-width 1 :color "gray19"))

    ;; (set-face-attribute
    ;;  'tabbar-selected-modified nil
    ;;  :foreground "orange red"
    ;;  :background "gray19"
    ;;  :box '(:line-width 1 :color "gray19"))

    :init
    (tabbar-mode 1)))


(defun tabbar-graphic-p ()
  (if (display-graphic-p)
      (progn
	(message "--> tabbar-graphic")
	(tabbar-ruler-config))
    (progn
      (message "--> tabbar-terminal")
      ;; (tabbar-ruler-config)
      (tabbar-config))))

;; https://emacs-china.org/t/topic/5387/15
;; https://www.cnblogs.com/darwin/archive/2011/05/26/2059282.html
;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame  (or frame (selected-frame))
;;   		  (message "--> tabbar-daemon")
;;   		  ;; (tabbar-graphic-p)
;;   		  (tabbar-ruler-config)
;;   		  )))
;;   (progn
;;     (message "--> tabbar-no-daemon")
;;     ;; (tabbar-graphic-p)
;;     (tabbar-ruler-config)
;;     ))

;; (add-hook 'find-file-hook 'tabbar-config)
(tabbar-config)
;; (tabbar-ruler-config)
;; (awesome-tab-config)


(defun tabbar/console-frame-setup ()
  (message "--> tabbar-terminal")
  ;; (tabbar-config)
  )
(add-hook 'after-make-console-frame-hooks 'tabbar/console-frame-setup)

(defun tabbar/window-system-frame-setup ()
  (message "--> tabbar-graphic")
  ;; (tabbar-ruler-config)
  )
(add-hook 'after-make-window-system-frame-hooks 'tabbar/window-system-frame-setup)


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

;; http://tigersoldier.is-programmer.com/2010/2/5/tips-on-emacs-daemon.15404.html
;; (setq window-system-default-frame-alist
;;       '(
;;         ;; if frame created on x display
;;         (x
;; 	 (message "x display")
;; 	 ;; face
;; 	 )
;;         ;; if on term
;;         (nil
;; 	 (message "terminal")
;; 	 )
;; 	)
;;       )



(provide 'init-tabbar)

;;; init-tabbar.el ends here
