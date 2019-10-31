;; init-tabbar.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tabbar
(defun awesome-tab-config ()
  (use-package awesome-tab
    :load-path "site-lisp/awesome-tab"
    ;; :commands (awesome-tab-mode)              ;; 与bind关键字,:pre,:post 配套。
    :init
    (defun my-awesome-tab-hide-tab (x)
      (let ((name (format "%s" x)))
    	(or
   	 (string-prefix-p "*NeoTree*" name)
    	 (string-prefix-p "*Ilist*" name)
    	 (string-prefix-p "*epc" name)
    	 (string-prefix-p "*helm" name)
    	 (string-prefix-p "*Compile-Log*" name)
    	 (string-prefix-p "*lsp" name)
    	 (and (string-prefix-p "magit" name)
    	      (not (file-name-extensionname)))
    	 )))
    (setq awesome-tab-hide-tab-function 'my-awesome-tab-hide-tab)

    ;; (with-eval-after-load 'imenu-list
    ;;   (push 'imenu-list-minor-mode-hook awesometab-hide-tabs-hooks))

    ;; (setq awesome-tab-height 12)
    ;; (setq awesome-tab-display-icon nil)

    (set-face-attribute
     'header-line nil
     :box nil)
    (setq awesome-tab-style "alternate")

    (defhydra awesome-fast-switch (:hint nil)
      "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
      ("h" awesome-tab-backward-tab)
      ("j" awesome-tab-forward-group)
      ("k" awesome-tab-backward-group)
      ("l" awesome-tab-forward-tab)
      ("0" my-select-window)
      ("1" my-select-window)
      ("2" my-select-window)
      ("3" my-select-window)
      ("4" my-select-window)
      ("5" my-select-window)
      ("6" my-select-window)
      ("7" my-select-window)
      ("8" my-select-window)
      ("9" my-select-window)
      ("C-a" awesome-tab-select-beg-tab)
      ("C-e" awesome-tab-select-end-tab)
      ("C-j" awesome-tab-ace-jump)
      ("C-h" awesome-tab-move-current-tab-to-left)
      ("C-l" awesome-tab-move-current-tab-to-right)
      ("b" ivy-switch-buffer)
      ("g" awesome-tab-counsel-switch-group)
      ("C-k" kill-current-buffer)
      ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
      ("q" nil "quit"))

    (global-set-key (kbd "M-t") 'awesome-fast-switch/body)
    ;; :bind
    ;; (("M-t" . hydra-tab/body))

    :config            ;; commands\bind 关键字与config下的模式开启是排斥关系。
    (awesome-tab-mode 1)
    ))


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
    :init
    (tabbar-mode 1)
    :config
    ;; C-x [right]	next-buffer	Move to next buffer
    ;; C-x [left]	previous-buffer	Move to previous buffer
    ;; (global-set-key (kbd "C-<left>") 'tabbar-backward)
    ;; (global-set-key (kbd "C-<right>") 'tabbar-forward)

    ;; 设置tabbar底色。
    ;; (customize-set-variable 'tabbar-background-color "#16161D")   ;;#353439   ;; #16161D
    (customize-set-variable 'tabbar-separator '(0.5))      ;; Change padding of the tabs    ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
    (customize-set-variable 'tabbar-use-images nil)        ;; Disable images for a speed-up

    ;; tabbar的box边框是mode-line-inactive的face继承mode-line的face中的box得来的
    ;; 将mode-line-inactive的face的:box设为nil即可取消tanbar的边框box。
    ;; (set-face-attribute 'mode-line-inactive nil            ; 默认情况下，未选择窗口的模式行显示在另一个面上，称为mode-line-inactive(inactive待用的意思)。
    ;; 					; 只有选定的窗口显示在mode-line脸部。
    ;; 			:box nil)

    (set-face-attribute 'header-line nil
			:box nil)

    (set-face-attribute 'tabbar-default nil
			:background "gray20"
			:foreground "gray20"
			:height 1.1
			:box '(:line-width 1 :color "gray20" :style nil))

    (set-face-attribute 'tabbar-unselected nil
			:background "gray30"
			:foreground "#EEDD82"
			:box '(:line-width 1 :color "gray30" :style nil))

    (set-face-attribute 'tabbar-selected nil
			:background "#1c1c1c"
			:foreground "#EEDD82"
			:box '(:line-width 1 :color "#1c1c1c" :style nil))

    (set-face-attribute 'tabbar-modified nil
			:background "blue"
			:foreground "Turquoise1"
			:box '(:line-width 1 :color "gray30" :style nil))

    (set-face-attribute 'tabbar-selected-modified nil
			:background "blue"
			:foreground "Turquoise1"
			:box '(:line-width 1 :color "#1c1c1c" :style nil))


    (set-face-attribute 'tabbar-highlight nil
			:background "#EEDD82"
			:foreground "black"
			:underline nil
			:box '(:line-width 1 :color "#EEDD82" :style nil))

    (set-face-attribute 'tabbar-separator nil
			:background "gray20"
			:height 0.6)

    (set-face-attribute 'tabbar-button nil
			:foreground "#EEDD82"
			:background "gray30"
			:family "Monospace"
			:box '(:line-width 1 :color "gray30" :style nil)
			:inherit 'tabbar-default)

    (set-face-attribute 'tabbar-button-highlight nil
			:foreground "Turquoise1"
			:background "gray30"
			:family "Monospace"
			:box '(:line-width 1 :color "gray30" :style nil)
			:inherit 'tabbar-default)

    ;; adding spaces
    (defun tabbar-buffer-tab-label (tab)
      "Return a label for tab.
That is, a string used to represent it on the tab bar."
      (let ((label  (if tabbar--buffer-show-groups
			(format "[%s]  " (tabbar-tab-tabset tab))
		      (format "%s  " (tabbar-tab-value tab)))))
	;; Unless the tab bar auto scrolls to keep the selected tab
	;; visible, shorten the tab label to keep as many tabs as possible
	;; in the visible area of the tab bar.
	(if tabbar-auto-scroll-flag
	    label
	  (tabbar-shorten
	   label (max 1 (/ (window-width)
			   (length (tabbar-view
				    (tabbar-current-tabset)))))))))

    ))


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
