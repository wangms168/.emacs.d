;; (custom-set-faces
;;  '(default ((t (:family "文泉驿等宽微米黑" :foundry "WQYF" :slant normal :weight normal :height 83 :width normal)))))
 
;; (set-face-attribute 'default nil :family "文泉驿等宽微米黑" :foundry "WQYF" :slant 'normal :weight 'normal :height 83 :width 'normal)

;; (setq default-frame-alist '((font . "-*-文泉驿等宽微米黑-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")))

;; (eval-after-load (load-theme 'afternoon t)             ;;color-theme\ 'zenburn  
  ;; '(progn
     ;; (set-face-attribute 'font-lock-comment-face nil :foreground "#3f7f5f")
     ;; (set-face-attribute 'font-lock-string-face nil :foreground "#4f004f")
     ;; (set-face-attribute 'font-lock-constant-face nil :foreground "#4f004f")
     ;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#00003f")
     ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#00003f")
     ;; (set-face-attribute 'font-lock-type-face nil :foreground "#000000")
     ;; (set-face-attribute 'font-lock-function-name-face nil
     ;; 			 :foreground "#000000" :weight 'bold)
     ;; (set-face-attribute 'font-lock-variable-name-face nil
     ;; 			 :foreground "#000000" :weight 'bold)
     
     ;; 语法高亮显示，区域选择，二次选择 ;;前景和背景色
     ;; (set-face-foreground 'highlight "white")
     ;; (set-face-background 'highlight "blue")
     ;; (set-face-foreground 'region "cyan")
     ;; (set-face-background 'region "blue")
     ;; (set-face-foreground 'secondary-selection "skyblue")
     ;; (set-face-background 'secondary-selection "#000000")
     (set-face-attribute 'secondary-selection nil :background "#2A2A2A" :foreground nil)    ;"#100a14"\#6B6A6C
     ;; (custom-set-faces '(secondary-selection ((t (:background "#000000" :foreground "grey70")))))  ;;这种方式设置不起作用

;; (set-face-attribute 'mode-line nil :family "DejaVu Sans Mono" :height 80 :width 'normal :box '(:line-width 1 :color "white" :style none) :foreground "white" :background "#2A2A2A")
;; (set-face-attribute 'mode-line nil :family "DejaVu Sans Mono" :width 'normal :box '(:line-width 1 :color "white" :style none) :foreground "white" :background "#2A2A2A")
;; (set-face-attribute 'mode-line nil :family "DejaVu Sans Mono" :width 'normal :box nil :foreground "white" :background "#2A2A2A")
(set-face-attribute 'mode-line nil :family "DejaVu Sans Mono" :height 88 :width 'normal :box nil :foreground "white" :background "#2A2A2A")

     ;; 要将注释设置为绿色和粗体：
     ;; (set-face-attribute 'font-lock-comment-face nil
     ;; 		    :foreground "Green"
     ;; 		    ;; :weight 'bold
     ;; 		    )
     ;; ))


(provide 'init-face)

;;; init-face.el ends here
