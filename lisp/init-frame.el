;;; init-frame.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (defun sanityinc/window-system-frame-setup ()
(use-package frame
	:ensure nil
  :config
  (progn
    (defvar tv-default-font (if (string= (invocation-name) "remacs")
                                "-*-DejaVu Sans Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1"
                              ;; Use .Xdefaults config
                              ;; (assoc-default 'font (frame-parameters))
			      "-*-Source Code Pro-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"    ;; 文泉驿等宽微米黑
			      ;; "DejaVu Sans Mono-8"    ;; 文泉驿等宽微米黑
			      ))
    ;; (setq-default frame-background-mode 'dark)
    (setq initial-frame-alist '((fullscreen . maximized)))
    (setq initial-frame-alist '(
    				;; (vertical-scroll-bars . nil)
                                ;; (tool-bar-lines . 0)
                                ;; (menu-bar-lines . 0)
                                ;; (title . ,(format "%s-%s"
                                ;;                   (capitalize (invocation-name))
                                ;;                   emacs-version))
                                ;; (cursor-color . "red")
     				(foreground-color . "Wheat")
                                (background-color . "black")   ;; #181A26
                                (alpha . 90)
                                ;;New frames go in right corner.
                                ;; (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
      				(top . 0)
				(left . 300)
				(width . 108)
				(height . 28)
                                (vertical-scroll-bars . nil)
                                ;; (title . ,(format "%s-%s"
    				;; 			(capitalize (invocation-name))
    				;; 			emacs-version))
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                ;; (font . ,tv-default-font)
                                (cursor-color . "red")
                                (fullscreen . nil)
    				))
    ;; (add-to-list 'initial-frame-alist `(font . ,tv-default-font))
    (setq default-frame-alist initial-frame-alist)      ;;http://ergoemacs.org/emacs/emacs_playing_with_color_theme.html
    ;; (setq frame-title-format " %f")       ;; 标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号


    ;; (setq frame-auto-hide-function 'delete-frame)
    (defun tv-transparency-modify (arg)
      "Increase Emacs frame transparency.
With a prefix arg decrease transparency."
      (interactive "P")
      (when (window-system)
        (let* ((ini-alpha (frame-parameter nil 'alpha))
               (def-alpha (or ini-alpha 80))
               (mod-alpha (if arg
                              (min (+ def-alpha 10) 100)
                            (max (- def-alpha 10)
                                 frame-alpha-lower-limit)))) ; 20
          (modify-frame-parameters nil (list (cons 'alpha mod-alpha)))
          (message "Alpha[%s]" mod-alpha))))

    ;; (if (or (daemonp)
    ;;         (not (window-system))
    ;;         (< emacs-major-version 24))
    ;; 	(progn
    ;; 	  (setq default-frame-alist `(          ;;daemon
    ;; 				      ;; (vertical-scroll-bars . nil)
    ;;                                   ;; (tool-bar-lines . 0)
    ;;                                   ;; (menu-bar-lines . 0)
    ;;                                   ;; (title . ,(format "%s-%s"
    ;;                                   ;;                   (capitalize (invocation-name))
    ;;                                   ;;                   emacs-version))
    ;;                                   ;; (cursor-color . "red")
    ;;  				      ;; (foreground-color . "Wheat")
    ;;                                   (background-color . "black")   ;; #181A26
    ;;                                   (alpha . 90)
    ;;                                   ;;New frames go in right corner.
    ;;                                   (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
    ;;   				      (top . 0) (left . 100) (width . 108) (height . 28)
    ;;                                   (vertical-scroll-bars . nil)
    ;;                                   ;; (title . ,(format "%s-%s"
    ;; 				      ;; 			(capitalize (invocation-name))
    ;; 				      ;; 			emacs-version))
    ;;                                   (tool-bar-lines . 0)
    ;;                                   (menu-bar-lines . 0)
    ;;                                   ;; (font . ,tv-default-font)
    ;;                                   (cursor-color . "red")
    ;;                                   (fullscreen . nil)
    ;; 				      ))
    ;; 	  (add-to-list 'default-frame-alist `(font . ,tv-default-font))
    ;; 	  ;; (setq default-frame-alist initial-frame-alist)      ;;http://ergoemacs.org/emacs/emacs_playing_with_color_theme.html
    ;; 	  )
    ;;   (setq default-frame-alist `(             ;;not daemon
    ;;   				  ;; (foreground-color . "Wheat")
    ;;                               (background-color . "black")
    ;;                               (alpha . 90)
    ;;                               ;;New frames go in right corner.
    ;;                               ;; (left . ,(- (* (window-width) 8) 160)) ; Chars are 8 bits long.
    ;;   				  (top . 0) (left . 100) (width . 108) (height . 28)
    ;;                               (vertical-scroll-bars . nil)
    ;;                               ;; (title . ,(format "%s-%s"
    ;;                               ;;                   (capitalize (invocation-name))
    ;;                               ;;                   emacs-version))
    ;;                               (tool-bar-lines . 0)
    ;;                               (menu-bar-lines . 0)
    ;;                               (font . ,tv-default-font)
    ;;                               (cursor-color . "red")
    ;;                               (fullscreen . nil)
    ;;                               ))
    ;;   )

 
    ;; Special buffer display.
    (add-hook 'window-setup-hook
              (lambda ()
                (setq special-display-regexps `(("\\*Help"
                                                 (minibuffer . nil)
                                                 (width . 80)
                                                 (height . 24)
                                                 (left-fringe . 0)
                                                 (border-width . 0)
                                                 (menu-bar-lines . 0)
                                                 (tool-bar-lines . 0)
                                                 (unsplittable . t)
                                                 (top . 24)
                                                 (left . 450)
                                                 (background-color . "Lightsteelblue1")
                                                 (foreground-color . "black")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                ("\\*Compile-Log"
                                                 (minibuffer . nil)
                                                 (width . 85)
                                                 (height . 24)
                                                 (left-fringe . 0)
                                                 (border-width . 0)
                                                 (menu-bar-lines . 0)
                                                 (tool-bar-lines . 0)
                                                 (unsplittable . t)
                                                 (top . 24)
                                                 (left . 450)
                                                 (background-color . "Brown4")
                                                 (foreground-color . "black")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                ("\\*Dict"
                                                 (minibuffer . nil)
                                                 (width . 80)
                                                 (height . 24)
                                                 (left-fringe . 0)
                                                 (border-width . 0)
                                                 (menu-bar-lines . 0)
                                                 (tool-bar-lines . 0)
                                                 (unsplittable . t)
                                                 (top . 24)
                                                 (left . 450)
                                                 (background-color . "LightSteelBlue")
                                                 (foreground-color . "DarkGoldenrod")
                                                 (alpha . nil)
                                                 (fullscreen . nil))
                                                ))))
    )
  :bind ("C-8" . tv-transparency-modify))
;; )
;; (add-hook 'after-make-window-system-frame-hooks 'sanityinc/window-system-frame-setup)

;; 应用实例https://github.com/purcell/emacs.d/blob/master/lisp/init-xterm.el
;; (defun sanityinc/console-frame-setup ()
;;   (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
;;   (mwheel-install))
;; (add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)


(provide 'init-frame)

;;; init-frame.el ends here
