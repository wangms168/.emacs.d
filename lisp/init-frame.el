;;; init-frame.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;  https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init.el
(use-package frame
  :ensure nil
  :config
  (progn
    (defvar tv-default-font (if (string= (invocation-name) "remacs")
				;; "-*-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"   ;; C-q C-l 分页符有问题
				"-PfEd-DejaVuSansMono Nerd Font-normal-normal-normal-*-11-*-*-*-*-0-iso10646-1"
                              ;; Use .Xdefaults config
                              ;; (assoc-default 'font (frame-parameters))
			      ;; "-*-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
			      "-PfEd-DejaVuSansMono Nerd Font-normal-normal-normal-*-11-*-*-*-*-0-iso10646-1"    ;; 文泉驿等宽微米黑
                              ))
    ;; (setq-default frame-background-mode 'dark)
    (setq frame-auto-hide-function 'delete-frame)

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

    ;;set transparent effect
    (global-set-key [(f9)] 'loop-alpha)
    (setq alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
    (defun loop-alpha ()
      (interactive)
      (let ((h (car alpha-list)))                ;; head value will set to
	((lambda (a ab)
	   (set-frame-parameter (selected-frame) 'alpha (list a ab))
	   (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
	   ) (car h) (car (cdr h)))
	(setq alpha-list (cdr (append alpha-list (list h))))
	)
      )

    ;;标题栏 https://www.emacswiki.org/emacs/FrameTitle
    (setq frame-title-format
    	  '("%b"
    	    (:eval (when (and (buffer-file-name) (buffer-modified-p)) " **") )
    	    (:eval (if (buffer-file-name)
    	    	       (concat " {"
    	    		       (directory-file-name
    	    			(file-name-directory
    	    			 (abbreviate-file-name
    	    			  (buffer-file-name))))"}") )
    	    	   )
    	    " - Emacs"))

    (setq initial-frame-alist `(
				;; (foreground-color . "Wheat")
				;; (background-color . "black")   ;; #181A26
				;; (left . ,(- (* (window-width) 8) 300)) ; Chars are 8 bits long.
				;; (width . 108)
				(alpha . 100)
				;; New frames go in right corner.
				(vertical-scroll-bars . nil)
                                ;; (title . ,(format "%s-%s"
				;; 		  (capitalize (invocation-name))
				;; 		  emacs-version))
				(tool-bar-lines . 0)
				(menu-bar-lines . 0)
				(font . ,tv-default-font)
				(cursor-color . "red")
				(fullscreen . fullscreen)     ;;fullscreen 全屏\maximized 最大化
				))

    ;; (add-to-list 'initial-frame-alist `(font . ,tv-default-font))
    (setq default-frame-alist initial-frame-alist)      ;;http://ergoemacs.org/emacs/emacs_playing_with_color_theme.html

    ;; Special buffer display.
    (add-hook 'window-setup-hook
              (lambda ()
                (setq special-display-regexps `(
						("\\*Help"
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
                                                )))))
  :bind ("C-8" . loop-alpha))

;; 应用实例https://github.com/purcell/emacs.d/blob/master/lisp/init-xterm.el
;; (defun sanityinc/console-frame-setup ()
;;   (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
;;   (mwheel-install))
;; (add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

;;(add-hook 'before-save-hook 'whitespace-cleanup nil (not global)))
;;    (`trailing
;;     (add-hook 'before-save-hook 'delete-trailing-whitespace nil (not global)))

(provide 'init-frame)

;;; init-frame.el ends here
