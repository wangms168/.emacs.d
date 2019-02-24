;;; init.el --- Initialize configurations.	-*- lexical-binding: t no-byte-compile: t; -*-

;; URL: https://github.com/seagle0128/.emacs.d

;;; Commentary:
;;
;;

;;; Code:

;; (let ((table (make-display-table)))  (aset table ?\^L [?📄])  (setq buffer-display-table table))
;; 

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;;----------------------------------------------------------------------------
;; Speed up startup
;;----------------------------------------------------------------------------
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))))


;;----------------------------------------------------------------------------
;; load-path
;;----------------------------------------------------------------------------
;; (dolist (i '("lisp"
;; 	     "site-lisp"
;; 	     ))
;;   ;; Add all at end of `load-path' to avoid conflicts.
;;   (add-to-list 'load-path (expand-file-name i user-emacs-directory) t)) ;; 参数t是Add all at end of `load-path'

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)      ;; push是加到`load-path'的前面。
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-const)
(require 'init-custom)
(require 'init-package)
(require 'init-basic)
(require 'init-funcs)

(require 'init-frame-hooks)
(require 'init-frame)
(require 'init-icons)
(require 'init-modeline)
;; (require 'init-test-modeline)
;; (require 'chunhui-modeline)
;; (require 'init-modeline-icons)
;; (require 'gnus-bindings)
;; (require 'core-modeline)
;; (require 'sml-modeline)
;; (require 'maple-modeline)
;; (require 'init-tv-powerline)
(require 'init-theme)  ;;主题
(require 'init-face)
(require 'init-visual) ;;自动补全括号等

(require 'doremi-frm)	;; 使用库doremi-frm.el(依赖库doremi.el、hexrgb.el、frame-fns.el、faces+.el)中doremi-font+命令, 循环查看可用字体及其效果.
(progn (require 'cursor-change) (cursor-change-mode 1)) ;;智能光标形状

(require 'init-ivy)
(require 'init-key)
(require 'init-complete)
(require 'init-neotree)
(require 'init-tabbar)

;; (require 'init-awesome-pair)
;; (require 'init-undo-tree)

(require 'init-editor) ;;自动补全括号等


;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")
;;----------------------------------------------------------------------------
;; emacs-backup-config
;;----------------------------------------------------------------------------
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 100
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)

;; Disable backup files  注意，如有下面变量为nil，将使上面的emacs_backup失去作用！
;; (setq make-backup-files nil) ; stop creating backup~ filess
;; (setq auto-save-default nil) ; stop creating #autosave# files

;; ;;----------------------------------------------------------------------------
;; ;; Variables configured via the interactive 'customize' interface
;; ;;----------------------------------------------------------------------------
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (add-hook 'after-init-hook (lambda ()
;;                              (unless (or (daemonp) (server-running-p))
;;                                (server-start)
;;                                (setq server-raise-frame t))))

(defun my-fontset-menu ()
  (interactive)
  (x-popup-menu
   `((0 0) ,(selected-frame))
   (append x-fixed-font-alist
           (list (generate-fontset-menu)))))


(provide 'init)

;;; init.el ends here
;; (put 'narrow-to-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
