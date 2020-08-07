;;; init.el --- personal emacs config file -*- lexical-binding: t -*-
;; make this file lexically bound 上面是使这个文件在词汇上绑定

;; -*- eval: (setq before-save-hook (lambda() (delete-trailing-whitespace))); -*-
;; 启用词法作用域. lexical scope（词法作用域）指局部变量只能作用在函数中和一个块里（block）

;; Copyright (C) 1999-2010 Fabrice Niessen
;; Time-stamp: <2010-07-09 Fri 11:40 sva on mundaneum>

;; Author: Fabrice Niessen <(concat "fni" at-symbol "mygooglest.com")>
;; Keywords: emacs, dotfile, config

;; $Revision: 4145 $
;; $Date: 2010-07-08 15:13:00 +0200 (Thu, 08 Jul 2010) $

;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; emacs下载地址：http://mirrors.ustc.edu.cn/gnu/emacs/windows/
;; http://ftp.gnu.org/gnu/emacs/windows/

;; use-package的:after 关键字的作用基本跟 with-eval-after-load 的作用是相同的：是加载:after值的包后、require请求use-package作用的包。
;; https://samrayleung.github.io/blog/2017/02/22/%E6%8F%90%E9%AB%98emacs%E5%90%AF%E5%8A%A8%E9%80%9F%E5%BA%A6/
;; use-package的:config 关键字的作用也基本跟 with-eval-after-load 的作用是相同的：是加载use-package作用的包后、执行config里的代码。
;; https://github.com/jwiegley/use-package/issues/453

;;; Code:
;;* Prerequisites

(message "* --[ Loading my Emacs init file ]--")

;;----------------------------------------------------------------------------
;; Speed up startup
;;----------------------------------------------------------------------------
;; (defvar default-file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)       ;;在启动过程中加载el和elc文件默认都会将文件名和正则表达式进行匹配. C-M e \ C-M a 跳转配对括号
;; (setq gc-cons-threshold 80000000)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             "Restore defalut values after init."
;;             (setq file-name-handler-alist default-file-name-handler-alist)
;;             (setq gc-cons-threshold 800000)
;;             (if (boundp 'after-focus-change-function)
;;                 (add-function :after after-focus-change-function
;;                               (lambda ()
;;                                 (unless (frame-focus-state)
;;                                   (garbage-collect))))
;;               (add-hook 'focus-out-hook 'garbage-collect))))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter 将垃圾收集阈值增加到500 MB以便于启动，启动后再将阈值降低到5 MB。
;; https://samrayleung.github.io/blog/2017/02/22/%E6%8F%90%E9%AB%98emacs%E5%90%AF%E5%8A%A8%E9%80%9F%E5%BA%A6/
;;----------------------------------------------------------------------------
;; 1MB=1024*1024
(let ((normal-gc-cons-threshold (* 20 1024 1024))       ;; 默认情况下，Emacs将每分配0.76 MB（gc-cons-threshold == 800000）启动GC。  (* 5 1024 1024)这个值垃圾回收6次 / 400000这个值垃圾回收10次 / 32位40万、64位80万
      (init-gc-cons-threshold (* 128 1024 1024)))       ;; 如果将其增加到20 MB（gc-cons-threshold== 20000000） https://github.com/lewang/flx 。 (* 500 1024 1024) / (* 128 1024 1024) / 100000000 / most-positive-fixnum
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(load-theme 'adwaita t)

;; TODO Specify variables using `defcustom'

;;----------------------------------------------------------------------------
;; load-path
;;----------------------------------------------------------------------------
(load-file (concat user-emacs-directory "lisp/init-paths.el"))

;;----------------------------------------------------------------------------
;; with-eval-after-load (after-load)
;;----------------------------------------------------------------------------
;; https://github.com/purcell/emacs.d/blob/a97dc5a44242f7f78c70335a9532bc657ea0a8d8/lisp/init-utils.el
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Features
;;----------------------------------------------------------------------------
;;*** Features
;;https://lists.gnu.org/archive/html/emacs-orgmode/2011-05/msg00466.html
;; redefine require to leave a trace of packages being loaded
;;(if (not (fboundp 'orig-require))
;;    (fset 'orig-require (symbol-function 'require))
;;  (message "The code to redefine `require' should not be loaded
;;twice"))
;;(defvar my/require-depth 0)
;;(defun require (feature &optional filename noerror)
;;  "Leave a trace of packages being loaded."
;;  (cond ((member feature features)
;;	 (message "%sRequiring `%s' (already loaded)"
;;		  (concat (make-string (* 2 my/require-depth) ? )
;;			  "+-> ")
;;		  feature))
;;	(t
;;	 (message "%sRequiring `%s'"
;;		  (concat (make-string (* 2 my/require-depth) ? )
;;			  "+-> ")
;;		  feature)
;;	 (let ((my/require-depth (+ 1 my/require-depth)))
;;	   (orig-require feature filename noerror))
;;	 (message "%sRequiring `%s'...done"
;;		  (concat (make-string (* 2 my/require-depth) ? )
;;			  "+-> ")
;;		  feature))))
;;;;require函数经过这里改造后，在ivy的M-x时报“Symbol’s value as variable is void: amx-initialized”错误。
;;;;于是初始化后要将require的定义恢复为原始：
;;(add-hook 'after-init-hook
;;	  (lambda () (fset 'require 'orig-require )))

;;(defvar missing-packages-list nil
;;  "List of packages that `try-require' can't find.")
;;
;;;; attempt to load a feature/library, failing silently
;;(defun try-require (feature)
;;  "Attempt to load a library or module. Return true if the
;;      library given as argument is successfully loaded. If not, instead
;;      of an error, just add the package to a list of missing packages."
;;  (condition-case err
;;      ;; protected form
;;      (progn
;;	(message "Checking for library `%s'..." feature)
;;	(if (stringp feature)
;;	    (load-library feature)
;;	  (require feature))
;;	(message "Checking for library `%s'... Found" feature))
;;    ;; error handler
;;    (file-error  ; condition
;;     (progn
;;       (message "Checking for library `%s'... Missing" feature)
;;       (add-to-list 'missing-packages-list feature 'append))
;;     nil)))

;; https://www.cnblogs.com/yangyingchao/p/3418630.html
;; Function to collect information of packages.
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defvar package-init-statistic nil "Package loading statistics")

;; attempt to load a feature/library, failing silently
(defun try-require (feature &optional click)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (let ((timestamp (current-time))
            (package (if (stringp feature) feature (symbol-name feature))))
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (if click
            (add-to-list 'package-init-statistic
                         (cons (if (stringp feature) feature (symbol-name feature))
                               (float-time (time-since timestamp)))))
        (message "Checking for library `%s'... Found, cost %.2f seconds"
                 feature (float-time (time-since timestamp))))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

;;(defadvice find-file (around my-find-file activate)
;;  "Open FILENAME and report time spent."
;;  (let* ((my-filename (ad-get-arg 0))
;;         (find-file-time-start (float-time)))
;;    (message (concat "| find-file | start |" my-filename " | ___ |"))
;;    ad-do-it
;;    (message "| find-file | stop | %s | %.1f |"
;;             my-filename
;;             (- (float-time) find-file-time-start))))

;; https://github.com/sjbalaji/myCustomizations/blob/master/ReferenceEmacsConfig
;; make loaded files give a message
;;(defadvice load (before debug-log activate)
;;  (message "wangms-Loading %s..." (locate-library (ad-get-arg 0))))

(defun require-extensions (action lst &optional click)
  ""
  (mapcar (lambda(ext) "" (funcall action ext click)) lst))
;; 有了这个函数，我们就可以进行非常简单的工作了删掉 require ，换成 list 。

;;----------------------------------------------------------------------------
;; cl
;;----------------------------------------------------------------------------
;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'

;; Load all configuration and packages.
(let ((ts-init (current-time)))
  (setq missing-packages-list nil
        package-init-statistic nil)

  (require-extensions 'try-require
		      '(
			init-const
			init-custom
			init-package
			init-frame
			init-basic
			init-funcs
			init-edit
			init-edit-visual
			init-frame-hooks
			init-icons
			;;------------------------------
			;; init-test-modeline
			;; chunhui-modeline
			;; init-modeline-icons
			;; gnus-bindings
			;; core-modeline
			;; sml-modeline
			;; maple-modeline
			;; init-tv-powerline
			;;-----------------------------
			init-modeline
			init-theme
			;; init-xresources-theme
			;; init-hide-modeline
			init-face

			;; doremi-frm	       ;; 使用库doremi-frm.el(依赖库doremi.el、hexrgb.el、frame-fns.el、faces+.el)中doremi-font+命令, 循环查看可用字体及其效果.

			init-ivy
			init-projectile
			init-key
			;; init-complete
			init-company
;;			init-hydra             ;;影响ivy的icons
			init-neotree
			init-tabbar
			init-sidebar
			init-slime
			;; init-treemacs
			init-imenu-list
			init-avyace
			init-dumb-jump
			init-aggressive-indent
			init-stardict
			;; init-ggtags
			;; init-xcscope
			;; init-lsp                          ;; misss
			) t)


  ;; https://www.cnblogs.com/yangyingchao/p/3418630.html
  (message "\n{\nShowing package initialization statistics:\n%s"
	   (mapconcat (lambda (x)
			(format "package %s cost %.2f seconds" (car x) (cdr x)))
		      (reverse package-init-statistic)
		      "\n"
		      ))
  (message "Finished startup in %.2f seconds,  %d packages missing%s\n\n"
	   (float-time (time-since ts-init)) (length missing-packages-list)
	   (if missing-packages-list
	       ". Refer to `missing-packages-list` for missing packages.\n}"
	     ".\n}")))


;;----------------------------------------------------------------------------
;; display time
;;----------------------------------------------------------------------------

;; 自定义"For information about GNU Emacs and the GNU system, type C-h C-a."这个消息。
;; (defun display-startup-echo-area-message ()
;;   "Display startup echo area message."
;;   (message "*** --【 Initialized in %s 】-- ***" (emacs-init-time))
;;   )

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** --【 Emacs ready in %s with %d garbage collections. 】-- ***"
                     (emacs-init-time)
                     gcs-done)))

;; (add-hook 'after-init-hook (lambda () (message (format "after/before-init-time = %s" (float-time (time-subtract after-init-time before-init-time))))))
(add-hook 'after-init-hook (lambda () (message  (format "emacs-init-time = %s" (emacs-init-time)))))

;;----------------------------------------------------------------------------
;; other
;;----------------------------------------------------------------------------

;; (defun print-elements-of-list (list)
;;   "Print each element of LIST on a line of its own."
;;   (interactive "vList: ")
;;   (while list
;;     (print (car list))
;;     (setq list (cdr list))))

;; (print-elements-of-list load-path)

;; (defun describe-variable-short (var)
;;   (interactive "vVariable: ")
;;   (message (format "%s: %s" (symbol-name var) (symbol-value var))) )
;; (global-set-key "\C-hV" 'describe-variable-short)



;;; init.el ends here
;; (put 'narrow-to-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
