;;; .emacs --- my Emacs Init File   	-*- lexical-binding: t no-byte-compile: t; -*-

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

;; 下载地址：http://mirrors.ustc.edu.cn/gnu/emacs/windows/
;; http://ftp.gnu.org/gnu/emacs/windows/

;; use-package的:after 关键字的作用基本跟 with-eval-after-load 的作用是相同的：是加载:after值的包后、require请求use-package作用的包。
;; https://samrayleung.github.io/blog/2017/02/22/%E6%8F%90%E9%AB%98emacs%E5%90%AF%E5%8A%A8%E9%80%9F%E5%BA%A6/
;; use-package的:config 关键字的作用也基本跟 with-eval-after-load 的作用是相同的：是加载use-package作用的包后、执行config里的代码。
;; https://github.com/jwiegley/use-package/issues/453

;;; Code:
;;* Prerequisites

(message "* --[ Loading my Emacs init file ]--")

;;----------------------------------------------------------------------------
;; uptimes
;;----------------------------------------------------------------------------
(defconst emacs-start-time (current-time))

;;----------------------------------------------------------------------------
;; cl
;;----------------------------------------------------------------------------
;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'

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
(let ((normal-gc-cons-threshold (* 20 1024 1024))        ;; 默认0.76MB=0.76*1024*1024 .  (* 5 1024 1024)这个值垃圾回收6次 / 400000这个值垃圾回收10次 / 32位40万、64位80万
      (init-gc-cons-threshold (* 128 1024 1024)))       ;;(* 500 1024 1024) / (* 128 1024 1024) / 100000000 / most-positive-fixnum
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;;----------------------------------------------------------------------------
;; load-path
;;----------------------------------------------------------------------------

;; load/load-file/autoload说明: load会搜索load-path，load-file需要指定文件完整路
;; 径和扩展名，autoload在一个函数被call后再load指定文件

;; 一、加载目录
;; 1、加载某一目录
;; (add-to-list 'load-path "~/.emacs.d/config/")        ;; 这个要放在所有require语句的前面，否则require语句会报错。通过 load-path 注册扩展文件的所在位置
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (push (expand-file-name "lisp" user-emacs-directory) load-path)

;; https://www.emacswiki.org/emacs/LoadPath
;; (let ((default-directory  "~/.emacs.d/lisp/"))
;;   (setq load-path
;;         (append
;;          (let ((load-path  (copy-sequence load-path))) ;; Shadow
;;            (append
;;             (copy-sequence (normal-top-level-add-to-load-path '(".")))
;;             (normal-top-level-add-subdirs-to-load-path)))
;;          load-path)))

;; 2、加载目录和子目录

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; 二、加载文件,这种方式可能无法控制加载顺序
;; 1、load仅加载指定目录下的el文件
;; (mapc 'load (directory-files "~/.emacs.d/config/" t "\\.el$"))    ;;mapcar 函数逐个装载 "~/..." 目录下的所有配置文件

;; 2、load加载指定目录及其子目录下的el文件
;; ~/.emacs.d/load-directory.el
;; Handy function to load recursively all '.el' files in a given directory
(defun load-directory (directory)
  "Load recursively all '.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))
;; (load-directory "~/.emacs.d/my-config/config")

;; 3、 加载目录和子目录 ;; 用provide/require这种方式加载配置文件，能自己控制加载先后顺序
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
;; (add-subdirs-to-load-path "~/.emacs.d/config/")


;; make loaded files give a message
 (defadvice load (before debug-log activate)
   (message "wangms-Loading %s..." (locate-library (ad-get-arg 0))))

;; load-path enhancement 增强
(defun fni/add-to-load-path (this-directory &optional with-subdirs recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the
optional argument WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
        (setq this-directory (expand-file-name this-directory)))

      (message "Adding `%s' to load-path..." this-directory)
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
          (setq dir-or-file (car files))
          (when (file-directory-p dir-or-file)
            (if recursive
                (fni/add-to-load-path dir-or-file 'with-subdirs 'recursive)
              (fni/add-to-load-path dir-or-file)))
          (setq files (cdr files)))))))

;; Use `M-x list-load-path-shadows RET' to display a list of external Emacs
;; Lisp files that shadow Emacs builtins (listing potential load path
;; problems).


;; (dolist (i '("lisp"
;; 	     "site-lisp"
;; 	     ))
;;   ;; Add all at end of `load-path' to avoid conflicts.
;;   (add-to-list 'load-path (expand-file-name i user-emacs-directory) t)) ;; 参数t是Add all at end of `load-path'

;; (eval-and-compile
;;   (mapc
;;    #'(lambda (path)
;;        (push (expand-file-name path zwb-private-emacs-config-path) load-path))
;;    '("lib""theme" "")))     ;;lambda函数的实参写法

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


;;*** Library Search

;; `load-path' is a list of directories where Emacs Lisp libraries (`.el' and
;; `.elc' files) are installed.

;; `exec-path' is different: it is a list of directories where executable
;; programs are installed.
;;
;; Shouldn't be `exec-path' and `PATH' achieve the same goal under Emacs?
;;
;; No. `exec-path' is used by Emacs to search for programs it runs directly.
;; But `M-x grep' does not run `grep.exe' directly; it runs the shell passing
;; it a command that invokes `grep'. So it's the shell that needs to find
;; `grep.exe', and it uses PATH, of course, not `exec-path'.
;;
;; So the right thing to do when you install a new program, in order for Emacs
;; to find it, is *both* to update `exec-path' *and* update `PATH'. This is
;; because some Emacs features invoke programs directly, while others do that
;; through the shell or some other intermediary programs.

;; The most important directories are the last!

;; TODO Specify variables using `defcustom'

(when (string-equal system-type "windows-nt")
  (let ((mypaths
         (list
          "C:/Program Files/Git/usr/bin/"      ;; 将git-find排在(getenv "PATH")前面
          (getenv "PATH")
          )
         ))

    (setenv "PATH" (mapconcat 'identity mypaths ";"))
    (setq exec-path (append mypaths (list "." exec-directory)))      ;;exec-path默认只是复制PATH的原来值Original value
    )
  )

;;----------------------------------------------------------------------------
;; Features
;;----------------------------------------------------------------------------

;;*** Features

;; ;; REPLACES ORIGINAL in `C source code' (dumped)
;; ;; redefine require to leave a trace of packages being loaded
;; (if (not (fboundp 'orig-require))
;;     (fset 'orig-require (symbol-function 'require))
;;   (message "The code to redefine `require' should not be loaded twice"))

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
      (progn
        (message "Checking for library `%s'..." feature)
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
		   feature (float-time (time-since timestamp)))))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

(defun require-extensions (action lst &optional click)
  ""
  (mapcar (lambda(ext) "" (funcall action ext click)) lst))
;; 有了这个函数，我们就可以进行非常简单的工作了删掉 require ，换成 list 。

;; Load all configuration and packages.
(let ((ts-init (current-time)))
  (setq missing-packages-list nil
        package-init-statistic nil)

(require-extensions 'try-require
		    '(init-const
		      init-custom
		      init-package
		      init-basic
		      init-funcs
		      init-edit
		      init-edit-visual
		      init-frame-hooks
		      init-frame
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
		      init-modeline
		      init-theme
		      ;; init-face

		      doremi-frm          ;; 使用库doremi-frm.el(依赖库doremi.el、hexrgb.el、frame-fns.el、faces+.el)中doremi-font+命令, 循环查看可用字体及其效果.
		      cursor-change       ;;智能光标形状

		      init-ivy
		      init-projectile
		      init-key
		      init-complete
		      init-neotree
		      init-tabbar
		      init-sidebar
		      init-slime
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
	     ". Refer to `missing-packages-list` for missing packages."
	   ".\n}")))

(cursor-change-mode 1) ;;智能光标形状

;;----------------------------------------------------------------------------
;; display time
;;----------------------------------------------------------------------------

(let ((ts-init (current-time)))
  (setq missing-packages-list nil
        package-init-statistic nil)
  (add-hook 'after-init-hook
            (lambda () (message "*** --【 Finished startup in %.2f seconds,  %d packages missing%s 】-- ***"
                                (float-time (time-since ts-init)) (length missing-packages-list)
                                (if missing-packages-list
                                    ". Refer to `missing-packages-list` for missing packages."
                                  "."))
              )))

;; 自定义"For information about GNU Emacs and the GNU system, type C-h C-a."这个消息。
(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "*** --【 Initialized in %s 】-- ***" (emacs-init-time))
  )

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** --【 Emacs ready in %s with %d garbage collections. 】-- ***"
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-hook 'after-init-hook (lambda () (message (format "time-subtract = %s" (float-time (time-subtract (current-time) emacs-start-time))))))
(add-hook 'after-init-hook (lambda () (message (format "after/before-init-time = %s" (float-time (time-subtract after-init-time before-init-time))))))
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


;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-up' (or `M-down')
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; ;; XXX `M-up' and `M-down' are bound multiple times (to different things)!
;; (global-set-key (kbd "<M-up>") 'move-line-up)
;; (global-set-key (kbd "<M-down>") 'move-line-down)


(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))



(provide 'init)


;;; init.el ends here
;; (put 'narrow-to-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
