;;; core-load-paths.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
  "If DIR exists in the file system, add it to `load-path'."
  (when (file-exists-p dir)
    (add-to-load-path dir)))

;; paths
;;  (defvar spacemacs-start-directory
;;    user-emacs-directory
;;    "Spacemacs start directory.")
;;  (defconst spacemacs-core-directory
;;    (expand-file-name (concat spacemacs-start-directory "core/"))
;;    "Spacemacs core directory.")
;;  (defconst spacemacs-info-directory
;;    (expand-file-name (concat spacemacs-core-directory "info/"))
;;    "Spacemacs info files directory")
;;  (defconst spacemacs-release-notes-directory
;;    (expand-file-name (concat spacemacs-info-directory "release-notes/"))
;;    "Spacemacs release notes directory")
;;  (defconst spacemacs-banner-directory
;;    (expand-file-name (concat spacemacs-core-directory "banners/"))
;;    "Spacemacs banners directory.")
;;  (defconst spacemacs-banner-official-png
;;    (expand-file-name (concat spacemacs-banner-directory "img/spacemacs.png"))
;;    "Spacemacs official banner image.")
;;  (defconst spacemacs-badge-official-png
;;    (expand-file-name (concat spacemacs-banner-directory
;;                              "img/spacemacs-badge.png"))
;;    "Spacemacs official badge image.")
;;  (defconst spacemacs-purple-heart-png
;;    (expand-file-name (concat spacemacs-banner-directory "img/heart.png"))
;;    "Purple heart emoji.")
;;  (defconst spacemacs-cache-directory
;;    (expand-file-name (concat user-emacs-directory ".cache/"))
;;    "Spacemacs storage area for persistent files")
;;  (defconst spacemacs-auto-save-directory
;;    (expand-file-name (concat spacemacs-cache-directory "auto-save/"))
;;    "Spacemacs auto-save directory")
;;  (defconst spacemacs-docs-directory
;;    (expand-file-name (concat spacemacs-start-directory "doc/"))
;;    "Spacemacs documentation directory.")
;;  (defconst spacemacs-news-directory
;;    (expand-file-name (concat spacemacs-start-directory "news/"))
;;    "Spacemacs News directory.")
;;  (defconst spacemacs-assets-directory
;;    (expand-file-name (concat spacemacs-start-directory "assets/"))
;;    "Spacemacs assets directory.")
;;  (defconst spacemacs-test-directory
;;    (expand-file-name (concat spacemacs-start-directory "tests/"))
;;    "Spacemacs tests directory.")
;;
;;  (defconst user-home-directory
;;    (expand-file-name "~/")
;;    "User home directory (~/).")
;;  (defconst pcache-directory
;;    (concat spacemacs-cache-directory "pcache/"))
;;  (unless (file-exists-p spacemacs-cache-directory)
;;    (make-directory spacemacs-cache-directory))
;;
;;  ;; load paths
;;  (mapc 'add-to-load-path
;;        `(
;;          ,spacemacs-core-directory
;;          ,(concat spacemacs-core-directory "libs/")
;;          ,(concat spacemacs-core-directory "libs/spacemacs-theme/")
;;          ;; ,(concat spacemacs-core-directory "aprilfool/")
;;          ))
;;
;;  ;; themes
;;  (add-to-list 'custom-theme-load-path (concat spacemacs-core-directory
;;                                               "libs/spacemacs-theme/"))




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

;; https://github.com/sjbalaji/myCustomizations/blob/master/ReferenceEmacsConfig
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
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  )

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

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

;; M-:(getenv ​"PATH")
;; M-:(getenv ​"exec-path")
;; (executable-find "ag")    看看能不能找到可执行程序

(when (string-equal system-type "windows-nt")
  (let ((mypaths
         (list
          "D/Program Files/Git/usr/bin/"      ;; 将git-find排在(getenv "PATH")前面
	  "D:\Portable\ripgrep(rg)"
          (getenv "PATH")
          )
         ))

    (setenv "PATH" (mapconcat 'identity mypaths ";"))
    (setq exec-path (append mypaths (list "." exec-directory)))      ;;exec-path默认只是复制PATH的原来值Original value
    )
  )
