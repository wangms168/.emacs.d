;;; init.el --- Initialize configurations.	-*- lexical-binding: t -*-
;; 测免帐号及密码
;; https://github.com/honmaple/dotfiles/tree/master/emacs.d/lisp

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize); You may delete these explanatory comments.

;;; Code:

;; (let ((table (make-display-table)))  (aset table ?\^L [?📄])  (setq buffer-display-table table))
;; 


(defvar maple/file-name-handler-alist file-name-handler-alist)
(defvar user-default-theme nil)
(defvar *company-lsp* nil)
(defvar *common* t)
(defvar *develop* t)

(setq user-full-name "jianglin"
      user-default-theme 'monokai
      user-mail-address "mail@honmaple.com"
      gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist maple/file-name-handler-alist
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;;----------------------------------------------------------------------------
;; load-path
;;----------------------------------------------------------------------------

;; (defmacro maple/require (pkg)
;;   "Load PKG."
;;   `(load (file-truename (format "%s/lisp/%s" (expand-file-name user-emacs-directory) ,pkg))  t t))

;; (defmacro maple/require (pkg)
;;   "Load PKG."
;;   `(require ,pkg (file-truename (format "%s/lisp/%s" (expand-file-name user-emacs-directory) ,pkg))))

;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory) t)         ;; 参数t是Add all at end of `load-path'
;; (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory) t)

(dolist (i '("lisp"
	     "site-lisp"
	     ))
  ;; Add all at end of `load-path' to avoid conflicts.
  (add-to-list 'load-path (expand-file-name i user-emacs-directory) t)) ;; 参数t是Add all at end of `load-path'

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-elpa)      ;; Machinery for installing required packages
;; (require 'init-basic)
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;; ----------------------------------------------------------------------------
(when *common*
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
  )

(when *develop*
  )

(when *company-lsp*
  ;; (maple/require 'init-lsp)
  )

;; (with-eval-after-load 'evil-leader
;;   (maple/require 'init-keybind))

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

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
 (load custom-file))

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
