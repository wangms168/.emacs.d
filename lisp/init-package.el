;;; init-package.el --- Initialize package configurations.	-*- coding: utf-8 lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

;;使用网络代理
;;(setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:49998")
;;         ("https" . "127.0.0.1:49998")))

(eval-when-compile
  (require 'init-custom))
;;
;; ELPA: refer to https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
;;
(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read "Choose package archives: "
                                  '(melpa melpa-mirror emacs-china netease tuna)))))
  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))
	       ))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))
	       ))
            ('emacs-china
             `(,(cons "melpa-cn"   (concat proto "://elpa.emacs-china.org/melpa/"))
               ,(cons "org-cn" (concat proto "://elpa.emacs-china.org/org/"))
	       ,(cons "gnu-cn" (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa-stable-cn" (concat proto "://elpa.emacs-china.org/melpa-stable/"))
	       ))
            ('netease
             `(,(cons "melpa-163" (concat proto "://mirrors.163.com/elpa/melpa/"))
               ,(cons "gnu-163"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa-stable-163" (concat proto "://mirrors.163.com/elpa/melpa-stable/"))
	       ))
            ('tuna
             `(,(cons "melpa-tuna"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
               ,(cons "org-tuna" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
               ,(cons "gun-tuna" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
	       ,(cons "melpa-stable-tuna" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))
	       ))
            (archives
             (error "Unknown archives: '%s'" archives))))
	;;	package-archive-priorities                   ;; 它指定每个存档的优先级（数字越大，优先级越高的存档）
	;;	'(("melpa-stable" . 10)
	;;	  ("melpa"        . 5)
	;;	  ("gnu"          . 0))
	)
  (message "Set package archives to '%s'." archives))
(set-package-archives centaur-package-archives)

;; 我需要稳定版的 `ace-page-break-lines'，只从 melpa-stable 安装
(setq package-pinned-packages                      ;; 将程序包/归档对添加到此列表，以确保仅从指定的归档文件下载指定的程序包。
      '(
	(page-break-lines . "melpa-stable-cn")
	))

;; Initialize packages
(require 'package)
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package--init-file-ensured t )          ;; t 是知道 init file 具有 package-initialize
  (setq package-enable-at-startup nil)          ; To prevent initializing twice ;; emacs startup 会自动加载程序包。禁用它，是为了不重复下面显式(package-initialize)
  (setq load-prefer-newer t)
  (package-initialize))                         ;; 初始化并加载程序包
(unless package-archive-contents
  (package-refresh-contents))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun require-packages (packages)
  (while packages
    (require-package (car packages))
    (setq packages (cdr packages)))
  t)


;; 用 Emacs 25 的话，应该可以直接设置 package-selected-packages，然后 M-x package-install-selected-packages 安装。我自己没试过，一直用 use-package 的 :ensure 安装包。
;; (defvar sanityinc/required-packages nil)
;; (when (fboundp 'package--save-selected-packages)
;;   (require-package 'seq)
;;   (add-hook 'after-init-hook
;;             (lambda () (package--save-selected-packages
;; 			(seq-uniq (append sanityinc/required-packages package-selected-packages))))))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)            ;; 删除了customize-save-variable呼叫
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Setup `use-package'
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    ;;(package-install 'bind-key)     ;;安装use-package时附加bind-key包
    ))
(setq use-package-verbose t)

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)        ;;总是安装软件包
  ;; (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))


(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
