;;; init-elpa.el --- Initialize elpa configurations.	-*- lexical-binding: t -*-

;; 下面这种方式几乎所有的包都安装成稳定版
;; (setq package-archives
;;       '(
;;         ("GNU ELPA"     . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") 
;; 	("MELPA Stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
;; 	("MELPA"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;; 	) 
;;       package-archive-priorities 
;;       '(("MELPA Stable" . 10) 
;; 	("GNU ELPA"     . 5)
;; 	("MELPA"        . 0))
;;       package-enable-at-startup nil
;;       package--init-file-ensured t
;;       )

(setq package-archives
      '(("gnu"          . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa"        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))

;; 我需要稳定版的 `ace-window'，只从 melpa-stable 安装
(setq package-pinned-packages
      '(
	;; (page-break-lines . "melpa-stable")
	))

;; ;; 开始安装我需要的包
;; (dolist (pkg '(ace-window paredit))
;;   (unless (package-installed-p pkg)
;;     (package-install pkg)))
;; 用 Emacs 25 的话，应该可以直接设置 package-selected-packages，然后 M-x package-install-selected-packages 安装。我自己没试过，一直用 use-package 的 :ensure 安装包。


;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                    (not (gnutls-available-p))))
;;       (proto (if no-ssl "http" "https")))
;;  (add-to-list 'package-archives (cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/")) t)
;;  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://elpa.emacs-china.org/melpa-stable/")) t))


(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)


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

(require-packages package-selected-packages)



(provide 'init-elpa)

;;; init-elpa.el ends here
