;;; init-icons.el ---    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 第九步 ==========================================================================;;
;; 安装all-the-icons包(附带font-lock+和memoize(这是一个函数)这2个包)
;; 并手工安装“all-the-icons.el-fonts”这个文件夹下的字体，
;; 以使neotree和dired使用，并如下设置：

(use-package all-the-icons)

;; https://github.com/domtronn/all-the-icons.el/commit/f155ce7e6984d8fe11831cd8a9f89828f5c5be43    ;解决tabbar、neotree、spaceline用all-the-icons卡顿的方法
(setq inhibit-compacting-font-caches t)

(use-package all-the-icons-dired
  ;; :config
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode)
  )

(use-package all-the-icons-ivy           ;;依赖ivy包
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  )

(provide 'init-icons)

;;; init-icons.el ends here
