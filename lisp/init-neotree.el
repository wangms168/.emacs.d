;;; init-naotree.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
;; neotree设置
;; 注意，打开calendar日历，要在neotr关闭开启状态下进行，否则日历会显示在上部分。
(use-package neotree
  :after projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme 'icons)     ;;
  :bind
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
	   (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
	     (projectile-project-root)
	     ))
	  (file-name (buffer-file-name))
	  (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
	       (neo-global--window-exists-p))
	  (neotree-hide)
	(progn
	  (neotree-show)
	  (if project-dir
	      (neotree-dir project-dir))
	  (if file-name
	      (neotree-find file-name)))))))


(provide 'init-neotree)

;;; init-neotree.el ends here
