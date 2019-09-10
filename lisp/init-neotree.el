;;; init-naotree.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
;; neotree设置
;; 注意，打开calendar日历，要在neotr关闭开启状态下进行，否则日历会显示在上部分。
(use-package neotree
  :config
  (progn
    (defun sanityinc/window-system-frame-setup()
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))  ;; display-graphic-p t为图形界面、nil为字符界面(即终端下打开emacs)
    (add-hook 'after-make-window-system-frame-hooks 'sanityinc/window-system-frame-setup)
    ))


(provide 'init-neotree)

;;; init-neotree.el ends here
