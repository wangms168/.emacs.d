;;; init-naotree.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://www.emacswiki.org/emacs/NeoTree_%E4%B8%AD%E6%96%87wiki
;; neotree设置
;; 注意，打开calendar日历，要在neotr关闭开启状态下进行，否则日历会显示在上部分。
(use-package neotree
  :ensure t
  :init
  ;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :custom
  (neo-theme 'icons)     ;;
  :bind
  ("<f9>" . neotree-toggle)
  )


(provide 'init-neotree)

;;; init-neotree.el ends here
