;;; init-naotree.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; NeoTree
(use-package neotree
  :config
  (progn
    (defun sanityinc/window-system-frame-setup()
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))  ;; display-graphic-p t为图形界面、nil为字符界面(即终端下打开emacs)
    (add-hook 'after-make-window-system-frame-hooks 'sanityinc/window-system-frame-setup)
    ))


(provide 'init-neotree)

;;; init-neotree.el ends here
