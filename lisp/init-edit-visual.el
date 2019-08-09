;;; init-edit-visual.el --- modeline configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auto-highlight-symbol
  :init
  (add-hook 'prog-mode-hook 'global-auto-highlight-symbol-mode)
  :config
  )

(use-package rainbow-mode
  :init
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  )

;; ------------------------------------------------------------------------------------
(use-package rainbow-delimiters           ;; 高亮缩进
  :init
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; (use-package highlight-parentheses
;;   :init
;;   :config
;;   (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
;;   )

;; ------------------------------------------------------------------------------------
(use-package highlight-indentation
  :init
  :config
  (set-face-background 'highlight-indentation-face "#1E1E1E")
  (add-hook 'prog-mode-hook 'highlight-indentation-mode)
  )

;; (use-package highlight-indent-guides
;;   :init
;;   (setq highlight-indent-guides-method 'column)    ;; 'character \ 'column
;;   :config
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;   )

;; (use-package indent-guide
;;   :init
;;   :config
;;   (add-hook 'prog-mode-hook 'indent-guide-global-mode)
;;   )


(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode)
  ;; :config
  ;; (add-hook 'after-init-hook 'global-page-break-lines-mode)
  )

(use-package flymd)            ;;预览md文件，M-x flymd-flyit。
(use-package impatient-mode)   ;;实时预览html文件。依赖simple-httpd与htmlize

(use-package skewer-mode       ;;html\css\js交互。依赖simple-httpd与js2-mode
  :ensure t
  :config
  (require 'simple-httpd)
  ;; set root folder for httpd server
  (setq httpd-root "/home/wangms/Documents/javascript")
  (add-hook 'html-mode-hook 'skewer-html-mode)
 )



(provide 'init-edit-visual)

;;; init-edit-visual.el ends here
