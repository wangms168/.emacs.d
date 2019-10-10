;;; init-edit-visual.el --- modeline configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auto-highlight-symbol
  :ensure t
  :diminish
  :hook (prog-mode . global-auto-highlight-symbol-mode))

;;----------------------------------------------------------------------------
;; 二次选择高亮
;;----------------------------------------------------------------------------
(use-package volatile-highlights
  :ensure t
  :diminish
  :hook (after-init . volatile-highlights-mode)
  :custom-face
  ;; (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  )

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-mode))

;; ------------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package highlight-parentheses
;;   :ensure t
;;   :init
;;   :config
;;   (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
;;   )

;; ------------------------------------------------------------------------------------
(use-package highlight-indentation            ;; 高亮缩进
  :ensure t
  :diminish
  :hook ((prog-mode yaml-mode) . highlight-indentation-mode)
  :custom
  (set-face-background 'highlight-indentation-face "#1E1E1E")
  )

;; (use-package highlight-indent-guides      ;; 另一个高亮缩进
;;   :ensure t
;;   :diminish
;;   :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
;;   :custom
;;   (highlight-indent-guides-auto-enabled t)
;;   (highlight-indent-guides-responsive t)
;;   (highlight-indent-guides-method 'character)) ; column

;; (use-package indent-guide
;;   :hook ((prog-mode yaml-mode) . indent-guide-global-mode))

(use-package page-break-lines
  :ensure t
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package flymd                        ;;预览md文件，M-x flymd-flyit。
  :ensure t)
(use-package impatient-mode               ;;实时预览html文件。依赖simple-httpd与htmlize
  :ensure t)

(use-package skewer-mode       ;;html\css\js交互。依赖simple-httpd与js2-mode
  :ensure t
  :hook (html-mode . skewer-html-mode)
  :config
  (require 'simple-httpd)
  ;; set root folder for httpd server
  (setq httpd-root "/home/wangms/Documents/javascript")
  )



(provide 'init-edit-visual)

;;; init-edit-visual.el ends here
