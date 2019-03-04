;;; init-edit-visual.el --- modeline configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auto-highlight-symbol
  :init
  :config
  (global-auto-highlight-symbol-mode t)
  )

(use-package rainbow-mode
  :init
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  )

;; ------------------------------------------------------------------------------------
(use-package rainbow-delimiters
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



(provide 'init-edit-visual)

;;; init-edit-visual.el ends here
