;;; init-key.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package auto-complete
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))


(provide 'init-complete)

;;; init-complete.el ends here
