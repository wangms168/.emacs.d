
;;(set-fontset-font t 'unicode (font-spec :family "icons-in-terminal") nil 'append)
;;(set-fontset-font t nil "icons-in-terminal" nil 'append)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/icons-in-terminal.el/") ;;seagle0128

(add-to-list 'load-path "~/.local/share/icons-in-terminal/")    ;;sidbar的依赖
(require 'icons-in-terminal)
(add-to-list 'load-path "~/.emacs.d/site-lisp/font-lock-plus/") ;;sidbar的依赖
(add-to-list 'load-path "~/.emacs.d/site-lisp/sidebar.el/") ;; If it's not already done
(add-to-list 'load-path "PATH-TO-SIDEBAR-DIRECTORY")
(require 'sidebar)
;; (global-set-key (kbd "C-x C-f") 'sidebar-open)
;; (global-set-key (kbd "C-x C-a") 'sidebar-buffers-open)


(provide 'init-sidebar)

;;; init-sidebar.el ends here
