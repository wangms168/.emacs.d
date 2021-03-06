
;;(set-fontset-font t 'unicode (font-spec :family "icons-in-terminal") nil 'append)
;;(set-fontset-font t nil "icons-in-terminal" nil 'append)

;; (add-to-list 'load-path "~/.local/share/icons-in-terminal")    ;; If it's not already done
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/sidebar")        ;; (add-to-list 'load-path "PATH-TO-SIDEBAR-DIRECTORY")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/font-lock-plus") ;;sidbar的依赖,需require前指明路径。

(add-to-list 'load-path "~/.local/share/icons-in-terminal")    ;; If it's not already done
(add-to-list 'load-path (expand-file-name "site-lisp/sidebar" user-emacs-directory))         ;; (add-to-list 'load-path "PATH-TO-SIDEBAR-DIRECTORY")
(add-to-list 'load-path (expand-file-name "site-lisp/font-lock-plus" user-emacs-directory))  ;;sidbar的依赖,需require前指明路径。

;; (use-package sidebar
;;   :load-path "site-lisp/sidebar"
;;   )
;; (require 'sidebar)

;; (global-set-key (kbd "C-x C-f") 'sidebar-open)
;; (global-set-key (kbd "C-x C-a") 'sidebar-buffers-open)


(provide 'init-sidebar)

;;; init-sidebar.el ends here
