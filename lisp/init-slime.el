(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")   ;;需pacman安装sbcl程序,sbcl=steel bank common lisp
  (setq slime-contribs '(slime-fancy))
  :config
  ;; (slime-setup)
  )


(provide 'init-slime)
