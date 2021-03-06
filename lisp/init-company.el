;; 代码补全
(use-package company
  :ensure t
  :custom
  (company-tooltip-align-annotations t "aligns annotation to the right")
  (company-tooltip-limit 24            "bigger popup window")
  (company-idle-delay .2               "decrease delay before autocompletion popup shows")
  (company-echo-delay 0                "remove annoying blinking")
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  ;; Nicer looking faces
  :custom-face
  (company-tooltip-common ((nil (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection ((nil (:inherit company-tooltip-selection :weight bold :underline nil))))
  :hook (after-init . global-company-mode)
  )

(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil
	company-box--max 1000)

  (with-eval-after-load 'all-the-icons
    (eval-and-compile
      (defun my-company-box-icon (family icon &rest args)
	"Defines icons using `all-the-icons' for `company-box'."
	(when icon
	  (let ((icon (pcase family
			('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 args))
			('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575))
			('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 args))
			('alltheicon (all-the-icons-alltheicon icon :height 0.8 args)))))
	    (unless (symbolp icon)
	      (concat icon
		      (propertize " " 'face 'variable-pitch)))))))

    (setq company-box-icons-all-the-icons
	  `((Unknown . ,(my-company-box-icon 'octicon "file-text"))
	    (Text . ,(my-company-box-icon 'faicon "file-text-o"))
	    (Method . ,(my-company-box-icon 'faicon "cube"))
	    (Function . ,(my-company-box-icon 'faicon "cube"))
	    (Constructor . ,(my-company-box-icon 'faicon "cube"))
	    (Field . ,(my-company-box-icon 'faicon "tag"))
	    (Variable . ,(my-company-box-icon 'faicon "tag"))
	    (Class . ,(my-company-box-icon 'faicon "cog"))
	    (Interface . ,(my-company-box-icon 'faicon "cogs"))
	    (Module . ,(my-company-box-icon 'alltheicon "less"))
	    (Property . ,(my-company-box-icon 'faicon "wrench"))
	    (Unit . ,(my-company-box-icon 'faicon "tag"))
	    (Value . ,(my-company-box-icon 'faicon "tag"))
	    (Enum . ,(my-company-box-icon 'faicon "file-text-o"))
	    (Keyword . ,(my-company-box-icon 'material "format_align_center"))
	    (Snippet . ,(my-company-box-icon 'material "content_paste"))
	    (Color . ,(my-company-box-icon 'material "palette"))
	    (File . ,(my-company-box-icon 'faicon "file"))
	    (Reference . ,(my-company-box-icon 'faicon "tag"))
	    (Folder . ,(my-company-box-icon 'faicon "folder"))
	    (EnumMember . ,(my-company-box-icon 'faicon "tag"))
	    (Constant . ,(my-company-box-icon 'faicon "tag"))
	    (Struct . ,(my-company-box-icon 'faicon "cog"))
	    (Event . ,(my-company-box-icon 'faicon "bolt"))
	    (Operator . ,(my-company-box-icon 'faicon "tag"))
	    (TypeParameter . ,(my-company-box-icon 'faicon "cog"))
	    (Template . ,(my-company-box-icon 'octicon "file-code"))))))

(use-package company-quickhelp
  :defines company-quickhelp-delay
  :bind (:map company-active-map
	      ("M-h" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.8))

(use-package company-english-helper
  :load-path "site-lisp/company-english-helper"
  :bind
  ("C-c h" . toggle-company-english-helper)
  :config
  ;; (toggle-company-english-helper)
  )


(provide 'init-company)
