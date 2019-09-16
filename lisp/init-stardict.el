(use-package sdcv
  :load-path "site-lisp/sdcv"
  :bind
  ("C-c d" . sdcv-search-pointer+)
  :init
  (setq sdcv-say-word-p t)               ;say word after translation
  (setq sdcv-dictionary-data-dir "~/.stardict/dic") ;setup directory of stardict dictionary

  (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
  	'(
	  "懒虫简明英汉词典"
  	  "朗道英汉字典5.0"
  	  "牛津现代英汉双解词典"
  	  "21世纪英汉汉英双向词典"
  	  ))
  (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
  	'(
	  "懒虫简明英汉词典"
  	  "朗道英汉字典5.0"
  	  "牛津现代英汉双解词典"
  	  "21世纪英汉汉英双向词典"
  	  ))
  )


;; (add-to-list 'load-path "~/.emacs.d/site-lisp/sdcv")
;; (require 'sdcv)
;; (setq sdcv-say-word-p t)               ;say word after translation
;; (setq sdcv-dictionary-data-dir "~/.stardict/dic/") ;setup directory of stardict dictionary

;; (setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
;;       '(
;; 	"朗道英汉字典5.0"
;; 	"牛津现代英汉双解词典"
;; 	"21世纪英汉汉英双向词典"
;; 	))
;; (setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
;;       '(
;; 	"朗道英汉字典5.0"
;; 	"牛津现代英汉双解词典"
;; 	"21世纪英汉汉英双向词典"
;; 	))
;; (global-set-key (kbd "C-c d") 'sdcv-search-pointer+ )



(provide 'init-stardict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-stardict.el ends here
