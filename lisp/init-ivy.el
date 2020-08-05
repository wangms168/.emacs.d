;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------------------------------------------------------------------
;;; Minibuffer Input Completion
;; 1、Icomplete Mode Setup  ;; http://ergoemacs.org/emacs/emacs_icomplete_mode.html
;; (progn
;;   ;; minibuffer enhanced completion
;;   (require 'icomplete)
;;   (icomplete-mode 1)
;;   ;; show choices vertically
;;   (setq icomplete-separator "\n")
;;   (setq icomplete-hide-common-prefix nil)
;;   (setq icomplete-in-buffer t)

;;   (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
;;   (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions))

;; 2、Ido Mode Setup ;; http://ergoemacs.org/emacs/emacs_ido_mode.html
;; (progn
;;   ;; make buffer switch command do suggestions, also for find-file command
;;   (require 'ido)
;;   (ido-mode 1)

;;   ;; show choices vertically
;;   (if (version< emacs-version "25")
;;       (progn
;; 	(make-local-variable 'ido-separator)
;; 	(setq ido-separator "\n"))
;;     (progn
;;       (make-local-variable 'ido-decorations)
;;       (setf (nth 2 ido-decorations) "\n")))

;;   ;; show any name that has the chars you typed
;;   (setq ido-enable-flex-matching t)
;;   ;; use current pane for newly opened file
;;   (setq ido-default-file-method 'selected-window)
;;   ;; use current pane for newly switched buffer
;;   (setq ido-default-buffer-method 'selected-window)
;;   ;; stop ido from suggesting when naming new file
;;   (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
;;   )
;; ;; big minibuffer height, for ido to show choices vertically
;; (setq max-mini-window-height 0.5)


;; Ivy, Counsel, Swiper Setup
(use-package counsel :ensure t)              ;; 依赖ivy、swiper
(ivy-mode 1) ;; Turn on ivy by default
;; (setq ivy-initial-inputs-alist nil)      ;; 默认情况下ivy启用过滤器^
(setq ivy-use-virtual-buffers t)  ;; no idea, but recommended by project maintainer
(setq enable-recursive-minibuffers t) ;; no idea, but recommended by project maintainer
(setq ivy-count-format "(%d/%d) ")  ;; changes the format of the number of results
(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)                      ;; execute-extended-command
(global-set-key (kbd "C-x C-f") 'counsel-find-file)            ;; find-file
(global-set-key (kbd "C-x C-l") 'counsel-find-library)         ;; find-library

;; (add-hook 'help-mode-hook
;; 	  (lambda ()
;; 	    (define-key help-mode-map "a" 'counsel-apropos)                 ;; apropos-command
;; 	    (define-key help-mode-map "b" 'counsel-descbinds)              ;; describe-bindings
;; 	    (define-key help-mode-map "f" 'counsel-describe-function)     ;; describe-function
;; 	    (define-key help-mode-map "v" 'counsel-describe-variable)    ;; describe-variable
;; 	    (define-key help-mode-map "S" 'counsel-info-lookup-symbol)  ;; info-lookup-symbol
;; 	    ))

;; (define-key help-mode-map (kbd "a") 'counsel-apropos)                 ;; apropos-command
;; (define-key help-mode-map (kbd "b") 'counsel-descbinds)              ;; describe-bindings
;; (define-key help-mode-map (kbd "f") 'counsel-describe-function)     ;; describe-function
;; (define-key help-mode-map (kbd "v") 'counsel-describe-variable)    ;; describe-variable
;; (define-key help-mode-map (kbd "S") 'counsel-info-lookup-symbol)  ;; info-lookup-symbol

(define-key help-map (kbd "a") 'counsel-apropos)                 ;; apropos-command
;; (define-key help-map (kbd "b") 'counsel-descbinds)              ;; describe-bindings
(define-key help-map (kbd "f") 'counsel-describe-function)     ;; describe-function
(define-key help-map (kbd "v") 'counsel-describe-variable)    ;; describe-variable
(define-key help-map (kbd "S") 'counsel-info-lookup-symbol)  ;; info-lookup-symbol

;; (global-set-key (kbd "C-h a") 'counsel-apropos)                 ;; apropos-command
;; (global-set-key (kbd "C-h b") 'counsel-descbinds)              ;; describe-bindings
;; (global-set-key (kbd "C-h f") 'counsel-describe-function)     ;; describe-function
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)    ;; describe-variable
;; (global-set-key (kbd "C-h S") 'counsel-info-lookup-symbol)  ;; info-lookup-symbol

(global-set-key (kbd "C-c u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag) ;; add counsel/ivy features to ag package
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
;; counsel-describe-face                describe-face
;; counsel-faces                           list-faces-display
;; counsel-imenu                          imenu
;; counsel-load-library                  load-library
;; counsel-load-theme                  load-theme
;; counsel-yank-pop                     yank-pop
;; counsel-mark-ring                    pop-to-mark-command
;; counsel-bookmark                   bookmark-jump

;; set action options during execution of counsel-find-file
;; replace "frame" with window to open in new window
(ivy-set-actions
 'counsel-find-file
 '(("j" find-file-other-frame "other frame")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("d" delete-file "delete")
   ("r" counsel-find-file-as-root "open as root")))

;; set actions when running C-x b
;; replace "frame" with window to open in new window
(ivy-set-actions
 'ivy-switch-buffer
 '(("j" switch-to-buffer-other-frame "other frame")
   ("k" kill-buffer "kill")
   ("r" ivy--rename-buffer-action "rename")))

;; End Ivy, Swiper, Counsel


(use-package ivy-rich
  :ensure t
  ;; :init
  ;; (defun ivy-rich-switch-buffer-icon (candidate)
  ;;   (with-current-buffer
  ;; 	(get-buffer candidate)
  ;;     (let ((icon (all-the-icons-icon-for-mode major-mode)))
  ;; 	(if (symbolp icon)
  ;; 	    (all-the-icons-icon-for-mode 'fundamental-mode)
  ;; 	  icon))))
  ;; (setq ivy-rich-display-transformers-list
  ;; 	'(ivy-switch-buffer
  ;; 	  (:columns
  ;; 	   ((ivy-rich-switch-buffer-icon :width 2)
  ;; 	    (ivy-rich-candidate (:width 30))
  ;; 	    (ivy-rich-switch-buffer-size (:width 7))
  ;; 	    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
  ;; 	    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
  ;; 	    (ivy-rich-switch-buffer-project (:width 15 :face success))
  ;; 	    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
  ;; 	   :predicate
  ;; 	   (lambda (cand) (get-buffer cand)))
  ;; 	  counsel-M-x
  ;; 	  (:columns
  ;; 	   ((counsel-M-x-transformer (:width 40))  ; thr original transformer
  ;; 	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
  ;; 	  counsel-describe-function
  ;; 	  (:columns
  ;; 	   ((counsel-describe-function-transformer (:width 40))  ; the original transformer
  ;; 	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
  ;; 	  counsel-describe-variable
  ;; 	  (:columns
  ;; 	   ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
  ;; 	    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
  ;; 	  counsel-recentf
  ;; 	  (:columns
  ;; 	   ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
  ;; 	    (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))) ; return the last modified time of the file
  ;; 	)
  :config
  (ivy-rich-mode 1)
  )


(provide 'init-ivy)

;;; init-ivy.el ends here
