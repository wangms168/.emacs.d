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
(use-package counsel    ;; 依赖ivy、swiper
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (counsel-mode 1)           ;;counsel-mode下有定义大部分键绑定
  ;;  (defvar counsel-mode-map
  ;;    (let ((map (make-sparse-keymap)))
  ;;      (dolist (binding
  ;;	       '((execute-extended-command . counsel-M-x)
  ;;		 (describe-bindings . counsel-descbinds)
  ;;		 (describe-function . counsel-describe-function)
  ;;		 (describe-variable . counsel-describe-variable)
  ;;		 (describe-symbol . counsel-describe-symbol)
  ;;		 (apropos-command . counsel-apropos)
  ;;		 (describe-face . counsel-describe-face)
  ;;		 (list-faces-display . counsel-faces)
  ;;		 (find-file . counsel-find-file)
  ;;		 (find-library . counsel-find-library)
  ;;		 (imenu . counsel-imenu)
  ;;		 (load-library . counsel-load-library)
  ;;		 (load-theme . counsel-load-theme)
  ;;		 (yank-pop . counsel-yank-pop)
  ;;		 (info-lookup-symbol . counsel-info-lookup-symbol)
  ;;		 (pop-to-mark-command . counsel-mark-ring)
  ;;		 (geiser-doc-look-up-manual . counsel-geiser-doc-look-up-manual)
  ;;		 (bookmark-jump . counsel-bookmark)))
  ;;	(define-key map (vector 'remap (car binding)) (cdr binding)))
  ;;      map)
  ;;    "Map for `counsel-mode'.
  ;;  Remaps built-in functions to counsel replacements.")
   )

;;  (define-key help-map (kbd "a") 'counsel-apropos)                 ;; apropos-command
;;  ;; (define-key help-map (kbd "b") 'counsel-descbinds)              ;; describe-bindings
;;  (define-key help-map (kbd "f") 'counsel-describe-function)     ;; describe-function
;;  (define-key help-map (kbd "v") 'counsel-describe-variable)    ;; describe-variable
;;  (define-key help-map (kbd "S") 'counsel-info-lookup-symbol)  ;; info-lookup-symbol

;; set action options during execution of counsel-find-file
;; replace "frame" with window to open in new window
;;  (ivy-set-actions
;;   'counsel-find-file
;;   '(("j" find-file-other-frame "other frame")
;;     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
;;     ("x" counsel-find-file-extern "open externally")
;;     ("d" delete-file "delete")
;;     ("r" counsel-find-file-as-root "open as root")))
;;
;;  ;; set actions when running C-x b
;;  ;; replace "frame" with window to open in new window
;;  (ivy-set-actions
;;   'ivy-switch-buffer
;;   '(("j" switch-to-buffer-other-frame "other frame")
;;     ("k" kill-buffer "kill")
;;     ("r" ivy--rename-buffer-action "rename")))

;; End Ivy, Swiper, Counsel


(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  )


(provide 'init-ivy)

;;; init-ivy.el ends here
