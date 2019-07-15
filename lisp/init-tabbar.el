;;; init-tabbar.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; tabbar
(use-package tabbar
  :ensure t
  :bind
  ("<C-S-iso-lefttab>" . tabbar-backward)
  ("<C-tab>" . tabbar-forward)
  :config
  (setq tabbar-background-color "#333333")
  (setq tabbar-separator '(0.3))

  (set-face-attribute
   'mode-line-inactive nil
   :box nil)

  (set-face-attribute
   'tabbar-separator nil
   :background "#333333"
   :box nil)

  (set-face-attribute
   'tabbar-default nil
   :height 1.0)

  (set-face-attribute
   'tabbar-button nil
   :box nil)

  (set-face-attribute
   'tabbar-highlight nil
   :foreground "black"
   :background "orange"
   :underline nil
   :box nil)

  (set-face-attribute
   'tabbar-unselected nil
   ;; :foreground "black"
   :background nil
   :box '(:line-width 1 :color "white" :style sunken)
   )

  (set-face-attribute
   'tabbar-selected nil
   ;; :foreground "block"
   :background "#BFBFBF"
   :box '(:line-width 1 :color "white" :style sunken)  ;;sunken
   )

  ;; (set-face-attribute
  ;;  'tabbar-modified nil
  ;;  :foreground "orange red"
  ;;  :background "gray25"
  ;;  :box '(:line-width 1 :color "gray19"))

  ;; (set-face-attribute
  ;;  'tabbar-selected-modified nil
  ;;  :foreground "orange red"
  ;;  :background "gray19"
  ;;  :box '(:line-width 1 :color "gray19")

  :init
  (tabbar-mode 1)
  )

;; https://emacs.stackexchange.com/questions/984/what-is-the-right-way-to-install-tab-bar
;; (use-package tabbar
;;   :ensure t
;;   :bind
;;   ("<C-S-iso-lefttab>" . tabbar-backward)
;;   ("<C-tab>" . tabbar-forward)

;;   :config
;;   (setq tabbar-background-color "#333333")
;;   (setq tabbar-separator '(0.2))

;;   (set-face-attribute
;;    'mode-line-inactive nil
;;    :box nil)
;;   (set-face-attribute
;;    'tabbar-default nil
;;    :height 1.0)
  
;;   (set-face-attribute
;;    'tabbar-button nil
;;    :box '(:line-width 1 :color "gray19"))

;;   (set-face-attribute
;;    'tabbar-selected nil
;;    :foreground "orange"
;;    :background "gray19"
;;    :box '(:line-width 1 :color "gray19"))

;;   (set-face-attribute
;;    'tabbar-unselected nil
;;    :foreground "gray75"
;;    :background "gray25"
;;    :box '(:line-width 1 :color "gray19"))

;;   (set-face-attribute
;;    'tabbar-highlight nil
;;    :foreground "black"
;;    :background "orange"
;;    :underline nil
;;    :box '(:line-width 1 :color "gray19" :style nil))

;;   (set-face-attribute
;;    'tabbar-modified nil
;;    :foreground "orange red"
;;    :background "gray25"
;;    :box '(:line-width 1 :color "gray19"))

;;   (set-face-attribute
;;    'tabbar-selected-modified nil
;;    :foreground "orange red"
;;    :background "gray19"
;;    :box '(:line-width 1 :color "gray19"))

;;   ;; Change padding of the tabs
;;   ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
;;   ;; (custom-set-variables
;;   ;;  '(tabbar-separator (quote (1.0))))
;;   (defun tabbar-buffer-tab-label (tab)
;;     "Return a label for TAB.
;;   That is, a string used to represent it on the tab bar."
;;     (let ((label  (if tabbar--buffer-show-groups
;;                       (format " [%s] " (tabbar-tab-tabset tab))
;;                     (format " %s " (tabbar-tab-value tab)))))
;;       ;; Unless the tab bar auto scrolls to keep the selected tab
;;       ;; visible, shorten the tab label to keep as many tabs as possible
;;       ;; in the visible area of the tab bar.
;;       (if tabbar-auto-scroll-flag
;;           label
;;         (tabbar-shorten
;;          label (max 1 (/ (window-width)
;;                          (length (tabbar-view
;;                                   (tabbar-current-tabset)))))))))

;;   (defun px-tabbar-buffer-select-tab (event tab)
;;     "On mouse EVENT, select TAB."
;;     (let ((mouse-button (event-basic-type event))
;;           (buffer (tabbar-tab-value tab)))
;;       (cond
;;        ((eq mouse-button 'mouse-2) (with-current-buffer buffer (kill-buffer)))
;;        ((eq mouse-button 'mouse-3) (pop-to-buffer buffer t))
;;        (t (switch-to-buffer buffer)))
;;       (tabbar-buffer-show-groups nil)))

;;   (defun px-tabbar-buffer-help-on-tab (tab)
;;     "Return the help string shown when mouse is onto TAB."
;;     (if tabbar--buffer-show-groups
;;         (let* ((tabset (tabbar-tab-tabset tab))
;;                (tab (tabbar-selected-tab tabset)))
;;           (format "mouse-1: switch to buffer %S in group [%s]"
;;                   (buffer-name (tabbar-tab-value tab)) tabset))
;;       (format "\
;; mouse-1: switch to %S\n\
;; mouse-2: kill %S\n\
;; mouse-3: Open %S in another window"
;;               (buffer-name (tabbar-tab-value tab))
;;               (buffer-name (tabbar-tab-value tab))
;;               (buffer-name (tabbar-tab-value tab)))))

;;   (defun px-tabbar-buffer-groups ()
;;     "Sort tab groups."
;;     (list (cond ((or
;;                   (eq major-mode 'dired-mode)
;;                   (string-equal "*" (substring (buffer-name) 0 1))) "emacs")
;;                 (t "user"))))
;;   (setq tabbar-help-on-tab-function 'px-tabbar-buffer-help-on-tab
;;         tabbar-select-tab-function 'px-tabbar-buffer-select-tab
;;         tabbar-buffer-groups-function 'px-tabbar-buffer-groups)

;;   :init
;;   (tabbar-mode 1))


;; (use-package awesome-tab
;;   :load-path "~/home/wangms/.emacs.d/default/site-lisp/awesome-tab/"
;;   ;; :straight (awesome-tab
;;   ;; 	     :type git
;;   ;; 	     :host github
;;   ;; 	     :repo "manateelazycat/awesome-tab")
;;   :config
;;   (setq awesome-tab-style 'alternate)
;;   (awesome-tab-mode t))


(provide 'init-tabbar)

;;; init-tabbar.el ends here
