;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35
	  )
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    ;; https://blog.jft.rocks/emacs/treemacs-icons.html
    ;; ;; Improve treemacs icons
    ;; (with-eval-after-load 'treemacs
    ;;   (with-eval-after-load 'all-the-icons
    ;; 	(let ((all-the-icons-default-adjust 0)
    ;; 	      (tab-width 1))
    ;; 	  ;; Root icon
    ;; 	  (setq treemacs-icon-root-png
    ;; 		(concat (all-the-icons-octicon "repo" :height 0.8 :v-adjust -0.2)  " "))
    ;; 	  ;; File icons
    ;; 	  (setq treemacs-icon-open-png
    ;; 		(concat
    ;; 		 (all-the-icons-octicon "chevron-down" :height 0.8 :v-adjust 0.1)
    ;; 		 "\t"
    ;; 		 (all-the-icons-octicon "file-directory" :v-adjust 0)
    ;; 		 "\t")
    ;; 		treemacs-icon-closed-png
    ;; 		(concat
    ;; 		 (all-the-icons-octicon "chevron-right" :height 0.8
    ;; 					:v-adjust 0.1 :face 'font-lock-doc-face)
    ;; 		 "\t"
    ;; 		 (all-the-icons-octicon "file-directory" :v-adjust 0 :face 'font-lock-doc-face)
    ;; 		 "\t"))
    ;; 	  ;; File type icons
    ;; 	  (setq treemacs-icons-hash (make-hash-table :size 200 :test #'equal)
    ;; 		treemacs-icon-fallback (concat
    ;; 					"\t\t"
    ;; 					(all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver
    ;; 							      :height 0.8 :v-adjust 0.0)
    ;; 					"\t")
    ;; 		treemacs-icon-text treemacs-icon-fallback)

    ;; 	  (dolist (item all-the-icons-icon-alist)
    ;; 	    (let* ((extension (car item))
    ;; 		   (func (cadr item))
    ;; 		   (args (append (list (caddr item)) '(:v-adjust -0.05) (cdddr item)))
    ;; 		   (icon (apply func args))
    ;; 		   (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) extension))
    ;; 		   (value (concat "\t\t" icon "\t")))
    ;; 	      (unless (ht-get treemacs-icons-hash (s-replace-regexp "\\?" "" key))
    ;; 		(ht-set! treemacs-icons-hash (s-replace-regexp "\\?" "" key) value))
    ;; 	      (unless (ht-get treemacs-icons-hash (s-replace-regexp ".\\?" "" key))
    ;; 		(ht-set! treemacs-icons-hash (s-replace-regexp ".\\?" "" key) value)))))))
    )
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(provide 'init-treemacs)

;;; init-neotree.el ends here
