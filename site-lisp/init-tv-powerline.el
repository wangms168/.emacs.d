;;; Powerline
;;
(use-package powerline
  :config
  (progn
    (use-package helm-ls-git :ensure t)
    ;; Will appear in mode-line once helm-ls-git is loaded
    (defpowerline powerline-git
      (when (and (buffer-file-name (current-buffer))
                 (fboundp 'helm-ls-git--branch)
                 (helm-ls-git-root-dir))
        (if (and window-system (not powerline-gui-use-vcs-glyph))
            (format " Git:%s" (format-mode-line '(:eval (helm-ls-git--branch))))
          (format " %s%s"
                  (char-to-string #x2221) ; MEASURED ANGLE (∡)
                  (format-mode-line '(:eval (helm-ls-git--branch)))))))

    (setq powerline-gui-use-vcs-glyph t)
    
    (defun tv/powerline-default-theme ()
      "Setup the default mode-line."
      (interactive)
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (powerline-selected-window-active))
                              (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                              (mode-line (if active 'mode-line 'mode-line-inactive))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (separator-left (intern (format "powerline-%s-%s"
                                                              (powerline-current-separator)
                                                              (car powerline-default-separator-dir))))
                              (separator-right (intern (format "powerline-%s-%s"
                                                               (powerline-current-separator)
                                                               (cdr powerline-default-separator-dir))))
                              (lhs (list (powerline-raw mode-line-remote mode-line 'l)
                                         (powerline-raw "%*" mode-line 'l)
                                         (when powerline-display-buffer-size
                                           (powerline-buffer-size mode-line 'l))
                                         (when powerline-display-mule-info
                                           (powerline-raw mode-line-mule-info mode-line 'l))
                                         (powerline-buffer-id mode-line-buffer-id 'l)
                                         (when (and (boundp 'which-func-mode) which-func-mode)
                                           (powerline-raw which-func-format nil 'l))
                                         (powerline-raw " ")
                                         (funcall separator-left mode-line face1)
                                         (powerline-raw "%4l" face1 'l)
                                         (powerline-raw ":" face1 'l)
                                         (powerline-raw "%3c" face1 'r)
                                         (funcall separator-left face1 mode-line)
                                         (powerline-raw " ")
                                         (powerline-raw "%6p" mode-line 'r)
                                         (funcall separator-left mode-line face1)
                                         (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                           (powerline-raw erc-modified-channels-object face1 'l))
                                         (powerline-major-mode face1 'l)
                                         (powerline-process face1)
                                         (powerline-minor-modes face1 'l)
                                         (powerline-narrow face1 'l)
                                         (powerline-raw " " face1)
                                         (funcall separator-left face1 face2)
                                         (powerline-git face2 'r)
                                         (when (bound-and-true-p nyan-mode)
                                           (powerline-raw (list (nyan-create)) face2 'l))))
                              (rhs (list (powerline-raw global-mode-string face2 'r)
                                         (funcall separator-right face2 face1)
                                         (unless window-system
                                           (powerline-raw (char-to-string #xe0a1) face1 'l))
                                         (when powerline-display-hud
                                           (powerline-hud face2 face1)))))
                         (concat (powerline-render lhs)
                                 (powerline-fill face2 (powerline-width rhs))
                                 (powerline-render rhs)))))))
    (tv/powerline-default-theme)
    (global-set-key [mode-line mouse-1] 'ignore)
    (global-set-key [mode-line mouse-2] 'ignore)
    (global-set-key [mode-line mouse-3] 'ignore)
    (setq mode-line-default-help-echo nil)
    (add-hook 'focus-in-hook 'force-mode-line-update))
  :ensure t)

(provide 'init-tv-powerline)

;;; init-tv-powerline.el ends here
