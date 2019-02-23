;;; init-modeline.el --- modeline configurations.	-*- lexical-binding: t -*-

;; https://emacs-china.org/t/mode-line/6979

;; mode-line
(defun roife/ml/shortened-path (path max-len)
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun roife/ml/mode-info ()
  (let ((info (cond ((derived-mode-p 'calc-mode) (prin1-to-string calc-angle-mode))
                    (t nil))))
    (if info (concat " [" (propertize info 'face '(:foreground "#cc6666")) "]")))
  )

(defvar roife/ml/selected-window nil)
(add-hook 'post-command-hook '(lambda () (setq roife/ml/selected-window (selected-window))))
(add-hook 'buffer-list-update-hook '(lambda () (force-mode-line-update)))
(defun roife/ml/selected-window-p (x y)
  "Return X if the current window is selected, if not, return Y."
  (if (eq roife/ml/selected-window (selected-window)) x y))

(defun roife/ml/fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun roife/ml/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s " err))))

(defun roife/ml/compute-mode-line ()
  (let* ((left (list
                ;; winum
                '(:eval (propertize (concat " " (winum-get-number-string) " ")
                                    'face `(:weight bold
                                                    :background ,(roife/ml/selected-window-p "#f0c674" "#373b41")
                                                    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))
                                    ))
                ;; file
                " %* %I "
                '(:eval (propertize (if overwrite-mode "Ovr " "") 'face '(:foreground "#f0c674")))
                '(:eval (propertize
                         (if (and (buffer-file-name) (projectile-project-p))
                             (roife/ml/shortened-path (file-relative-name buffer-file-name (projectile-project-root)) 15)
			   "%b")
                         'face `(:weight ,(roife/ml/selected-window-p 'bold 'normal)
                                         :foreground ,(roife/ml/selected-window-p "#b5bd68" "#969896"))))
                " "

                ))

         (center (list
		  "wangms"
                  ;; projectile
                  '(:eval (when (and (buffer-file-name) (projectile-project-p))
                            (concat "["(propertize (projectile-project-name)
                                                   'face `(:foreground ,(roife/ml/selected-window-p "#81a2be" "#969896")))
                                    "] ")))
                  ;; major-mode
                  '(:eval (propertize "%m" 'face `(:foreground ,(roife/ml/selected-window-p "#b294bb" "#969896"))))
                  '(:eval (roife/ml/mode-info))
                  ;; flycheck
                  '(:eval
                    (when (and (bound-and-true-p flycheck-mode)
                               (or flycheck-current-errors
                                   (eq 'running flycheck-last-status-change)))
                      (concat
                       " "
                       (propertize " " 'face '(:background "#282a2e"))
                       (cl-loop for state in '((error . "#cc6666")
                                               (warning . "#de935f")
                                               (info . "#8abeb7"))
                                as lighter = (roife/ml/flycheck-lighter (car state))
                                when lighter
                                concat (propertize
                                        lighter
                                        'face `(:foreground ,(cdr state)
                                                            :background "#282a2e")))
                       )))
                  ;; git
                  '(:eval vc-mode)
                  ;; selected
                  " "
                  '(:eval (when (and (use-region-p) (roife/ml/selected-window-p t nil))
			    (concat
                             (propertize (format " C:%d W:%d L:%d "
                                                 (abs (- (mark t) (point)))
                                                 (count-words (region-beginning) (region-end))
                                                 (count-lines (region-beginning) (region-end)))
                                         'face '(:background "#969896" :foreground "#1D1F21"))
                             " ")))
                  ))
         (right (list
                 ;; encoding
                 '(:eval (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
				       (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
					   (match-string 1 buf-coding)
					 buf-coding))))
                 " "
                 ;; position
                 '(:eval (propertize " %l: %C " 'face `(:background ,(roife/ml/selected-window-p "#969896" "#373b41")
                                                                    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
                 " "
                 '(-3 "%p")
                 " "
                 ))
         (fill-space (roife/ml/fill 'mode-line (string-width (format-mode-line right))))
         (width-lcr (string-width (format-mode-line (list left center right))))
         (width-lr (string-width (format-mode-line (list left right))))
	 )
    (cond ((> width-lr (window-width)) (list left))
          ((> width-lcr (window-width)) (list left fill-space right))
          (t (list left center fill-space right)))
    ))
(setq-default mode-line-format '(:eval (roife/ml/compute-mode-line)))


(provide 'init-modeline-3)
;;; init-modeline.el ends here
