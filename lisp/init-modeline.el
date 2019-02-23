;;; init-modeline.el --- modeline configurations.	-*- lexical-binding: t -*-

(require 'all-the-icons)

;; ========================================================================================================================
;; modeline各显示项目自定义
;; https://github.com/domtronn/all-the-icons.el/wiki/Spaceline
;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line

;; -------------------------------------------------------------------------------------------------------------------------
;; mode-line-format的mode-line-modes中的mode-name以图标显示
;; propertize中不用 (format "%s" xxx ) 则有错误提示：  Wrong type argument: stringp, minibuffer-inactive-mode
;; (add-hook 'buffer-list-update-hook
;; 	  (lambda () (setq mode-name (all-the-icons-icon-for-buffer))))
(add-hook 'buffer-list-update-hook
	  (lambda () (setq mode-name (propertize (format "%s" (all-the-icons-icon-for-buffer)) 'display '(raise 0.0)
						 'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer) :inherit))))) 

;; -------------------------------------------------------------------------------------------------------------------------
;; mode-line-modified状态图标显示
(defun custom-modified-icon ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.0 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.0 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.0 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit ))))

;; 修改或只读。片段在保存当前文件时显示链图标，在修改时显示断链，在文件只读时显示填充锁定。
;; (defun custom-modeline-modified
;;     ((let* ((config-alist
;;              '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
;;                ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
;;                ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
;;             (result (cdr (assoc (format-mode-line "%*") config-alist))))
;;        (propertize (apply (cadr result) (cddr result))
;;                    'face `(:family ,(funcall (car result)))))))

;; -------------------------------------------------------------------------------------------------------------------------
;; 窗口编号图标显示
(use-package  winum
  :ensure t
  :init
  (winum-mode)
  :config
  (progn
    (defun mapleline--unicode-number (str)
      "Return a nice unicode representation of a single-digit number STR."
      (cond
       ((string= "1" str) "➊")
       ((string= "2" str) "➋")
       ((string= "3" str) "➌")
       ((string= "4" str) "➍")
       ((string= "5" str) "➎")
       ((string= "6" str) "➏")
       ((string= "7" str) "➐")
       ((string= "8" str) "➑")
       ((string= "9" str) "➒")
       ((string= "10" str) "➓")
       (t str)))
    (defun custom-modeline-window-number ()
      (propertize (format "%c" (+ 9311 (winum-get-number)))
		  'face `(:height 1.0 :inherit)
		  'display '(raise -0.0))
      ;; :tight t :when (fboundp 'window-numbering-mode)
      )
    ))

;; (use-package  window-numbering
;;   :ensure t
;;   :config
;;   (window-numbering-mode)
;;   )
;; (defun custom-modeline-window-number ()
;;   (propertize (format "%c" (+ 9311 (window-numbering-get-number)))
;;               'face `(:height 1.2 :inherit)
;;               'display '(raise -0.0))
;;   ;; :tight t :when (fboundp 'window-numbering-mode)
;;   )

;; 窗口编号       此代码段显示当前窗口编号的数字图标。这适用于 Window Numbering 包。
;; (defun custom-modeline-window-number ()
;;   (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
;;               'face `(:height ,(/ (* 0.90 powerline/default-height) 100.0))
;;               'display '(raise 0.0)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Major-mode图标显示
(defun custom-modeline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (propertize icon
                  'help-echo (format "Major-mode: `%s`" major-mode)
                  'display '(raise 0.0)
                  ;; 'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))
		  'face `(:family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

;; 模式图标       此代码段显示该缓冲区文件模式的开发人员图标。
;; (defun custom-modeline-mode-icon ()
;;   (format " %s"
;; 	  (propertize icon
;;                       'help-echo (format "Major-mode: `%s`" major-mode)
;;                       'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

;; -------------------------------------------------------------------------------------------------------------------------
;; region-info及图标在modeline上显示
(defun custom-modeline-region-info ()
  ;; ati-region-info "An `all-the-icons' segment for the currently marked region"
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "%s " (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family) :inherit) 'display '(raise 0.1))
       (propertize (format "(%s, %s)" words chars)
                   'face `(:height 0.9 :inherit))))))

;; 区域标记     此片段显示有关当前标记区域的有用信息，即标记 的行数和字符数。
;; (defun custom-modeline-region-info ()
;;   (when mark-active
;;     (let ((words (count-lines (region-beginning) (region-end)))
;;           (chars (count-words (region-end) (region-beginning))))
;;       (concat
;;        (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
;;                    'face `(:family ,(all-the-icons-octicon-family))
;;                    'display '(raise -0.0))
;;        (propertize (format " (%s, %s)" words chars)
;;                    'face `(:height 0.9))))))

;; -------------------------------------------------------------------------------------------------------------------------
;; vc-mode 版本控制modeline显示
;; https://emacs.stackexchange.com/questions/10955/customize-vc-mode-appearance-in-mode-line
;; Define faces.
(defface my/mode:vc-added
  `(
    (  ((class color))
       (:background "#FFAA55"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that have just been added to
version-control."
  :group 'MY/mode)

(defface my/mode:vc-edited
  `(
    (  ((class color))
       (:background "#F05B80"  :foreground "black")  )   ; "#F04040" maybe?
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are under version control
but which have been edited."
  :group 'MY/mode)

(defface my/mode:vc-in-sync
  `(
    (  ((class color))
       (:background "#60CC60"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are under version control
and which are in sync with the respository."
  :group 'MY/mode)

(defface my/mode:vc-none
  `(
    (  ((class color))
       (:background "#70A0D0"  :foreground "black")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files that are not under version
control"
  :group 'MY/mode)

(defface my/mode:vc-unknown
  `(
    (  ((class color))
       (:background "#FF0000"  :foreground "white")  )
    (  t
       (:weight bold :underline t)  )
    )
  "VC status tag face for files whose version-control status
cannot be determined."
  :group 'MY/mode)

(defvar my-vc-mode-attrs
  '((""  . (" NoVC "  my/mode:vc-none))
    ("-" . (" VC = "  my/mode:vc-in-sync))
    (":" . (" VC > "  my/mode:vc-edited))
    ("@" . (" VC + "  my/mode:vc-added))
    ("?" . (" ?VC? "  my/mode:vc-unknown))
    )
  "Lookup table to translate vc-mode character into another string/face."
  )

;; This function helps me understand the version-control status.
(defun my-mode-line-vc-info ()
  "Return version-control status information about the file in
the current buffer, as a fontified string.

The mode-line variable `vc-mode' is nil if the file is not under
version control, and displays a hyphen or a colon depending on whether
the file has been modified since check-in.  I can never keep those
straight.

This function returns \"NoVC\" if the file is not under version 
control.  It displays a string with an = sign if the file is in sync
with its version control, and a string with a > sign if the file has
been modified since its last check-in."
  (let* ((class
          (cond
           ;; If not under version-control
           ((not vc-mode)
            "")

           ;; If under version-control decode the -:@ character
           ((string-match "\\` ?\\(?:CVS\\|Git\\)\\([-:@]\\)\\([^^:~ \x00-\x1F\\\\/]+\\)?" vc-mode)
            (match-string-no-properties 1 vc-mode))

           ;; Otherwise, indicate confusion
           (t
            "?")
           ))

         (branch
          ;; (if (any class '("-" ":" "@"))
          (concat " " (match-string-no-properties 2 vc-mode))
          ;; ""
	  ;; )
	  )

         ;; Fetch properties list for the class character above
         (props (cdr (assoc class my-vc-mode-attrs)))
         )

    (concat (propertize (car props) 'face (cadr props))
            branch)))



(defun spaceline---github-vc ()
  "Function to return the Spaceline formatted GIT Version Control text."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (all-the-icons-alltheicon "git") 'face '(:height 1.1 :inherit) 'display '(raise 0.1))
     (propertize " · ")
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
                 'display '(raise 0.2))
     (propertize (format " %s" branch) 'face `(:height 0.9 :inherit) 'display '(raise 0.2)))))

(defun spaceline---svn-vc ()
  "Function to return the Spaceline formatted SVN Version Control text."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-vc-icons ()
  ;;ati-vc-icon "An `all-the-icons' segment for the current Version Control icon"
  ;; (when vc-mode
  (cond ((string-match "Git[:-]" vc-mode) (spaceline---github-vc))
        ((string-match "SVN-" vc-mode) (spaceline---svn-vc))
        (t (propertize (format "%s" vc-mode))))
  ;; )
  ;; :when active
  )

;; 版本控制图标    此代码段显示有关当前缓冲区版本控制系统的信息。目前，它仅支持SVN和Git包含图标。
;; (defun -custom-modeline-github-vc ()
;;   (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
;;     (concat
;;      (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
;;      " · "
;;      (propertize (format "%s" (all-the-icons-octicon "git-branch"))
;;                  'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
;;                  'display '(raise -0.1))
;;      (propertize (format " %s" branch) 'face `(:height 0.9)))))

;; (defun -custom-modeline-svn-vc ()
;;   (let ((revision (cadr (split-string vc-mode "-"))))
;;     (concat
;;      (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
;;      (propertize (format " · %s" revision) 'face `(:height 0.9)))))

;; (defun custom-modeline-icon-vc ()
;;   (when vc-mode
;;     (cond
;;      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
;;      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
;;      (t (format "%s" vc-mode)))))

;; -------------------------------------------------------------------------------------------------------------------------
;; flycheck状态图标显示
(use-package  flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (progn
    (defun custom-modeline-flycheck-status ()
      (format-mode-line
       '(:eval
	 (pcase flycheck-last-status-change
	   (`finished (if flycheck-current-errors
			  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
					 (+ (or .warning 0) (or .error 0)))))
			    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))))
			(propertize "✔ No Issues")))
	   (`running (propertize "⟲ Running..."))
	   (`no-checker (propertize "⚠ No Checker"))
	   (`not-checked "✖ Disabled")
	   (`errored (propertize "⚠ Error"))
	   (`interrupted "⛔ Interrupted")))))
    ))

;; Flycheck Checker信息    此代码段显示有关在当前缓冲区上运行Flycheck的结果的信息 。它实际上并没有使用，all-the-icons但确实看起来更好，并且可以使用它。
;; (defun custom-modeline-flycheck-status ()
;;   (let* ((text (pcase flycheck-last-status-change
;;                  (`finished (if flycheck-current-errors
;; 				(let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
;;                                                (+ (or .warning 0) (or .error 0)))))
;;                                   (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
;;                               "✔ No Issues"))
;;                  (`running     "⟲ Running")
;;                  (`no-checker  "⚠ No Checker")
;;                  (`not-checked "✖ Disabled")
;;                  (`errored     "⚠ Error")
;;                  (`interrupted "⛔ Interrupted")
;;                  (`suspicious  ""))))
;;     (propertize text
;;                 'help-echo "Show Flycheck Errors"
;;                 'mouse-face '(:box 1)
;;                 'local-map (make-mode-line-mouse-map
;;                             'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))))

;; -------------------------------------------------------------------------------------------------------------------------
;; 包更新图标
(defvar spaceline--upgrades nil)
(defun spaceline--count-upgrades ()
  "Function to count the number of package upgrades needed."
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq spaceline--upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'spaceline--count-upgrades)
(defun custom-modeline-package-updates ()
  ;; ati-package-updates "An `all-the-icons' spaceline segment to indicate number of package updates needed"
  (let ((num (or spaceline--upgrades (spaceline--count-upgrades))))
    (propertize
     (concat
      (propertize (format "%s" (all-the-icons-octicon "package"))
                  'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
                  'display '(raise 0.0))
      (propertize (format " %d updates " num) 'face `(:height 1.0 :inherit) 'display '(raise 0.0)))
     'help-echo "Open Packages Menu"
     'mouse-face '(:box 1)
     'local-map (make-mode-line-mouse-map
                 'mouse-1 (lambda () (interactive) (package-list-packages)))))
  ;; :when (and active (> (or spaceline--upgrades (spaceline--count-upgrades)) 0))
  )

;; 要更新的包数     此代码段显示您上次需要更新的软件包数。每次刷新包归档列表时，这当前都有效，因此数字很快就会过时。
;; (defvar powerline/upgrades nil)
;; (defun powerline/count-upgrades ()
;;   (let ((buf (current-buffer)))
;;     (package-list-packages-no-fetch)
;;     (with-current-buffer "*Packages*"
;;       (setq powerline/upgrades (length (package-menu--find-upgrades))))
;;     (switch-to-buffer buf)))
;; (advice-add 'package-menu-execute :after 'powerline/count-upgrades)

;; (defun custom-modeline-package-updates ()
;;   (let ((num (or powerline/upgrades (powerline/count-upgrades))))
;;     (when (> num 0)
;;       (propertize
;;        (concat
;;         (propertize (format "%s" (all-the-icons-octicon "package"))
;;                     'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
;;                     'display '(raise -0.1))
;;         (propertize (format " %d updates " num)
;;                     'face `(:height 0.9)))
;;        'help-echo "Open Packages Menu"
;;        'mouse-face '(:box 1)
;;        'local-map (make-mode-line-mouse-map
;;                    'mouse-1 (lambda () (interactive) (package-list-packages)))))))

;; -------------------------------------------------------------------------------------------------------------------------
;; yahoo-weather雅虎天气modeline显示
(use-package  yahoo-weather
  :ensure t
  :init
  (yahoo-weather-mode)
  ;; (setq yahoo-weather-guess-location-function #'yahoo-weather-ipinfo)
  :config
  (progn
    (defun custom-modeline-Weather-icons ()
      (let* ((weather (yahoo-weather-info-format 'yahoo-weather-info "%(weather)"))
             ;; (temp (spaceline--get-temp)	      )
             ;; (help (concat "Weather is '" weather "' and the temperature is " temp))
             (icon (all-the-icons-icon-for-weather (downcase weather))))
	(concat
	 ;; (if (> (length icon) 1)
	 ;;     (propertize icon 'help-echo help 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
	 (propertize icon
                     ;; 'help-echo help
                     'face `(:height 0.9 :family ,(all-the-icons-wicon-family) :inherit)
                     'display '(raise 0.0))
	 ;; )
	 ;; (propertize " " 'help-echo help)
	 ;; (propertize (spaceline--get-temp) 'face '(:height 0.9 :inherit) 'help-echo help)
	 ))
      ;; :when (and active (boundp 'yahoo-weather-info) yahoo-weather-mode)
      ;; :enabled nil
      ;; :tight t
      )    
    )
  )

;; 天气摘要     此片段显示当前时间，日出，日落，当前天气和当前温度。它使用（并依赖于）在 雅虎天气包。
;; (defun custom-modeline-suntime ()
;;   (if (and (boundp 'yahoo-weather-info) yahoo-weather-mode)
;;       (concat
;;        (format "%s "(yahoo-weather-info-format yahoo-weather-info "%(sunrise-time)"))
;;        (format "%s  " (all-the-icons-wicon "sunrise" :height 0.5 :v-adjust -0.1)) 
;;        (format "%s "(yahoo-weather-info-format yahoo-weather-info "%(sunset-time)")) 
;;        (format "%s "(all-the-icons-wicon "sunset" :height 0.5 :v-adjust -0.1)))
;;     ""))

;; (defun custom-modeline-weather ()
;;   (if (and (boundp 'yahoo-weather-info) yahoo-weather-mode)
;;       (let* ((weather (yahoo-weather-info-format yahoo-weather-info format))
;;              (icon (all-the-icons-icon-for-weather (downcase weather)))
;;              (family (if (> (length icon) 2)
;;                          (face-attribute 'default :family)
;;                        (all-the-icons-wicon-family))))
;;         (propertize (format " %s " icon)
;;                     'help-echo weather
;;                     'face `(:height 1.0 :family ,family)
;;                     'display '(raise 0.1)))
;;     ""))

;; -------------------------------------------------------------------------------------------------------------------------
;; 时间图标显示
(defun custom-modeline-time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.0 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string " %H:%M") 'face `(:height 1.0))
     ;; (propertize icon 'face `(:height 0.77 :family ,(all-the-icons-wicon-family)) 'display '(raise 0.1))
     )))

;; 时间与图标时钟    此片段以一个小时钟图标显示当前时间，该图标表示当前小时（即分针不移动的钟面）
;; (defun custom-modeline-time ()
;;   (let* ((hour (string-to-number (format-time-string "%I")))
;;          (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
;;     (concat
;;      (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
;;      (propertize (format "%s " icon) 'face `(:height 1.0 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

;; -------------------------------------------------------------------------------------------------------------------------
;; mode-line-process
(defun custom-process-icon ()
  ;; ati-process "An `all-the-icons' segment for the current process"
  (let ((icon (all-the-icons-icon-for-buffer)))
    (concat
     (when (or (symbolp icon) mode-line-process)
       (propertize (format-mode-line "%m") 'face `(:height 0.8 :inherit) 'display '(raise 0.2)))
     (when mode-line-process
       (propertize (format-mode-line mode-line-process) 'face '(:height 0.7 :inherit) 'display '(raise 0.2)))))
  ;; :tight t
  )

;; projectile
(defun custom-projectile-icon ()
  ;; ati-projectile "An `all-the-icons' segment for current `projectile' project"
  (concat
   (propertize "|" 'face '(:height 1.0 :inherit))
   " "
   (if (and (fboundp 'projectile-project-name)
            (projectile-project-name))
       (propertize (format "%s" (concat (projectile-project-name) ))
                   'face '(:height 1.0 :inherit)
                   'display '(raise 0.2)
                   'help-echo "Switch Project"
                   'mouse-face '(:box 1)
                   'local-map (make-mode-line-mouse-map
                               'mouse-1 (lambda () (interactive) (projectile-switch-project))))
     (propertize "×" 'face '(:height 1.0 :inherit)))
   " "
   (propertize "|" 'face '(:height 1.0 :inherit)))
  ;; :tight t
  )

;; buffer-id
(defun custom-buffer-id-icon ()
  ;; ati-buffer-id "An `all-the-icons' segment for the current buffer id"
  (if (fboundp 'projectile-project-root)
      (let* ((buf (or (buffer-file-name) (buffer-name)))
             (proj (ignore-errors (projectile-project-root)) )
             (name (if (buffer-file-name)
                       (or (cadr (split-string buf proj))
                           (format-mode-line "%b"))
                     (format-mode-line "%b"))))
	(propertize (format "%s" name)
                    'face `(:height 1.0 :inherit)
                    'display '(raise 0.2)
                    'help-echo (format "Major-mode: `%s`" major-mode)))
    (propertize (format-mode-line "%b ") 'face '(:height 1.0 :inherit) 'display '(raise 0.0)))
  ;; :tight t
  )

(defun replace-buffer-encoding ()
  "Display the encoding and eol style of the buffer the same way atom does."
  (propertize
   (concat (pcase (coding-system-eol-type buffer-file-coding-system)
	     (0 " LF")
	     (1 " RLF")
	     (2 " CR"))
	   (let ((sys (coding-system-plist buffer-file-coding-system)))
	     (cond ((memq (plist-get sys :category)
			  '(coding-category-undecided coding-category-utf-8))
		    " UTF-8")
		   (t (upcase (symbol-name (plist-get sys :name))))))
	   " ")))

;; (setq-default mode-line-format '("%e" (:eval 
;; 			       (concat
;; 				(custom-modeline-modified)
;; 				(custom-modeline-window-number)
;; 				(custom-modeline-mode-icon)
;; 				(custom-modeline-icon-vc)
;; 				(custom-modeline-region-info)
;; 				(custom-modeline-flycheck-status)
;; 				(custom-modeline-suntime)
;; 				(custom-modeline-weather)
;; 				(custom-modeline-time)))))


;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line-read-only-face nil
		    :inherit 'mode-line-face
		    :foreground "#4271ae"
		  ;;  :box '(:line-width nil :color "#4271ae")
)
(set-face-attribute 'mode-line-modified-face nil
		    :inherit 'mode-line-face
		    :foreground "#c82829"
		    :background "#ffffff"
		  ;;  :box '(:line-width nil :color "#c82829")
)
(set-face-attribute 'mode-line-80col-face nil
		    :inherit 'mode-line-position-face
		    :foreground "black"
		    :background "#eab700")

;; ========================================================================================================================
;; 1、左中右分布网上寻得框架-1
;; https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line
(defun mode-line-fill-right (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun mode-line-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))

(defconst RIGHT_PADDING 1)

(defun reserve-left/middle ()
  (/ (length (format-mode-line mode-line-align-middle)) 2))

(defun reserve-middle/right ()
  (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

;; https://emacs-china.org/t/mode-line/6979
(defvar roife/ml/selected-window nil)
(add-hook 'post-command-hook '(lambda () (setq roife/ml/selected-window (selected-window))))
(add-hook 'buffer-list-update-hook '(lambda () (force-mode-line-update)))
(defun roife/ml/selected-window-p (x y)
  "Return X if the current window is selected, if not, return Y."
  (if (eq roife/ml/selected-window (selected-window)) x y))


;; 左中右各部分自定义
(setq-default mode-line-align-left
	      '("%e"                                          ;; %e 当Emacs对于Lisp对象几乎没有内存时，会发出一条简短的消息。否则，这是空的。
	      	mode-line-front-space                       ;; 模式行前置空间:该变量显示在模式行的前面。默认情况下，此构造显示在模式行的开头，但如果存在满内存消息，则首先显示该构造。
	      	;; winum
	      	" "
	      	(:eval (propertize (mapleline--unicode-number (winum-get-number-string)) 'face `(:height 1.0) 'display '(raise -0.0)))     ;; for winum
	      	;; '(:eval (propertize (mapleline--unicode-number (int-to-string (window-numbering-get-number)))))                          ;; for window-numbering         
	      	;; '(:eval  (propertize (custom-modeline-window-number) 'face `(:height 1.0) 'display '(raise -0.0)))
	      	;; 图标插入测试
 	      	" |" 
	      	(:eval (propertize (all-the-icons-octicon "package")               'face `(:family ,(all-the-icons-octicon-family) :height 1.0) 'display '(raise -0.0)))
	      	(:eval (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:family ,(all-the-icons-alltheicon-family) :height 1.0) 'display '(raise -0.0)))
	      	;; 先eval再propertize，至少对于all-the-icons不如先propertize再eval这种写法。
	      	;; (:propertize (:eval (all-the-icons-alltheicon "git")) 'face `(:family ,(all-the-icons-alltheicon-family) :height 1.0) 'display '(raise -0.0))      
	      	(:eval (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.0) 'display '(raise -0.0)))
	      	(propertize " " )
	      	;; buffer大小 
	      	"|" 
	      	(:eval (propertize (format-mode-line "%I") 'face `(:height 1.0 :inherit) 'display '(raise 0.0)))
	      	;; 
	      	"|" 
	      	mode-line-mule-info ;; 此变量保存模式行构造的值，该构造显示有关语言环境，缓冲区编码系统和当前输入方法的信息。请参阅非ASCII字符。一般显示为‘U:’,XX快捷键触发后显示‘xx’汉字。
       	      	mode-line-client    ;; 此变量用于标识emacsclient帧。emacsclient -cn时显示“@”。
       	      	mode-line-modified  ;; 模式行修改:此变量保存模式行构造的值，该构造显示当前缓冲区是否已修改。其默认值显示'**'如果缓冲区被修改，'--'如果没有修改缓冲区，'%%'如果缓冲区是只读的，'％*'如果缓冲区是只读和修改的。
       	      	mode-line-remote    ;; 此变量用于显示default-directory当前缓冲区是否为远程缓冲区,若不是远程缓冲区，则显示为'-'。
	      	"|"
	      	(:eval (custom-modified-icon))
		(:eval (cond (buffer-read-only
		       (propertize "RO" 'face 'mode-line-read-only-face))
		      ((buffer-modified-p)
		       (propertize "**" 'face 'mode-line-modified-face))
		      (t "  ")))
		"|" 
		mode-line-frame-identification    ;; 模式行帧识别:此变量标识当前帧。如果您正在使用可以显示多个帧的窗口系统,其默认值显示" "，或者在一次只显示一个帧的普通终端上，显示"-%F " 。
	      	"|" 
	      	;; (propertize (shorten-directory default-directory 30) face font-lock-string-face)
       	      	;; mode-line-buffer-identification   ;; 模式行缓冲区识别:此变量标识窗口中显示的缓冲区。其默认值显示缓冲区名称，用空格填充至少12列。
	      	(:eval (custom-buffer-id-icon))
	      	(:eval (custom-modeline-mode-icon))
	      	(:eval (custom-process-icon))
	      	(:eval (custom-projectile-icon))
	      	)
	      )

(setq-default mode-line-align-middle
	      '("%e"                                ;; %e 当Emacs对于Lisp对象几乎没有内存时，会发出一条简短的消息。否则，这是空的。
	      	mode-line-modes ;; 模式线模式:此变量显示缓冲区的主要和次要模式。其默认值还显示递归编辑级别，有关进程状态的信息以及缩小是否有效。其中含有：mode-name、mode-line-process、minor-mode-alist。
	      	(:eval (custom-modeline-region-info))
	      	)
	      )

(setq-default mode-line-align-right
	      '("%e"                             ;; %e 当Emacs对于Lisp对象几乎没有内存时，会发出一条简短的消息。否则，这是空的。
	      	(:eval (my-mode-line-vc-info))
	      	(:eval (propertize (format " %s " (custom-modeline-flycheck-status)) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	      												 :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	      	(:eval (format " %s" (custom-modeline-package-updates)))
	      	;; (:eval (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
       	      	;; 		     (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
       	      	;; 			 (format " %s " (match-string 1 buf-coding))
       	      	;; 		       (format " %s " buf-coding))) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	      	;; 		     :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")) 'display '(raise +0.0)))
	      	(:eval (propertize (format "%s" (replace-buffer-encoding)) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	      											:foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	      	(:eval (custom-modeline-time))
	      	;; (:eval (propertize  (custom-modeline-time) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
	      	;; 								       :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	      	" "
	      	;; (:eval (propertize (format-mode-line " %l:%c ") 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")              ;; :inherit nil与:inherit不用哦
       	      	;;   								    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	      	(:eval (propertize (format-mode-line " %l:") 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")              ;; :inherit nil与:inherit不用哦
       	      	  								    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
		(:eval (propertize "%c" 'face
			    (if (>= (current-column) 80)
				'mode-line-80col-face
			      `(:background ,(roife/ml/selected-window-p "#787879" "#373b41") 
       	      	  			    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")))))
		(:eval (propertize (format "|%s" (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p")))) 'face
	      			   `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	      	  				 :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	      	;; mode-line-position     ;; 模式行位置:此变量指示缓冲区中的位置。其默认值显示缓冲区百分比，以及可选的缓冲区大小，行号和列号。显示形如：'xx% (xx,xx)'
	      	;; (:eval (propertize (format "  %s" (format-mode-line mode-line-position)) 'face `(:height 1.0 :background ,(roife/ml/selected-window-p "#787879" "#373b41")
	      	;; 											     :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")) ))     
	      	mode-line-end-spaces              ;; 模式行结束空间:该变量显示在模式行的末尾。
	      	)
	      )

(defun customize-mode-line-format-1 ()
  (let* (
	 (active (powerline-selected-window-active))
         (background (if active "#787879" "#373b41"))
	 (foreground (if active "#1d1f21" "#c5c8c6"))

	 (lhs '( "%e"
		 mode-line-front-space                       ;; 模式行前置空间:该变量显示在模式行的前面。默认情况下，此构造显示在模式行的开头，但如果存在满内存消息，则首先显示该构造。
		 ;; winum
		 " "
		 (:eval (propertize (mapleline--unicode-number (winum-get-number-string)) 'face `(:height 1.0) 'display '(raise -0.0)))     ;; for winum
		 ;; '(:eval (propertize (mapleline--unicode-number (int-to-string (window-numbering-get-number)))))                          ;; for window-numbering         
		 ;; '(:eval  (propertize (custom-modeline-window-number) 'face `(:height 1.0) 'display '(raise -0.0)))
		 ;; 图标插入测试
 		 " |" 
		 (:eval (propertize (all-the-icons-octicon "package")               'face `(:family ,(all-the-icons-octicon-family) :height 1.0) 'display '(raise -0.0)))
		 (:eval (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:family ,(all-the-icons-alltheicon-family) :height 1.0) 'display '(raise -0.0)))
		 ;; 先eval再propertize，至少对于all-the-icons不如先propertize再eval这种写法。
		 ;; (:propertize (:eval (all-the-icons-alltheicon "git")) 'face `(:family ,(all-the-icons-alltheicon-family) :height 1.0) 'display '(raise -0.0))      
		 (:eval (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.0) 'display '(raise -0.0)))
		 (propertize " " )
		 ;; buffer大小 
		 "|" 
		 (:eval (propertize (format-mode-line "%I") 'face `(:height 1.0 :inherit) 'display '(raise 0.0)))
		 ;; 
		 "|" 
		 mode-line-mule-info ;; 此变量保存模式行构造的值，该构造显示有关语言环境，缓冲区编码系统和当前输入方法的信息。请参阅非ASCII字符。一般显示为‘U:’,XX快捷键触发后显示‘xx’汉字。
       		 mode-line-client    ;; 此变量用于标识emacsclient帧。emacsclient -cn时显示“@”。
       		 mode-line-modified  ;; 模式行修改:此变量保存模式行构造的值，该构造显示当前缓冲区是否已修改。其默认值显示'**'如果缓冲区被修改，'--'如果没有修改缓冲区，'%%'如果缓冲区是只读的，'％*'如果缓冲区是只读和修改的。
       		 mode-line-remote    ;; 此变量用于显示default-directory当前缓冲区是否为远程缓冲区,若不是远程缓冲区，则显示为'-'。
		 "|"
		 (:eval (custom-modified-icon))
		 (:eval (cond (buffer-read-only
	       		       (propertize "RO" 'face 'mode-line-read-only-face))
	       		      ((buffer-modified-p)
	       		       (propertize "**" 'face 'mode-line-modified-face))
	       		      (t "  ")))
		 "|" 
		 ;; mode-line-frame-identification    ;; 模式行帧识别:此变量标识当前帧。如果您正在使用可以显示多个帧的窗口系统,其默认值显示" "，或者在一次只显示一个帧的普通终端上，显示"-%F " 。
		 "|" 
		 ;; (propertize (shorten-directory default-directory 30) face font-lock-string-face)
       		 ;; mode-line-buffer-identification   ;; 模式行缓冲区识别:此变量标识窗口中显示的缓冲区。其默认值显示缓冲区名称，用空格填充至少12列。
		 (:eval (custom-buffer-id-icon))
		 (:eval (custom-modeline-mode-icon))
		 (:eval (custom-process-icon))
		 (:eval (custom-projectile-icon))
		 ))
	 (center '("%e"
	      	   mode-line-modes ;; 模式线模式:此变量显示缓冲区的主要和次要模式。其默认值还显示递归编辑级别，有关进程状态的信息以及缩小是否有效。其中含有：mode-name、mode-line-process、minor-mode-alist。
	      	   (:eval (custom-modeline-region-info))
		   ))
	 (rhs '("%e"
		"wms"
		"%*"
		(:eval (:propertize (:eval (custom-modeline-flycheck-status)) 'face `(:background ,background :foreground ,foreground)))
		;; (:eval (propertize (format " %s " (custom-modeline-flycheck-status)) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       		;;      											 :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))

		;; `(:propertize ,background) 
		" "
		;; `("wangms-" ,background)
		;; (:propertize (:eval (custom-modeline-flycheck-status)) 'face `(:background ,background :foreground ,foreground))
		;; (:eval (propertize (format " %s " (custom-modeline-flycheck-status)) 'face `(:background ,background :foreground ,foreground)))
		))
	 ;; (rhs '("%e"
	 ;; 	(:eval (my-mode-line-vc-info))
	 ;; 	(:eval (propertize (format " %s " (custom-modeline-flycheck-status)) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	 ;;      												 :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	 ;; 	(:eval (format " %s" (custom-modeline-package-updates)))
	 ;; 	;; (:eval (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
       	 ;; 	;; 		     (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
       	 ;; 	;; 			 (format " %s " (match-string 1 buf-coding))
       	 ;; 	;; 		       (format " %s " buf-coding))) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	 ;; 	;; 		     :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")) 'display '(raise +0.0)))
	 ;; 	(:eval (propertize (format "%s" (replace-buffer-encoding)) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	 ;;      										       :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	 ;; 	(:eval (custom-modeline-time))
	 ;; 	;; (:eval (propertize  (custom-modeline-time) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
	 ;; 	;; 								       :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	 ;; 	" "
	 ;; 	;; (:eval (propertize (format-mode-line " %l:%c ") 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")              ;; :inherit nil与:inherit不用哦
       	 ;; 	;;   								    :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	 ;; 	(:eval (propertize (format-mode-line " %l:") 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")              ;; :inherit nil与:inherit不用哦
       	 ;;      	  								 :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	 ;; 	(:eval (propertize "%c" 'face
	 ;; 			   (if (>= (current-column) 80)
	 ;; 			       'mode-line-80col-face
	 ;; 			     `(:background ,(roife/ml/selected-window-p "#787879" "#373b41") 
       	 ;;      	  				   :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")))))
	 ;; 	(:eval (propertize (format "|%s" (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p")))) 'face
	 ;;      			   `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	 ;;      	  				 :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6"))))
	 ;; 	;; mode-line-position     ;; 模式行位置:此变量指示缓冲区中的位置。其默认值显示缓冲区百分比，以及可选的缓冲区大小，行号和列号。显示形如：'xx% (xx,xx)'
	 ;; 	;; (:eval (propertize (format "  %s" (format-mode-line mode-line-position)) 'face `(:height 1.0 :background ,(roife/ml/selected-window-p "#787879" "#373b41")
	 ;; 	;; 											     :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")) ))     
	 ;; 	mode-line-end-spaces              ;; 模式行结束空间:该变量显示在模式行的末尾。
	 ;; 	)
	 ;;      )
	 
	 ;; (rhs '(   ;;"%e"
	 ;;       (:eval (my-mode-line-vc-info))
	 ;;       (:eval (propertize (format " %s " (custom-modeline-flycheck-status)) 'face `(:background ,background :foreground ,foreground)))
	 ;;       ;; (:eval (format " %s" (custom-modeline-package-updates)))
	 ;;       ;; ;; (:eval (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
       	 ;;       ;; ;; 		     (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
       	 ;;       ;; ;; 			 (format " %s " (match-string 1 buf-coding))
       	 ;;       ;; ;; 		       (format " %s " buf-coding))) 'face `(:background ,(roife/ml/selected-window-p "#787879" "#373b41")
       	 ;;       ;; ;; 		     :foreground ,(roife/ml/selected-window-p "#1d1f21" "#c5c8c6")) 'display '(raise +0.0)))
	 ;;       ;; (:eval (propertize (format "%s" (replace-buffer-encoding)) 'face `(:background ,background :foreground ,foreground)))
	 ;;       ;; (:eval (custom-modeline-time))
	 ;;       ;; ;; (:eval (propertize  (custom-modeline-time) 'face `(:background ,background :foreground ,foreground)))
	 ;;       ;; " "
	 ;;       ;; ;; (:eval (propertize (format-mode-line " %l:%c ") 'face `(:background ,background :foreground ,foreground)))
	 ;;       ;; (:eval (propertize (format-mode-line " %l:") 'face `(:background ,background :foreground ,foreground)))
	 ;;       ;; (:eval (propertize "%c" 'face
	 ;;       ;; 			  (if (>= (current-column) 80)
	 ;;       ;; 			      'mode-line-80col-face
	 ;;       ;; 			    `(:background ,background :foreground ,foreground))))
	 ;;       ;; (:eval (propertize (format "|%s" (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p")))) 'face
	 ;;       ;; 			  `(:background ,background :foreground ,foreground)))
	 ;;       ;; ;; mode-line-position     ;; 模式行位置:此变量指示缓冲区中的位置。其默认值显示缓冲区百分比，以及可选的缓冲区大小，行号和列号。显示形如：'xx% (xx,xx)'
	 ;;       ;; (:eval (propertize (format "  %s" (format-mode-line mode-line-position)) 'face `(:background ,background :foreground ,foreground)))     
	 ;;       ;; mode-line-end-spaces              ;; 模式行结束空间:该变量显示在模式行的末尾。
	 ;;       )
	 ;;      )
	 

         (width (string-width (format-mode-line (list lhs center rhs))))
	 )
    ;; (if (> width (window-width))
    ;;     (list
    ;;      lhs
    ;; 	 '(:eval (mode-line-fill-right 
    ;; 		  (reserve-middle/right)))
    ;;      rhs
    ;;      )
    ;;   (list
    ;;    lhs
    ;;    '(:eval (mode-line-fill-center 'mode-line
    ;;                                   (reserve-left/middle)))
    ;;    center
    ;;    '(:eval
    ;;      (mode-line-fill-right 'mode-line
    ;;                            (reserve-middle/right)))
    ;;    rhs
    ;;    )
    ;;   )
    (list
     lhs
     '(:eval (mode-line-fill-center 'mode-line
                                    (reserve-left/middle)))
     center
     '(:eval
       (mode-line-fill-right 'mode-line
                             (reserve-middle/right)))
     rhs
     )
    ))

;; ========================================================================================================================
;; 2、左中右分布网上寻得框架-2
(defun jordon-fancy-mode-line-render (left center right &optional lpad rpad)
  "Return a string the width of the current window with 
LEFT, CENTER, and RIGHT spaced out accordingly, LPAD and RPAD,
can be used to add a number of spaces to the front and back of the string."
       (condition-case err
	   (let* ((left (if lpad (concat (make-string lpad ?\s) left) left))
		  (right (if rpad (concat right (make-string rpad ?\s)) right))
		  (width (apply '+ (window-width) (let ((m (window-margins))) (list (or (car m) 0) (or (cdr m) 0)))))
		  (total-length (+ (length left) (length center) (length right) 2)))
             ;; (when (> total-length width) (setq left "" right ""))
             (when (> total-length width) (setq center ""))
             (let* ((left-space (/ (- width (length center)) 2))
		    (right-space (- width left-space (length center)))
		    (lspaces (max (- left-space (length left)) 1))
		    (rspaces (max (- right-space (length right)) 1 0)))
               ;; (concat left (make-string lspaces  ?\s)
               ;;         center
               ;;         (make-string rspaces ?\s)
               ;;         right)))
	       (list left (make-string lspaces  ?\s)
		       center
		       (make-string rspaces ?\s)
		       right)))

	 (error (format "[%s]: (%s) (%s) (%s)" err left center right))))

;; (defun customize-mode-line-format-2 ()
;;   '((:eval (jordon-fancy-mode-line-render
;;             (format-mode-line (format " %s (%%l/%d) %%c "
;;                                       (downcase (format-mode-line mode-name))
;;                                       (line-number-at-pos (point-max))))
;;             (concat (buffer-name)
;;                     (cond
;;                      ((not (buffer-file-name)) " ")
;;                      ((buffer-modified-p) "*")
;;                      (t " ")))
;;             " " 1 3           ))))

;; (defun customize-mode-line-format-2 ()
;;   '((:eval (jordon-fancy-mode-line-render
;; 	    (format-mode-line mode-line-align-left) 
;; 	    (format-mode-line mode-line-align-middle) 
;; 	    (format-mode-line mode-line-align-right) 
;;             ))))


(defvar powerline-selected-window (frame-selected-window)
  "Selected window.")
(defun powerline-selected-window-active ()
  "Return whether the current window is active."
  (eq powerline-selected-window (selected-window)))

(defun customize-mode-line-format-2 (&optional lpad rpad)
  (let* ((active (powerline-selected-window-active))
         (background (if active "#787879" "#373b41"))
         (foreground (if active "#1d1f21" "#c5c8c6"))
	 (lhs (concat
	       (format-mode-line mode-line-front-space)                        ;; 模式行前置空间:该变量显示在模式行的前面。默认情况下，此构造显示在模式行的开头，但如果存在满内存消息，则首先显示该构造。
	       ;; winum
	       " "
	       (propertize (mapleline--unicode-number (winum-get-number-string)) 'face `(:height 1.0) 'display '(raise -0.0))     ;; for winum
	       ;; '(:eval (propertize (mapleline--unicode-number (int-to-string (window-numbering-get-number)))))                          ;; for window-numbering         
	       ;; '(:eval  (propertize (custom-modeline-window-number) 'face `(:height 1.0) 'display '(raise -0.0)))
	       ;; 图标插入测试
 	       " |" 
	       (propertize (all-the-icons-octicon "package") 'face `(:family ,(all-the-icons-octicon-family) :height 1.0) 'display '(raise -0.0))
	       (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:family ,(all-the-icons-alltheicon-family) :height 1.0) 'display '(raise -0.0))
	       ;; 先eval再propertize，至少对于all-the-icons不如先propertize再eval这种写法。
	       ;; (:propertize (:eval (all-the-icons-alltheicon "git")) 'face `(:family ,(all-the-icons-alltheicon-family) :height 1.0) 'display '(raise -0.0))      
	       (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.0) 'display '(raise -0.0))
	       (propertize " " )
	       ;; buffer大小 
	       "|" 
	       (propertize (format-mode-line "%I") 'face `(:height 1.0 :inherit) 'display '(raise 0.0))
	       ;; 
	       "|" 
	       (format-mode-line mode-line-mule-info)    ;; 此变量保存模式行构造的值，该构造显示有关语言环境，缓冲区编码系统和当前输入方法的信息。请参阅非ASCII字符。一般显示为‘U:’,XX快捷键触发后显示‘xx’汉字。
       	       (format-mode-line mode-line-client)       ;; 此变量用于标识emacsclient帧。emacsclient -cn时显示“@”。
       	       (format-mode-line mode-line-modified)     ;; 模式行修改:此变量保存模式行构造的值，该构造显示当前缓冲区是否已修改。其默认值显示'**'如果缓冲区被修改，'--'如果没有修改缓冲区，'%%'如果缓冲区是只读的，'％*'如果缓冲区是只读和修改的。
       	       (format-mode-line mode-line-remote)       ;; 此变量用于显示default-directory当前缓冲区是否为远程缓冲区,若不是远程缓冲区，则显示为'-'。
	       "|" 
	       (custom-modified-icon)
	       (cond (buffer-read-only
		      (propertize "RO" 'face 'mode-line-read-only-face))
		     ((buffer-modified-p)
		      (propertize "**" 'face 'mode-line-modified-face))
		     (t "  "))
	       "|" 
       	       (format-mode-line mode-line-frame-identification)     ;; 模式行帧识别:此变量标识当前帧。如果您正在使用可以显示多个帧的窗口系统,其默认值显示" "，或者在一次只显示一个帧的普通终端上，显示"-%F " 。
	       "|" 
	       ;; (propertize (shorten-directory default-directory 30) face font-lock-string-face)
       	       ;; (format-mode-line mode-line-buffer-identification)   ;; 模式行缓冲区识别:此变量标识窗口中显示的缓冲区。其默认值显示缓冲区名称，用空格填充至少12列。
	       (custom-buffer-id-icon)
	       (custom-modeline-mode-icon)
	       (custom-process-icon)
	       (custom-projectile-icon)	
	       ))
	 (center (concat
		  (format-mode-line mode-line-modes)  ;; 模式线模式:此变量显示缓冲区的主要和次要模式。其默认值还显示递归编辑级别，有关进程状态的信息以及缩小是否有效。其中含有：mode-name、mode-line-process、minor-mode-alist。
		  (custom-modeline-region-info)
		  ))
	 
	 (rhs (concat
	       (my-mode-line-vc-info)
	       (propertize (format " %s " (custom-modeline-flycheck-status)) 'face `(:background ,background :foreground ,foreground))
	       (format " %s" (custom-modeline-package-updates))
	       ;; (propertize (let ((buf-coding (format "%s" buffer-file-coding-system)))
       	       ;; 		     (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
       	       ;; 			 (format " %s " (match-string 1 buf-coding))
       	       ;; 		       (format " %s " buf-coding))) 'face `(:background ,background :foreground ,foreground) 'display '(raise +0.0))
	       (propertize (format "%s" (replace-buffer-encoding)) 'face `(:background ,background :foreground ,foreground))
	       (custom-modeline-time)
	       ;; (propertize  (custom-modeline-time) 'face `(:background ,background :foreground ,foreground))
	       " "
	       ;; (propertize (format-mode-line " %l:%c ") 'face `(:background ,background :foreground ,foreground))
	       (propertize (format-mode-line "%l:") 'face `(:background ,background :foreground ,foreground))
	       (propertize "%c" 'face
				  (if (>= (current-column) 80)
				      'mode-line-80col-face
				    `(:background ,background :foreground ,foreground)))
	       (propertize (format "|%s" (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p")))) 'face `(:background ,background :foreground ,foreground))
	       "   "
	       ;; mode-line-position     ;; 模式行位置:此变量指示缓冲区中的位置。其默认值显示缓冲区百分比，以及可选的缓冲区大小，行号和列号。显示形如：'xx% (xx,xx)'
	       ;; (propertize (format "  %s" (format-mode-line mode-line-position)) 'face `(:height 1.0 :background ,background :foreground ,foreground)) 
	       (format-mode-line mode-line-end-spaces)              ;; 模式行结束空间:该变量显示在模式行的末尾。
	       )
	      )
	 
         (left (if lpad (concat (make-string lpad ?\s) lhs) lhs))
	 (right (if rpad (concat rhs (make-string rpad ?\s)) rhs))
	 (width (apply '+ (window-width) (let ((m (window-margins))) (list (or (car m) 0) (or (cdr m) 0)))))
	 (total-length (+ (length left) (length center) (length right) 2))
	 )
    ;; (when (> total-length width) (setq left "" right ""))
    (when (> total-length width) (setq center ""))
    (let* ((left-space (/ (- width (length center)) 2))
	   (right-space (- width left-space (length center)))
	   (lspaces (max (- left-space (length left)) 1))
	   (rspaces (max (- right-space (length right)) 1 0)))
      (concat left (make-string lspaces  ?\s)
	      center
	      (make-string rspaces ?\s)
	      right)))
  )


;; ========================================================================================================================
(setq-default mode-line-format '(:eval (customize-mode-line-format-1)))
;; (setq-default mode-line-format '(:eval (customize-mode-line-format-2)))


;; (setq column-number-mode t)           ;; 模式栏显示列号
;; (setq line-number-mode t)             ;; 模式栏显示行号
;; (display-time-mode)          ;; modeline中显示时间模式。
;; (which-function-mode)        ;; modeline中显示选择那个函数模式。如这里开启了，modeline中设不设置为：(which-func-mode ("" which-func-format "--"))，modeline中都会显示的。

;; (setq-default mode-line-format
;;       (list "-"
;;             'mode-line-mule-info
;;             'mode-line-modified
;;             'mode-line-frame-identification
;;             "%b--"
;;             ;; Note that this is evaluated while making the list.
;;             ;; It makes a mode line construct which is just a string.
;;             (getenv "HOST")
;;             ":"
;;             'default-directory
;;             "   "
;;             'global-mode-string
;;             "   %[("
;;             '(:eval (mode-line-mode-name))
;;             'mode-line-process
;;             'minor-mode-alist
;;             "%n"
;;             ")%]--"
;;             '(which-func-mode ("" which-func-format "--"))
;;             '(line-number-mode "L%l--")
;;             '(column-number-mode "C%c--")
;;             '(-3 "%p")))

;; -------------------------------------------------------------------------------------------------------------------------
;; ========================================================================================================================

;; mode-line-format中的书写格式：
;; 1、(list ...) 或者 '(...)，list格式内的()表达式前需加',而后者则不能加'。
;; 2、需加mode-line-format表内元素如是变量，则直接添加或通过(:peoertize xx)添加;如是引用函数，则通过(:eval xx)添加。
;; (setq-default mode-line-format                   ;; Original value of mode-line-format
;; 	      '("%e"                             ;; %e 当Emacs对于Lisp对象几乎没有内存时，会发出一条简短的消息。否则，这是空的。
;; 		mode-line-front-space            ;; 模式行前置空间:该变量显示在模式行的前面。默认情况下，此构造显示在模式行的开头，但如果存在满内存消息，则首先显示该构造。
;; 		mode-line-mule-info              ;; 此变量保存模式行构造的值，该构造显示有关语言环境，缓冲区编码系统和当前输入方法的信息。请参阅非ASCII字符。一般显示为‘U:’,XX快捷键触发后显示‘xx’汉字。
;; 		mode-line-client                 ;; 此变量用于标识emacsclient帧。emacsclient -cn时显示“@”。
;; 		mode-line-modified               ;; 模式行修改:此变量保存模式行构造的值，该构造显示当前缓冲区是否已修改。其默认值显示'**'如果缓冲区被修改，'--'如果没有修改缓冲区，'%%'如果缓冲区是只读的，'％*'如果缓冲区是只读和修改的。
;; 		mode-line-remote                 ;; 此变量用于显示default-directory当前缓冲区是否为远程缓冲区,若不是远程缓冲区，则显示为'-'。
;; 		mode-line-frame-identification   ;; 模式行帧识别:此变量标识当前帧。如果您正在使用可以显示多个帧的窗口系统,其默认值显示" "，或者在一次只显示一个帧的普通终端上，显示"-%F " 。
;; 		mode-line-buffer-identification  ;; 模式行缓冲区识别:此变量标识窗口中显示的缓冲区。其默认值显示缓冲区名称，用空格填充至少12列。
;; 		"   "
;; 		mode-line-position               ;; 模式行位置:此变量指示缓冲区中的位置。其默认值显示缓冲区百分比，以及可选的缓冲区大小，行号和列号。显示形如：'xx% (xx,xx)'
;; 		(vc-mode vc-mode)                ;; vc-mode每个缓冲区中的缓冲区本地变量记录缓冲区的访问文件是否使用版本控制进行维护，如果是，则记录哪种类型。它的值是出现在模式行中的字符串，或者nil没有版本控制。
;; 		"  "
;; 		mode-line-modes                  ;; 模式线模式:此变量显示缓冲区的主要和次要模式。其默认值还显示递归编辑级别，有关进程状态的信息以及缩小是否有效。其中含有：mode-name、mode-line-process、minor-mode-alist。
;; 		"      "
;; 		;; mode-line-misc-info              ;; 用于杂项信息的模式行构造。默认情况下，它显示由指定的信息global-mode-string。与global-mode-string、"%M"效果相同。
;; 		;; (:eval (custom-modeline-time))
;; 	        ;; (:eval (propertize (format-time-string " %H:%M ") 'face `(:height 1.0)))
;; 	        (:eval (format-time-string " %H:%M "))
;; 		"-%-"
;; 		mode-line-end-spaces             ;; 模式行结束空间:该变量显示在模式行的末尾。
;; 		))


;; https://emacs-china.org/t/mode-line-window/6852/5
;; (defun compute-mode-line ()
;;   (let* ((left '("%e" mode-line-buffer-identification mode-line-position))
;;          (center '("%e" mode-line-modes mode-line-misc-info global-mode-string))
;;          ;; (right default-directory)
;;          (right "right")
;;          (width (string-width (format-mode-line (list left center right)))))
;;     (if (> width (window-width))
;;         (list left right)
;;       (list left center right))))
;; (setq-default mode-line-format '(:eval (compute-mode-line)))



(provide 'init-modeline)
;;; init-modeline.el ends here
