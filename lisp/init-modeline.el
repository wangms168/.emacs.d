;;; powerline.el --- Rewrite of Powerline

;; Copyright (C) 2012-2013 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.4
;; Keywords: mode-line
;; Package-Requires: ((cl-lib "0.2"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Powerline is a library for customizing the mode-line that is based on the Vim
;; Powerline. A collection of predefined themes comes with the package.
;;

;;; Code:

(require 'cl-lib)
;; (require 'all-the-icons)

;;;; face

(defgroup powerline nil
  "Powerline, a prettier mode line."
  :group 'mode-line)

(defface powerline-active0 '((t (:inherit mode-line)))
  "Powerline face 0."
  :group 'powerline)

(defface powerline-active1 '((t (:background "grey17" :foreground "white" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-active2 '((t (:background "grey40" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-inactive0
  '((t (:inherit mode-line-inactive)))
  "Powerline face 0."
  :group 'powerline)

(defface powerline-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)

(defface mode-line-buffer-id-inactive
  '((t (:inherit mode-line-buffer-id)))
  "Powerline mode-line face"
  :group 'powerline)

(make-face 'mode-line-80col-face)
(set-face-attribute 'mode-line-80col-face nil
		    :inherit 'mode-line-position-face
		    :foreground "black"
		    :background "#eab700")

;;

;; the frame-local powerline cache causes problems if included in a saved desktop,
;; so delete it before the desktop is saved.
;;
;; see https://github.com/milkypostman/powerline/issues/58
;;
;; It is better to put the following code into your init file for Emacs 24.4 or later.
;; (require 'frameset)
;; (push '(powerline-cache . :never) frameset-filter-alist)

(defun powerline-delete-cache (&optional frame)
  "Set the FRAME cache to nil."
  (set-frame-parameter frame 'powerline-cache nil))

(defun powerline-desktop-save-delete-cache ()
  "Set all caches to nil unless `frameset-filter-alist' has :never for powerline-cache."
  (unless (and (boundp 'frameset-filter-alist)
               (eq (cdr (assq 'powerline-cache frameset-filter-alist))
                   :never))
    (dolist (fr (frame-list)) (powerline-delete-cache fr))))

(add-hook 'desktop-save-hook 'powerline-desktop-save-delete-cache)

;; item 处理

;; 删除了192-331行
;;;###autoload
(defun powerline-mouse (click-group click-type string)
  "Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING."
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

;;;###autoload
(defmacro defpowerline (name body)
  "Create function NAME by wrapping BODY with powerline padding an propetization."
  `(defun ,name
       (&optional face pad)
     (powerline-raw ,body face pad)))

(defun pl/property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun pl/assure-list (item)
  "Assure that ITEM is a list."
  (if (listp item)
      item
    (list item)))

(defun pl/add-text-property (str prop val)
  (mapconcat
   (lambda (mm)
     (let ((cur (pl/assure-list (get-text-property 0 'face mm))))
       (propertize mm 'face (append cur (list val)))))
   (pl/property-substrings str prop)
   ""))

;;;###autoload
(defun powerline-raw (str &optional face pad)    ;; pad的l、r的作用相当于是向左靠齐或向右靠齐。
  "Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r)."
  (when str
    (let* ((rendered-str (format-mode-line str))
           (padded-str (concat
                        (when (and (> (length rendered-str) 0) (eq pad 'l)) " ")
                        (if (listp str) rendered-str str)
                        (when (and (> (length rendered-str) 0) (eq pad 'r)) " "))))

      (if face
          (pl/add-text-property padded-str 'face face)
        padded-str))))

;; 左中右布局

;;;###autoload
(defun powerline-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  ;; (when powerline-text-scale-factor
  ;;   (setq reserve (* powerline-text-scale-factor reserve)))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun powerline-fill-center (face reserve)
  "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  ;; (when powerline-text-scale-factor
  ;;   (setq reserve (* powerline-text-scale-factor reserve)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))

;; item 定义一

;;;###autoload (autoload 'powerline-major-mode "powerline")
(defpowerline powerline-major-mode
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

;;;###autoload (autoload 'powerline-minor-modes "powerline")
(defpowerline powerline-minor-modes
  (mapconcat (lambda (mm)
               (propertize mm
                           'mouse-face 'mode-line-highlight
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map
                                          [mode-line down-mouse-1]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [mode-line mouse-2]
                                          (powerline-mouse 'minor 'help mm))
                                        (define-key map
                                          [mode-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [header-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        map)))
             (split-string (format-mode-line minor-mode-alist))
             (propertize " " 'face face)))

(defcustom powerline-narrowed-indicator "Narrow"
  "A string to display in the mode-line when the buffer is narrowed."
  :group 'powerline
  :type 'string)

;;;###autoload (autoload 'powerline-narrow "powerline")
(defpowerline powerline-narrow
  (when ;; (buffer-narrowed-p) introduced in Emacs 24.3.
      (/= (- (point-max) (point-min)) (buffer-size))
    (propertize powerline-narrowed-indicator
                'mouse-face 'mode-line-highlight
                'help-echo "mouse-1: Remove narrowing from the current buffer"
                'local-map (make-mode-line-mouse-map
                            'mouse-1 'mode-line-widen))))

;; vc版本控制-----------------------------------------------------------------------------------
(defcustom powerline-gui-use-vcs-glyph nil
  "Display a unicode character to represent a version control system. Not always supported in GUI."
  :group 'powerline
  :type 'boolean)

;;;###autoload (autoload 'powerline-vc "powerline")
(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if (and window-system (not powerline-gui-use-vcs-glyph))
        (format-mode-line '(vc-mode vc-mode))
      (format " %s%s"
              (char-to-string #xe0a0)
              (format-mode-line '(vc-mode vc-mode))))))

;; https://github.com/RenChunhui/.emacs.d
;; (defpowerline powerline-vc
;;   (when vc-mode
;;     (powerline-raw
;;      (format-mode-line
;;       (cond
;;        ((string-match "Git[:-]" vc-mode)
;; 	(let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
;; 	  (concat
;; 	   (propertize (format " %s " branch)))))
;;        (t (format "%s" vc-mode)))))))

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

;; https://github.com/RenChunhui/.emacs.d
(defun modeline-git-vc ()
  "自定义 git 状态."
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode)
      (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
	(concat
	 (propertize "\xf418")
	 (propertize (format " %s" branch) 'face `(:height 0.9)))))
     (t (format "%s" vc-mode)))))


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

;; encoding -------------------------------------------------------------------------------
;;;###autoload (autoload 'powerline-encoding "powerline")
(defpowerline powerline-encoding
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(defcustom powerline-buffer-size-suffix t
  "Display the buffer size suffix."
  :group 'powerline
  :type 'boolean)

;;;###autoload (autoload 'powerline-buffer-size "powerline")
(defpowerline powerline-buffer-size
  (propertize
   (if powerline-buffer-size-suffix
       "%I"
     "%i")
   'mouse-face 'mode-line-highlight
   'local-map (make-mode-line-mouse-map
               'mouse-1 (lambda () (interactive)
                          (setq powerline-buffer-size-suffix
                                (not powerline-buffer-size-suffix))
                          (force-mode-line-update)))))

;;;###autoload (autoload 'powerline-buffer-id "powerline")
(defun powerline-buffer-id (&optional face pad)
  (powerline-raw
   (format-mode-line
    (concat (propertize
                 (format-mode-line mode-line-buffer-identification)
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
                 'local-map (let ((map (make-sparse-keymap)))
                              (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
                              (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
                              map))))
   face pad))

;;;###autoload (autoload 'powerline-process "powerline")
(defpowerline powerline-process
  (cond
   ((symbolp mode-line-process) (symbol-value mode-line-process))
   ((listp mode-line-process) (format-mode-line mode-line-process))
   (t mode-line-process)))

;; item 定义二

;; modeline各显示项目自定义
;; https://github.com/domtronn/all-the-icons.el/wiki/Spaceline
;; https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line

;; -------------------------------------------------------------------------------------------------------------------------
;; mode-line-format的mode-line-modes中的mode-name以图标显示
;; propertize中不用 (format "%s" xxx ) 则有错误提示：  Wrong type argument: stringp, minibuffer-inactive-mode
;; (add-hook 'buffer-list-update-hook
;; 	  (lambda () (setq mode-name (all-the-icons-icon-for-buffer))))
;; (add-hook 'buffer-list-update-hook
;; 	  (lambda () (setq mode-name (propertize (format "%s" (all-the-icons-icon-for-buffer)) 'display '(raise 0.0)
;; 						 'face `(:height 1.0 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))

;; -------------------------------------------------------------------------------------------------------------------------
;; mode-line-modified状态图标显示
(defun custom-modified-icon ()
  (let* ((config-alist
          '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.0 :v-adjust -0.0)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.0 :v-adjust -0.0)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.0 :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (propertize (format "%s" (apply (cadr result) (cddr result))) 'face `(:family ,(funcall (car result)) :inherit ))))

;; https://github.com/howardabrams/dot-files/blob/master/emacs-mode-line.org
(defun powerline-get-icon (name alt-sym help-message)
  "Returns a propertized icon if available, otherwise, returns ALT-SYM."
  (propertize alt-sym 'help-echo help-message))

(defun powerline-modified ()
  (condition-case ex
      (let ((state (vc-git-state (buffer-file-name))))
        ;; (cond ((buffer-modified-p)  (powerline-get-icon "pencil" "**✦" "Modified buffer"))
	(cond ((buffer-modified-p)  (when (buffer-modified-p) mode-line-modified))
              ((eq state 'edited)   (powerline-get-icon "pencil" "✦" "Modified buffer, unregistered changes"))
              ((eq state 'unregistered) (powerline-get-icon "question" "" "Unregistered file in VCS"))
              ((eq state 'missing)  (powerline-get-icon "exclamation" "⁈" "File exists only in VCS, not on the hard disk"))
              ((eq state 'ignored)  (powerline-get-icon "ban" "♟" "Ignored file in VCS"))
              ((eq state 'added)    (powerline-get-icon "plus" "" "File will be registered in VCS in the next commit"))
              (t " ")))
    (error (powerline-get-icon "exclamation" "⁈" (car ex)))))

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
;; (use-package  winum
;;   :init
;;   (winum-mode)
;;   :config
;;   (progn
;;     (defun mapleline--unicode-number (str)
;;       "Return a nice unicode representation of a single-digit number STR."
;;       (cond
;;        ((string= "1" str) "➊")
;;        ((string= "2" str) "➋")
;;        ((string= "3" str) "➌")
;;        ((string= "4" str) "➍")
;;        ((string= "5" str) "➎")
;;        ((string= "6" str) "➏")
;;        ((string= "7" str) "➐")
;;        ((string= "8" str) "➑")
;;        ((string= "9" str) "➒")
;;        ((string= "10" str) "➓")
;;        (t str)))
;;     (defun custom-modeline-window-number ()
;;       (propertize (format "%c" (+ 9311 (winum-get-number)))
;; 		  'face `(:height 1.0 :inherit)
;; 		  'display '(raise -0.0))
;;       ;; :tight t :when (fboundp 'window-numbering-mode)
;;       )
;;     ))

(use-package  winum                 ;;依赖dash包
  :config
  (winum-mode))
(defun powerline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (propertize
   (concat
    (cond
     ((string= "1" str) "➀ ")     ;;U+278A   ➊
     ((string= "2" str) "➁ ")     ;;U+278B   ➋
     ((string= "3" str) "➂ ")     ;;U+278C   ➌
     ((string= "4" str) "➃ ")     ;;U+278D   ➍
     ((string= "5" str) "➄ ")     ;;U+278E   ➎
     ((string= "6" str) "➅ ")     ;;U+278F   ➏
     ((string= "7" str) "➆ ")     ;;U+2790   ➐
     ((string= "8" str) "➇ ")     ;;U+2790   ➑
     ((string= "9" str) "➈ ")     ;;U+2792   ➒
     ((string= "10" str) "➉ ")    ;;U+2793   ➓
     (t str)
     ))
   'face `(:height 0.9 :inherit)
   'display '(raise 0.0)
   ))

(defpowerline powerline-window-number
  (when (bound-and-true-p winum-mode)
    (let* ((num (winum-get-number-string)))
      (powerline--unicode-number num))))

;; https://github.com/RenChunhui/.emacs.d
;; (defun powerline--unicode-number (str)
;;   "Return a nice unicode representation of a single-digit number STR."
;;   (powerline-raw
;;    (format-mode-line
;;     (concat
;;      (cond
;;       ((string= "1" str) "➀ ")
;;       ((string= "2" str) "➁ ")
;;       ((string= "3" str) "➂ ")
;;       ((string= "4" str) "➃ ")
;;       ((string= "5" str) "➄ ")
;;       ((string= "6" str) "➅ ")
;;       ((string= "7" str) "➆ ")
;;       ((string= "8" str) "➇ ")
;;       ((string= "9" str) "➈ ")
;;       ((string= "0" str) "➉ "))))))
;; (defpowerline powerline-window-number
;;   (when (bound-and-true-p winum-mode)
;;     (let* ((num (winum-get-number))
;; 	   (str (when num (int-to-string num))))
;;       (powerline--unicode-number str))))

;; (use-package  window-numbering
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
  (when (fboundp 'all-the-icons-icon-for-buffer)
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
	(propertize icon
		    'help-echo (format "Major-mode: `%s`" major-mode)
		    'display '(raise 0.0)
		    'face `(:height 0.9 :family ,(all-the-icons-icon-family-for-buffer) :inherit)))))
  )

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
;; flycheck状态图标显示
(use-package  flycheck        ;;依赖epl和pkg-info两个包
  :init
  (global-flycheck-mode)
  :config)

;; https://github.com/RenChunhui/.emacs.d
(defpowerline powerline-flycheck
  '(:eval
    (let* ((text (pcase flycheck-last-status-change
		           (`finished (if flycheck-current-errors
				                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
						                         (+ (or .warning 0) (or .error 0)))))
				                    (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
				                "✔ No Issues"))
		           (`running     "⟲ Running")
		           (`no-checker  "⚠ No Checker")
		           (`not-checked "✖ Disabled")
		           (`errored     "⚠ Error")
		           (`interrupted "⛔ Interrupted")
		           (`suspicious  ""))))
      (propertize text
		          'help-echo "Show Flycheck Errors"
		          'mouse-face '(:box 1)
		          'local-map (make-mode-line-mouse-map
			                  'mouse-1 (lambda () (interactive) (flycheck-list-errors)))))
    ))


;; -------------------------------------------------------------------------------------------------------------------------
;; 在模式行中显示当前命令及其键
( use-package keycast
  :load-path "site-lisp/keycast-1.0.1"
  :config
  (keycast-mode))

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

(defpowerline powerline-package-updates
  ;; ati-package-updates "An `all-the-icons' spaceline segment to indicate number of package updates needed"
  (let* ((num (or spaceline--upgrades (spaceline--count-upgrades))))
    (propertize
     (concat
      ;; (propertize (format "%s" (all-the-icons-octicon "package"))
      (when (fboundp 'all-the-icons-octicon) (propertize (format "%s"  (all-the-icons-octicon "package"))
							 'face `(:family ,(all-the-icons-octicon-family) :height 1.0 :inherit)
							 'display '(raise 0.0)))
      (propertize (format " %s updates " num) 'face `(:height 1.0 :inherit) 'display '(raise 0.0)))
     'help-echo "Open Packages Menu"
     'mouse-face '(:box 1)
     'local-map (make-mode-line-mouse-map
		 'mouse-1 (lambda () (interactive) (package-list-packages)))))
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
;; (use-package  yahoo-weather
;;   :init
;;   (yahoo-weather-mode)
;;   ;; (setq yahoo-weather-guess-location-function #'yahoo-weather-ipinfo)
;;   :config
;;   (progn
;;     (defun custom-modeline-Weather-icons ()
;;       (let* ((weather (yahoo-weather-info-format 'yahoo-weather-info "%(weather)"))
;;              ;; (temp (spaceline--get-temp)	      )
;;              ;; (help (concat "Weather is '" weather "' and the temperature is " temp))
;;              (icon (all-the-icons-icon-for-weather (downcase weather))))
;; 	(concat
;; 	 ;; (if (> (length icon) 1)
;; 	 ;;     (propertize icon 'help-echo help 'face `(:height 0.9 :inherit) 'display '(raise 0.1))
;; 	 (propertize icon
;;                      ;; 'help-echo help
;;                      'face `(:height 0.9 :family ,(all-the-icons-wicon-family) :inherit)
;;                      'display '(raise 0.0))
;; 	 ;; )
;; 	 ;; (propertize " " 'help-echo help)
;; 	 ;; (propertize (spaceline--get-temp) 'face '(:height 0.9 :inherit) 'help-echo help)
;; 	 ))
;;       ;; :when (and active (boundp 'yahoo-weather-info) yahoo-weather-mode)
;;       ;; :enabled nil
;;       ;; :tight t
;;       )
;;     )
;;   )

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
;; https://github.com/RenChunhui/.emacs.d
(defpowerline powerline-time
  (powerline-raw
   (format-mode-line
    (concat
     (propertize (format-time-string " %H:%M"))))))

(defun custom-modeline-time ()
  (let* ((hour (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" hour) :height 0.9 :v-adjust 0.0)))
    (concat
     (propertize (format-time-string " %H:%M ") 'face `(:height 0.9))
     (propertize (format "%s " icon) 'face `(:height 0.9 :family ,(all-the-icons-wicon-family)) 'display '(raise -0.0)))))

(defun modeline-time ()
  "自定义时间显示."
  (concat
   (propertize " \uf64f" 'face `(:height 1.0))   ;;  nerd-fonts \uf64f  \ue03c ; all-the-icons "\xe192" ;\x1f552  🕒
   (propertize (format-time-string " %H:%M ") 'face `(:height 1.0))
   ))

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
(defun powerline-project-vc ()
  (ignore-errors
    (when (projectile-project-p)
      (propertize (projectile-project-name)
                  'help-echo (format "Base: %s"
                                     (projectile-project-root))))))

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

(defun powerline-evil-state ()
  "Displays *my* version of displaying the evil state."
  (case evil-state
    ('normal " Ⓝ")
    ('insert " Ⓘ")
    ('visual " Ⓥ")
    ('motion " Ⓜ")
    (t       " Ⓔ")))

(defpowerline powerline-evil
  (powerline-evil-state))


;; selected-window

(defvar powerline-selected-window (frame-selected-window)
  "Selected window.")

;;;###autoload (autoload 'powerline-selected-window-active "powerline")
(defun powerline-selected-window-active ()
  "Return whether the current window is active."
  (eq powerline-selected-window (selected-window)))

;; item处理

(defun pl/render (item)
  "Render a powerline ITEM."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (item item)))

(defun powerline-render (values)
  "Render a list of powerline VALUES."
  (mapconcat 'pl/render values ""))

(defun powerline-width (values)
  "Get the length of VALUES."
  (if values
      (let ((val (car values)))
        (+ (cond
            ((stringp val) (string-width (format-mode-line val)))
            ((and (listp val) (eq 'image (car val)))
             (car (image-size val)))
            (t 0))
           (powerline-width (cdr values))))
    0))

;; 自定义状态栏

;; (setq column-number-mode t)           ;; 模式栏显示列号
;; (setq line-number-mode t)             ;; 模式栏显示行号
;; (display-time-mode)          ;; modeline中显示时间模式。
;; (which-function-mode)        ;; modeline中显示选择那个函数模式。如这里开启了，modeline中设不设置为：(which-func-mode ("" which-func-format "--"))，modeline中都会显示的。

(defun Short-directory ()
  (if (buffer-file-name)
      (concat (directory-file-name
	       (file-name-directory
		(abbreviate-file-name
		 (buffer-file-name))))"/"))
  )

;;;###autoload
(defun powerline-center-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format           ;; header-line-format 是tabbar的格式
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (lhs (list
				                (powerline-window-number face0 'l)
		  		                ;; (powerline-raw (mapleline--unicode-number (winum-get-number-string)) face0 'l)
		  		                ;; (powerline-raw (custom-modeline-window-number) face0 'l)
				                (when (and (boundp 'evil-mode) evil-mode)
				                  (powerline-evil face0 'l))
				                ;; (powerline-raw (powerline-evil-state) face0 'r)
				                (when (featurep 'all-the-icons)
				                  (powerline-raw (propertize (all-the-icons-octicon "package") 'face `(:family ,(all-the-icons-octicon-family) :height 1.0) 'display '(raise -0.0)) face0 'l))
				                ;; (powerline-raw (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.0) 'display '(raise -0.0)) face0 'l)
				                ;; (powerline-raw " " face0 'l)
				                (powerline-buffer-size face0 'l)
				                (powerline-raw mode-line-mule-info face0 'l)     ;; 此变量保存模式行构造的值，该构造显示有关语言环境，缓冲区编码系统和当前输入方法的信息。请参阅非ASCII字符。
				                ;; 一般显示为‘U:’，C-\(toggle-input-method)显示‘拼符U:’。
				                ;; (powerline-raw "[%z]" face0)
				                (powerline-raw mode-line-client face0 'l)        ;; 此变量用于标识emacsclient帧。emacsclient -cn时显示“@”。
				                (powerline-raw mode-line-modified face0 'l)
                                ;;				(powerline-raw (powerline-modified) face0 'l)    ;; powerline-modified很卡。模式行修改:此变量保存模式行构造的值，该构造显示当前缓冲区是否已修改。
				                ;; (when (buffer-modified-p)
				                ;;   (powerline-raw mode-line-modified face0 'l))
				                ;; (when buffer-read-only
				                ;;   (powerline-raw "[RO]" face0 'l))

				                ;; (powerline-raw "|" face0 'l)
				                ;; (powerline-raw mode-line-remote face0 'l)     ;; 此变量用于显示default-directory当前缓冲区是否为远程缓冲区,若不是远程缓冲区，则显示为'-'。
				                ;; (powerline-raw mode-line-frame-identification face0 'l)  ;; 模式行帧识别:此变量标识当前帧。如果您正在使用可以显示多个帧的窗口系统,其默认值显示" "，
				                ;;                                                          ;; 或者在一次只显示一个帧的普通终端上，显示"-%F " 。
				                (powerline-raw "|" face0 'l)
				                (powerline-raw (Short-directory) face0 'l)
				                (powerline-buffer-id `(mode-line-buffer-id ,face0) 'r)
				                ;; (powerline-raw mode-line-buffer-identification face0 'r) ;; 模式行缓冲区识别:此变量标识窗口中显示的缓冲区。其默认值显示缓冲区名称，用空格填充至少12列。
				                ;; (powerline-raw "%b" face0 'l)
				                (powerline-raw (custom-modeline-mode-icon) face0 'r)        ;; (when (featurep 'all-the-icons) ... )
				                (powerline-process face0)
				                ;; (powerline-raw (custom-process-icon) face0)
				                ;; (powerline-raw (powerline-project-vc) face0 'r)
				                ;; (powerline-raw (custom-projectile-icon) face0)              ;;含有projectile，会使modeline高度变化,显示|x|
		  		                (powerline-narrow face1 'l)
				                ;; (powerline-raw (modeline-git-vc) face0 'l)
		  		                (powerline-vc face1 'l)
                                (when (bound-and-true-p mode-line-keycast)
				                  (powerline-raw mode-line-keycast face1 'r))
		  		                ))
                          (rhs (list
				                (powerline-flycheck face1 'l)
				                ;; (powerline-raw (modeline-flycheck-status) face1 'l)
				                (powerline-package-updates face2 'l)
				                ;; (powerline-raw (custom-modeline-package-updates) face2 'l)
				                ;; (powerline-encoding face1 'r)
				                (powerline-raw (replace-buffer-encoding) face1 'r)
				                ;; (powerline-time face0 'r)
				                (powerline-raw (modeline-time) face0 )
				                ;; (powerline-raw global-mode-string face1 'r)

				                ;; (powerline-raw "%4l:%3c" face1 'l)
				                (powerline-raw "%4l:"  face1 'l)                       ;;(format-mode-line '(4 "%l"))
				                (powerline-raw "%3c" (if (>= (current-column) 80)    ;;(format-mode-line '(3 "%c"))
							                             'mode-line-80col-face face1))
				                (powerline-raw " " face0)
				                (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) face0 'r)
				                (powerline-raw "%3 " face0)
				                (powerline-fill face0 0)
				                ))
                          (center (list
				                   (powerline-raw "" face2)
				                   (when (and (boundp 'which-func-mode) which-func-mode)
				                     (powerline-raw which-func-format face2 'l))
				                   (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
				                     (powerline-raw erc-modified-channels-object face2 'l))
				                   (powerline-raw "[" face2 'l)
				                   (powerline-major-mode face2)
				                   (powerline-process face2)
				                   (powerline-raw "]" face2)
				                   (powerline-raw "[" face2 'l)
				                   ;;(powerline-minor-modes face2)
                                   (powerline-raw "C-h m" face2)
				                   (powerline-raw "%n" face2)
				                   (powerline-raw "]" face2)
				                   ;; (powerline-raw mode-line-modes face2)      ;; 模式线模式:此变量显示缓冲区的主要和次要模式。其默认值还显示递归编辑级别，有关进程状态的信息以及缩小是否有效。
				                   ;; 其中含有：mode-name、mode-line-process、minor-mode-alist。
				                   (powerline-raw (custom-modeline-region-info) face2)
				                   ))
		  	              )
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)
		     	             )
		             )))))

(powerline-center-theme)
;; (setq-local mode-line-format nil)
;; (setq-default mode-line-format nil)


(defun powerline-center-theme-ori ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          ;; (separator-left (intern (format "powerline-%s-%s"
                          ;;                                 (powerline-current-separator)
                          ;;                                 (car powerline-default-separator-dir))))
                          ;; (separator-right (intern (format "powerline-%s-%s"
                          ;;                                  (powerline-current-separator)
                          ;;                                  (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     ;; (when powerline-display-buffer-size
                                     ;;   (powerline-buffer-size face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (powerline-raw " " face0)
                                     ;; (funcall separator-left face0 face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-vc face1)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw "%4l" face1 'r)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%3c" face1 'r)
                                     ;; (funcall separator-right face1 face0)
                                     (powerline-raw " " face0)
                                     (powerline-raw "%6p" face0 'r)
                                     ;; (when powerline-display-hud
                                     ;;   (powerline-hud face2 face1))
                                     (powerline-fill face0 0)))
                          (center (list (powerline-raw " " face1)
                                        ;; (funcall separator-left face1 face2)
                                        ;; (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                        ;;   (powerline-raw erc-modified-channels-object face2 'l))
                                        (powerline-major-mode face2 'l)
                                        (powerline-process face2)
                                        (powerline-raw " :" face2)
                                        (powerline-minor-modes face2 'l)
                                        (powerline-raw " " face2)
                                        ;; (funcall separator-right face2 face1)
					                    )))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

;;(powerline-center-theme-ori)


;; 左右中布局：https://emacs.stackexchange.com/questions/16654/how-to-re-arrange-things-in-mode-line
;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
;; https://emacs-china.org/t/mode-line-window/6852/5
;; https://emacs-china.org/t/mode-line/6979
;; 左右布局：https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline

;; mode-line-format中的书写格式：
;; 1、(list ...) 或者 '(...)，list格式内的()表达式前需加',而后者则不能加'。
;; 2、需加mode-line-format表内元素如是变量，则直接添加或通过(:peoertize xx)添加;如是引用函数，则通过(:eval xx)添加。
;; (setq-default mode-line-format                ;; Original value of mode-line-format
;; 	      '("%e"                             ;; %e 当Emacs对于Lisp对象几乎没有内存时，会发出一条简短的消息。否则，这是空的。
;; 		mode-line-front-space            ;; 模式行前置空间:该变量显示在模式行的前面。默认情况下，此构造显示在模式行的开头，但如果存在满内存消息，则首先显示该构造。
;; 		mode-line-mule-info              ;; 此变量保存模式行构造的值，该构造显示有关语言环境，缓冲区编码系统和当前输入方法的信息。
;;                                               ;;请参阅非ASCII字符。utf-8显示为‘U:’、prefer-utf-8显示为"-:"，C-\(toggle-input-method)显示‘拼符U:’。
;; 		mode-line-client                 ;; 此变量用于标识emacsclient帧。emacsclient -cn时显示“@”。
;; 		mode-line-modified               ;; 模式行修改:此变量保存模式行构造的值，该构造显示当前缓冲区是否已修改。
;;                                               ;;其默认值显示'**'如果缓冲区被修改，'--'如果没有修改缓冲区，'%%'如果缓冲区是只读的，'％*'如果缓冲区是只读和修改的。
;; 		mode-line-remote                 ;; 此变量用于显示default-directory当前缓冲区是否为远程缓冲区,若不是远程缓冲区，则显示为'-'。
;; 		mode-line-frame-identification   ;; 模式行帧识别:此变量标识当前帧。如果您正在使用可以显示多个帧的窗口系统,其默认值显示" "，或者在一次只显示一个帧的普通终端上，显示"-%F " 。
;; 		mode-line-buffer-identification  ;; 模式行缓冲区识别:此变量标识窗口中显示的缓冲区。其默认值显示缓冲区名称，用空格填充至少12列。
;; 		"   "
;; 		mode-line-position               ;; 模式行位置:此变量指示缓冲区中的位置。其默认值显示缓冲区百分比，以及可选的缓冲区大小，行号和列号。显示形如：'xx% (xx,xx)'
;; 		(vc-mode vc-mode)                ;; vc-mode每个缓冲区中的缓冲区本地变量记录缓冲区的访问文件是否使用版本控制进行维护，如果是，则记录哪种类型。
;;                                               ;;它的值是出现在模式行中的字符串，或者nil没有版本控制。
;; 		"  "
;; 		mode-line-modes                  ;; 模式线模式:此变量显示缓冲区的主要和次要模式。其默认值还显示递归编辑级别，有关进程状态的信息以及缩小是否有效。
;;其中含有：mode-name、mode-line-process、minor-mode-alist。
;; 		"      "
;; 		;; mode-line-misc-info           ;; 用于杂项信息的模式行构造。默认情况下，它显示由指定的信息global-mode-string。与global-mode-string、"%M"效果相同。
;; 		;; (:eval (custom-modeline-time))
;; 	        ;; (:eval (propertize (format-time-string " %H:%M ") 'face `(:height 1.0)))
;; 	        (:eval (format-time-string " %H:%M "))
;; 		"-%-"
;; 		mode-line-end-spaces             ;; 模式行结束空间:该变量显示在模式行的末尾。
;; 		))



(provide 'init-modeline)

;;; powerline.el ends here
