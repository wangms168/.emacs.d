;;; init-theme.el --- modeline configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 第六步 ==========================================================================;;
;; 安装color-theme包，第七步的主题配置方式需要这个包。
;; ;(use-package color-theme
;;   :config
;;   (eval-after-load "color-theme"
;;     '(progn
;;        ;; (color-theme-initialize)
;;        ;; (color-theme-hober)
;;        ;; (color-theme-clarity-and-beauty)
;;        ;; (color-theme-charcoal-black)    ;hober\Calm Forest \Charcoal Black
;;        ;; (color-theme-calm-forest)
;;        )))
;; ;; 用M-x color-theme-select来选择你喜欢的颜色主题了

;; 第8步 ==========================================================================;;
;; 主题设置：内置/自定义主题是custom-theme-load-path后load-theme；彩色主题是load-path后require后再主题函数；theme.el是指定路径后load主题文件再主题函数。
;; 1 内置主题及自定义主题 load customize theme
;; (let ((basedir "~/.emacs.d/my-config/custom-themes/"))
;;   (dolist (f (directory-files basedir))
;;     (if (and (not (or (equal f ".") (equal f "..")))
;;              (file-directory-p (concat basedir f)))
;;         (add-to-list 'custom-theme-load-path (concat basedir f)))))

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/my-config/custom-themes/")
;; ;; (load-theme 'monokai t)
;; ;; (load-theme 'organic-green t)

;; 2 彩色主题
;; (add-to-list 'load-path "~/.emacs.d/my-config/color-themes/")
;; (require 'color-theme-ahei)       ;; **************************就是这个未注释掉，将color-theme-ahei主题的高亮当前行设置，带入到monokai-theme主题，产生特差的效果,害我花了7、8个小时找原因。
;; (color-theme-ahei)              ;;按 d 看见的代码

;; monokai-theme主题并稍加配色
(use-package monokai-theme
  :init
  ;; (require 'monokai-theme)     ;;为了显示加载时间，显式require请求下。
    ;;setq ;; foreground and background
    ;;monokai-foreground     "#ABB2BF"
    ;;monokai-background     "#282C34"               ;; "#21242B"             ;;"#002B36"\#282C34\#0C1021\#002B36\0B0F20\#002A38\#414142\#464646\#262D2F\#1B1D1D
    ;;;; highlights and comments
    ;;monokai-comments       "#F8F8F0"
    ;;monokai-emphasis       "#282C34"
    ;;monokai-highlight      "#FFB269"
    ;;monokai-highlight-alt  "#66D9EF"
    ;;monokai-highlight-line "#1B1D1E"
    ;;monokai-line-number    "#F8F8F0"
    ;;;; colours
    ;;monokai-blue           "#61AFEF"
    ;;monokai-cyan           "#56B6C2"
    ;;monokai-green          "#98C379"
    ;;monokai-gray           "#3E4451"
    ;;monokai-violet         "#C678DD"
    ;;monokai-red            "#E06C75"
    ;;monokai-orange         "#D19A66"
    ;;monokai-yellow         "#E5C07B")
  :config
  (load-theme 'monokai t)
  ;; (add-hook 'after-init-hook '(lambda () (load-theme 'monokai t)))     ;; 将使header-line的box显现
  )

;; 用于helm迷你栏的face
(defface helm-source-header
  '((((background dark))
     :background "#22083397778B"
     :foreground "white"
     :weight bold :height 1.3 :family "Sans Serif")
    (((background light))
     :background "#abd7f0"
     :foreground "black"
     :weight bold :height 1.3 :family "Sans Serif"))
  "Face for source header in the helm buffer."
  :group 'helm-faces)



;; (use-package doom-themes
;;   :init
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled

;;   ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;;   ;; may have their own settings.
;;   (add-hook 'after-init-hook
;;          (lambda () (load-theme 'doom-city-lights t)))
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme
;;   (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)

;;   )


;; (use-package afternoon-theme
;;   :init
;;   (add-hook 'after-init-hook
;;	    (lambda () (load-theme 'afternoon t)))
;;   )

;; (use-package color-theme-sanityinc-tomorrow
;;   :init
;;   (add-hook 'after-init-hook
;;	    ;; (lambda () (load-theme 'sanityinc-tomorrow-bright t)))
;;	    (lambda () (load-theme 'sanityinc-tomorrow-blue t)))
;;   )

;; ;; cyberpunk-theme主题
;; (use-package cyberpunk-theme
;;   :init
;;   (add-hook 'after-init-hook
;;             (lambda () (load-theme 'cyberpunk t)))
;;   )


;; 3 添加 03-themes.el文件，和添加"~\Emacs\myconfig\color-themes\gui"文件夹下的my主题文件。

(defvar dark-background nil)

(defun toggle-dark-background ()
  (interactive)
  (let ((difficult-colors
         '("red" "blue" "medium blue")))
    (mapc
     (lambda (face)
       (and (member (face-attribute face :foreground)  difficult-colors)
            (set-face-bold-p face (not dark-background))))
     (face-list)))
  (setq dark-background (not dark-background)))


(provide 'init-theme)

;;; init-theme.el ends here
