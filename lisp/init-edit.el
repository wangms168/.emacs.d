;;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t  -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
;; (prefer-coding-system 'utf-8)

;; (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
;; (setq savehist-file "~/.emacs.d/default/tmp/savehist")
;; (tooltip-mode -1)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)
;; (prefer-coding-system 'utf-8)
;; (setq visible-bell t)                     ;; 将Emacs配置为闪存而不是响铃
(setq-default indicate-empty-lines t)        ;; 在缓冲区结束后可视地指示空行
;; (setq-default show-trailing-whitespace t)    ;; 突出显示行尾空格,但这样C-x b的minibuffer行尾空格显示出来很难看。
(defalias 'yes-or-no-p 'y-or-n-p)            ;; 用'y'和'n'来代替频繁地输入'yes’和'no'
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (setq-default cursor-type 'bar)           ;; 设置光标为小长条形状.设置这个，智能改变光标形状不起作用。

(setq split-width-threshold nil)             ;; 窗口垂直分割
(setq split-height-threshold 0)

;; 在消息缓冲区中打印sexp时删除恼人的省略号
;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)                  ;; 取消光标闪烁
 '(font-use-system-font t)
 '(show-paren-mode t)                      ;;光标位于括号之后显示匹配的括号
 )

;; Miscs
(setq inhibit-startup-message t)          ;; 禁用启动画面
(setq-default indicate-empty-lines t)     ;; show (in left margin) marker for empty lines
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
;; (setq make-backup-files nil)               ; Forbide to make backup files
;; (setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq-default kill-whole-line t)           ; Kill line including '\n'
(setq inhibit-compacting-font-caches t)    ;; Don’t compact font caches during GC.据说可以解决windows下所有字体的卡顿问题。

(add-to-list 'auto-mode-alist '("\\.nanorc\\'" . sh-mode))   ;;conf-unix-mode  ;;.nanorc的语法高亮

(global-hl-line-mode)                     ;;高亮当前行
(with-eval-after-load 'hl-line (set-face-background hl-line-face "#212121" ) )
;;----------------------------------------------------------------------------
;; paren
;;----------------------------------------------------------------------------
(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c" :underline "blue"))))
  :custom
  (show-paren-style 'parentheses)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))


;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	((looking-at "\\s)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; M-x check-parens <RET>  https://codeday.me/bug/20180225/137251.html

;; https://www.emacswiki.org/emacs/DebuggingParentheses
;; could be bad, will not let you save at all, until you correct the error
;; 可能是坏的，根本不会让你保存，直到你纠正错误
;; (add-hook 'emacs-lisp-mode-hook                 ;; 如有括号不匹配，则给出提示并不让保存。
;; 	  (lambda ()
;; 	    (add-hook 'local-write-file-hooks      ;;  local-write-file-hooks在22.1版过时了，用write-file-functions代替。   ;; 用before-save-hook，也有出错提示，但仍然保存。
;; 		      'check-parens)))

(add-hook 'write-file-functions 'check-parens)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 光标在括号内时就高亮包含内容的两个括号
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

;;突出显示匹配的双引号
(defun show-paren--match-quotes ()
  (let ((ppss (syntax-ppss)))
    ;; In order to distinguish which quote is opening and which is starting,
    ;; check that that point is not within a string (or comment, for that
    ;; matter).  Also ignore escaped quotes.
    (unless (or (nth 8 ppss) (nth 5 ppss))
      (or
       (and (not (bobp))
	    (eq 7 (car-safe (syntax-after (1- (point)))))
	    (save-excursion
	      (let ((end (point))
		    (ppss (syntax-ppss (1- (point)))))
		(when (nth 3 ppss)
		  (let ((beg (nth 8 ppss)))
		    (list beg
			  (1+ beg)
			  (1- end)
			  end))))))
       (and (not (eobp))
	    (eq 7 (car-safe (syntax-after (point))))
	    (save-excursion
	      (let ((beg (point)))
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      (list beg
			    (1+ beg)
			    (1- (point))
			    (point)))))))))))

(advice-add 'show-paren--default :after-until #'show-paren--match-quotes)

;; 删除匹配的括号
;; https://zhuanlan.zhihu.com/p/24309937
(defun c-delete-pair ()
  (interactive)
  (let ((re "[([{<'\"]"))
    (when (or (looking-at-p re) (re-search-backward re nil t))
      (save-excursion (forward-sexp) (delete-char -1))
      (delete-char 1))))
;; ----------------------------------------------------------------------------------------


;; (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;;显示当前函数的参数列表,似乎已默认开启。

;; (setq-default major-mode 'text-mode)

;; (setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; (setq sentence-end-double-space nil)

;; ;; Tab and Space
;; ;; Permanently indent with spaces, never with TABs
;; (setq-default c-basic-offset   4
;;               tab-width        4
;;               indent-tabs-mode nil)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))   ;; 粘贴或键入的文本即可替换活动区域，该替换动作隐含一个Backspace键（‘DEL’）删除所选文本的动作。

;;----------------------------------------------------------------------------
;; Copy/paste
;;----------------------------------------------------------------------------
;; (setq select-active-regions t)
;; (setq x-select-enable-clipboard-manager nil
;;       select-enable-clipboard           t
;;       select-enable-primary             nil)
;; (delete-selection-mode 1)              ;; 粘贴或键入的文本即可替换活动区域，该替换动作隐含一个Backspace键（‘DEL’）删除所选文本的动作。

(dolist (command '(yank yank-pop))      ;;粘贴时自打格式化
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; ;; Rectangle
;; (use-package rect
;;   :ensure nil
;;   :bind (("<C-return>" . rectangle-mark-mode)))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; ;; Pass a URL to a WWW browser
;; (use-package browse-url
;;   :ensure nil
;;   :defines dired-mode-map
;;   :bind (("C-c C-z ." . browse-url-at-point)
;;          ("C-c C-z b" . browse-url-of-buffer)
;;          ("C-c C-z r" . browse-url-of-region)
;;          ("C-c C-z u" . browse-url)
;;          ("C-c C-z v" . browse-url-of-file))
;;   :init
;;   (with-eval-after-load 'dired
;;     (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; ;; Jump to things in Emacs tree-style
;; (use-package avy
;;   :bind (("C-:" . avy-goto-char)
;;          ("C-'" . avy-goto-char-2)
;;          ("M-g f" . avy-goto-line)
;;          ("M-g w" . avy-goto-word-1)
;;          ("M-g e" . avy-goto-word-0))
;;   :hook (after-init . avy-setup-default)
;;   :config (setq avy-background t))

;; ;; Kill text between the point and the character CHAR
;; (use-package avy-zap
;;   :bind (("M-z" . avy-zap-to-char-dwim)
;;          ("M-Z" . avy-zap-up-to-char-dwim)))

;; ;; Quickly follow links
;; (use-package ace-link
;;   :bind (("M-o" . ace-link-addr))
;;   :hook (after-init . ace-link-setup-default))

;; ;; Jump to Chinese characters
;; (use-package ace-pinyin
;;   :diminish
;;   :hook (after-init . ace-pinyin-global-mode))

;; ;; Minor mode to aggressively keep your code always indented
;; (use-package aggressive-indent
;;   :diminish
;;   :hook ((after-init . global-aggressive-indent-mode)
;;          ;; FIXME: Disable in big files due to the performance issues
;;          ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
;;          (find-file . (lambda ()
;;                         (if (> (buffer-size) (* 3000 80))
;;                             (aggressive-indent-mode -1)))))
;;   :config
;;   ;; Disable in some modes
;;   (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode go-mode))
;;     (push mode aggressive-indent-excluded-modes))

;;   ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
;;   (add-to-list
;;    'aggressive-indent-dont-indent-if
;;    '(and (or (derived-mode-p 'c-mode)
;;              (derived-mode-p 'c++-mode)
;;              (derived-mode-p 'csharp-mode)
;;              (derived-mode-p 'java-mode)
;;              (derived-mode-p 'go-mode)
;;              (derived-mode-p 'swift-mode))
;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                              (thing-at-point 'line))))))

;; ;; Show number of matches in mode-line while searching
;; (use-package anzu
;;   :diminish
;;   :bind (([remap query-replace] . anzu-query-replace)
;;          ([remap query-replace-regexp] . anzu-query-replace-regexp)
;;          :map isearch-mode-map
;;          ([remap isearch-query-replace] . anzu-isearch-query-replace)
;;          ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
;;   :hook (after-init . global-anzu-mode))

;; ;; An all-in-one comment command to rule them all
;; (use-package comment-dwim-2
;;   :bind ([remap comment-dwim] . comment-dwim-2)) ;

;; ;; Drag stuff (lines, words, region, etc...) around
;; (use-package drag-stuff
;;   :diminish
;;   :commands drag-stuff-define-keys
;;   :hook (after-init . drag-stuff-global-mode)
;;   :config
;;   (add-to-list 'drag-stuff-except-modes 'org-mode)
;;   (drag-stuff-define-keys))

;; ;; A comprehensive visual interface to diff & patch
;; (use-package ediff
;;   :ensure nil
;;   :hook(;; show org ediffs unfolded
;;         (ediff-prepare-buffer . outline-show-all)
;;         ;; restore window layout when done
;;         (ediff-quit . winner-undo))
;;   :config
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq ediff-split-window-function 'split-window-horizontally)
;;   (setq ediff-merge-split-window-function 'split-window-horizontally))

;; ;; Edit multiple regions in the same way simultaneously
;; (use-package iedit
;;   :defines desktop-minor-mode-table
;;   :bind (("C-;" . iedit-mode)
;;          ("C-x r RET" . iedit-rectangle-mode)
;;          :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
;;          :map esc-map ("C-;" . iedit-execute-last-modification)
;;          :map help-map ("C-;" . iedit-mode-toggle-on-function))
;;   :config
;;   ;; Avoid restoring `iedit-mode'
;;   (with-eval-after-load 'desktop
;;     (add-to-list 'desktop-minor-mode-table
;;                  '(iedit-mode nil))))

;; ;; Increase selected region by semantic units
;; (use-package expand-region
;;   :bind ("C-=" . er/expand-region))

;; ;; Multiple cursors
;; (use-package multiple-cursors
;;   :bind (("C-S-c C-S-c"   . mc/edit-lines)
;;          ("C->"           . mc/mark-next-like-this)
;;          ("C-<"           . mc/mark-previous-like-this)
;;          ("C-c C-<"       . mc/mark-all-like-this)
;;          ("C-M->"         . mc/skip-to-next-like-this)
;;          ("C-M-<"         . mc/skip-to-previous-like-this)
;;          ("s-<mouse-1>"   . mc/add-cursor-on-click)
;;          ("C-S-<mouse-1>" . mc/add-cursor-on-click)
;;          :map mc/keymap
;;          ("C-|" . mc/vertical-align-with-space)))

;; ;; Smartly select region, rectangle, multi cursors
;; (use-package smart-region
;;   :hook (after-init . smart-region-on))

;; ;; On-the-fly spell checker
;; (use-package flyspell
;;   :ensure nil
;;   :diminish
;;   :if (executable-find "aspell")
;;   :hook (((text-mode outline-mode) . flyspell-mode)
;;          (prog-mode . flyspell-prog-mode)
;;          (flyspell-mode . (lambda ()
;;                             (dolist (key '("C-;" "C-," "C-."))
;;                               (unbind-key key flyspell-mode-map)))))
;;   :init
;;   (setq flyspell-issue-message-flag nil)
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; ;; Hungry deletion
;; (use-package hungry-delete
;;   :diminish
;;   :hook (after-init . global-hungry-delete-mode)
;;   :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; ;; Make bindings that stick around
;; (use-package hydra)

;; ;; Framework for mode-specific buffer indexes
;; (use-package imenu
;;   :ensure nil
;;   :bind (("C-." . imenu)))

;; ;; Move to the beginning/end of line or code
;; (use-package mwim
;;   :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
;;          ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; ;; Windows-scroll commands
;; (use-package pager
;;   :bind (([remap scroll-up-command] . pager-page-down)
;;          ([next]   . pager-page-down)
;;          ([remap scroll-down-command] . pager-page-up)
;;          ([prior]  . pager-page-up)
;;          ([M-up]   . pager-row-up)
;;          ([M-kp-8] . pager-row-up)
;;          ([M-down] . pager-row-down)
;;          ([M-kp-2] . pager-row-down)))

;; ;; Treat undo history as a tree
;; (use-package undo-tree
;;   :diminish
;;   :hook (after-init . global-undo-tree-mode))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; ;; Handling capitalized subwords in a nomenclature
;; (use-package subword
;;   :ensure nil
;;   :diminish
;;   :hook ((prog-mode . subword-mode)
;;          (minibuffer-setup . subword-mode)))

;; ;; Hideshow
;; (use-package hideshow
;;   :ensure nil
;;   :diminish hs-minor-mode
;;   :bind (:map hs-minor-mode-map
;;               ("C-`" . hs-toggle-hiding)))

;; ;; Narrow/Widen
;; (use-package fancy-narrow
;;   :diminish
;;   :hook (after-init . fancy-narrow-mode))

;;----------------------------------------------------------------------------
;; comment
;;----------------------------------------------------------------------------
;; (global-set-key [?\M-;] 'comment-or-uncomment-region)		 ;; 批量注释
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)		 ;; 批量注释
(defun my-comment-or-uncomment-region (beg end &optional arg)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end) nil)
		 (list (line-beginning-position)
		       (line-beginning-position 2))))
  (comment-or-uncomment-region beg end arg)
  )
(global-set-key [remap comment-or-uncomment-region] 'my-comment-or-uncomment-region)

;; (use-package newcomment
;;   :ensure nil
;;   :config
;;   (progn
;;     ;; Change the behavior of `M-;' by commenting line.
;;     ;; Much simpler than emacs-25 `comment-line'.
;;     (defun comment--advice-dwim (old--fn &rest args)
;;       (if (region-active-p)
;;           (apply old--fn args)
;;         (save-excursion
;;           (goto-char (point-at-bol))
;;           (push-mark (point-at-eol) t t)
;;           (apply old--fn args))
;;         (indent-region (point-at-bol) (point-at-eol))
;;         (forward-line 1)
;;         (back-to-indentation)))
;;     (advice-add 'comment-dwim :around 'comment--advice-dwim)))

;;----------------------------------------------------------------------------
;; line-numbers
;;----------------------------------------------------------------------------
;; (global-linum-mode t)
;; (add-hook 'find-file-hook 'linum-mode)                         ;; 当查看超过数万行的文件时,emacs会卡顿。查卡顿方法：M-x profiler-start 然后 M-x profiler-report
;; (setq linum-format "%4d|")               ;;set format
;; (setq linum-format "%4d \u2502 ")        ;; "\u2502"="|"
;; (global-display-line-numbers-mode t)
(add-hook 'find-file-hook 'display-line-numbers-mode)             ;; Emacs 26 新增了原生的行号支持。这与“linum-mode”提供的类似，但更快，并且不会占用行号的显示余量。

;;----------------------------------------------------------------------------
;; title
;;----------------------------------------------------------------------------
;; (setq frame-title-format " %f")       ;; 标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号
;; (setq column-number-mode t)           ;; 模式栏显示列号
;; (setq line-number-mode t)             ;; 模式栏显示行号

;; (setq undo-tree-auto-save-history t)     ;;可以撤销关闭重启前的修改
;; (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))
;;测试撤销重启前的修改,经测试，上述的设置不管用。

(use-package beacon     ;; 每当窗口滚动时，光线就会照亮光标顶部;你知道它在哪里。
  :config
  (beacon-mode 1))

(use-package pos-tip
  :config
  )

;; 要在ProgrammingModes中自动填充注释而不是代码.
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (set (make-local-variable 'fill-nobreak-predicate)
		 (lambda ()
		   (not (eq (get-text-property (point) 'face)
			    'font-lock-comment-face))))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; 高亮非ASCII字符
;; 高亮非ASCII字符   M-x highlight-regexp (C-x w h) RET [^[:ascii:]] RET
;; 取消高亮          M-x unhighlight-regexp (C-x w h)RET [^[:ascii:]] RET
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  ;; (occur "[^[:ascii:]]"))
  (highlight-regexp "[^[:ascii:]]"))

;; https://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
	(exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
	(exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
	(goto-char end)
	(newline)
	(insert region)
	(setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c <down>") 'duplicate-current-line-or-region)

;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
    With argument N, make N copies.
    With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
		      (buffer-substring (region-beginning) (region-end))
		    (prog1 (thing-at-point 'line)
		      (end-of-line)
		      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
			  (newline))))))
	(dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
	  (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
	(if (> 0 n)                             ;Comment out original with negative arg
	    (comment-region (line-beginning-position) (line-end-position)))
	(forward-line 1)
	(forward-char pos)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(defun get-mode-name ()
  "显示`major-mode'及`mode-name'"
  (interactive)
  (message "major-mode为%s, mode-name为%s" major-mode mode-name))
(global-set-key (kbd "C-x m") 'get-mode-name)


;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-up' (or `M-down')
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; ;; XXX `M-up' and `M-down' are bound multiple times (to different things)!
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)


(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
