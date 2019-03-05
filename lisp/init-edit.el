;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t  -*-

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
(setq visible-bell t)                        ;;将Emacs配置为闪存而不是响铃
(setq-default indicate-empty-lines t)        ;; 在缓冲区结束后可视地指示空行
(setq-default show-trailing-whitespace t)    ;; 突出显示尾随空格
(defalias 'yes-or-no-p 'y-or-n-p)            ;; 用'y'和'n'来代替频繁地输入'yes’和'no'
;; (fset 'yes-or-no-p 'y-or-n-p)
;; (setq-default cursor-type 'bar)           ;; 设置光标为小长条形状.设置这个，智能改变光标形状不起作用。

;; Miscs
(setq inhibit-startup-message t)          ;; 禁用启动画面
(blink-cursor-mode -1)                    ;; 取消光标闪烁
(setq-default indicate-empty-lines t)     ;; show (in left margin) marker for empty lines
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
;; (setq make-backup-files nil)               ; Forbide to make backup files
;; (setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq-default kill-whole-line t)           ; Kill line including '\n'

(global-hl-line-mode)                     ;;高亮当前行
(with-eval-after-load 'hl-line (set-face-background hl-line-face "#212121" ) )
;;----------------------------------------------------------------------------
;; paren
;;----------------------------------------------------------------------------
(use-package paren
  :ensure nil
  :config
  (progn
    (show-paren-mode)                     ;;光标位于括号之后显示匹配的括号
    (setq show-paren-ring-bell-on-mismatch t)
    (setq show-paren-style 'parentheses)
    ;; (set-face-attribute 'show-paren-match nil :background nil :underline "#13F811")
    ;;'(show-paren-match ((t (:background "steelblue3" :underline "lawn green")))))
    (custom-set-faces '(show-paren-match ((t (:background "steelblue3" :foreground "yellow" :underline "yellow")))))
    ))

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

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;;add the % jump function in vim
;;ref: http://docs.huihoo.com/homepage/shredderyin/emacs_elisp.html
;;ref: emacs FAQ info doc "Matching parentheses"
(defun match-paren (arg)
  "Go to the matching paren if on a paren;otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)

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
;; (global-set-key [?\M-;] 'comment-or-uncomment-region)    ;; 批量注释
;; (defun my-comment-or-uncomment-region (beg end &optional arg)
;;   (interactive (if (use-region-p)
;;                    (list (region-beginning) (region-end) nil)
;;                  (list (line-beginning-position)
;;                        (line-beginning-position 2))))
;;   (comment-or-uncomment-region beg end arg)
;; )
;; (global-set-key [remap comment-or-uncomment-region] 'my-comment-or-uncomment-region)

(use-package newcomment
  :ensure nil
  :config
  (progn
    ;; Change the behavior of `M-;' by commenting line.
    ;; Much simpler than emacs-25 `comment-line'.
    (defun comment--advice-dwim (old--fn &rest args)
      (if (region-active-p)
          (apply old--fn args)
        (save-excursion
          (goto-char (point-at-bol))
          (push-mark (point-at-eol) t t)
          (apply old--fn args))
        (indent-region (point-at-bol) (point-at-eol))
        (forward-line 1)
        (back-to-indentation)))
    (advice-add 'comment-dwim :around 'comment--advice-dwim)))

;;----------------------------------------------------------------------------
;; 二次选择高亮
;;----------------------------------------------------------------------------
(use-package volatile-highlights
  :config
  (volatile-highlights-mode))

;;----------------------------------------------------------------------------
;; line-numbers
;;----------------------------------------------------------------------------
(add-hook 'find-file-hook 'linum-mode)
(setq linum-format "%d| ")               ;;set format
;; (setq linum-format "%4d \u2502 ")           ;; "\u2502"="|"
;; ;; (setq display-line-numbers-type 'visual)
;; (add-hook 'find-file-hook 'display-line-numbers-mode)
;; (global-linum-mode t)
;; (setq linum-format "%4d\u2502 ")
;;----------------------------------------------------------------------------
;; title
;;----------------------------------------------------------------------------
(setq frame-title-format " %f")       ;; 标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号
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

;; 高亮非ASCII字符
;; 高亮非ASCII字符   M-x highlight-regexp (C-x w h) RET [^[:ascii:]] RET
;; 取消高亮          M-x unhighlight-regexp (C-x w h)RET [^[:ascii:]] RET
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  ;; (occur "[^[:ascii:]]"))
  (highlight-regexp "[^[:ascii:]]"))


(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
