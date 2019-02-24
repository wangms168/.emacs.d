;;; init-editor.el --- Initialize editor configurations.	-*- lexical-binding: t -*-

;; (menu-bar-mode -1)                        ;;never have a retarded menu-bar at top
;; (tool-bar-mode -1)                        ;;never have a retarded tool-bar at top
;; (scroll-bar-mode -1)                      ;;never have a retarded scrill-bar at side
;; (setq initial-frame-alist '((top . 0) (left . 100) (width . 108) (height . 28)))

;; -----------------------------------------------------------------------------------------------------------------------------
(setq inhibit-startup-message t)          ;; 禁用启动画面
(setq-default indicate-empty-lines t)     ;; show (in left margin) marker for empty lines
(defalias 'yes-or-no-p 'y-or-n-p)         ;; 用'y'和'n'来代替频繁地输入'yes’和'no'
(blink-cursor-mode -1)                    ;; 取消光标闪烁
(setq-default cursor-type 'bar)           ;; 设置光标为小长条形状.设置这个，智能改变光标形状不起作用。
;;----------------------------------------------------------------------------
;; paren
;;----------------------------------------------------------------------------
(use-package paren
  :ensure nil
  :config
  (progn
    (show-paren-mode 1)                     ;; 高亮显示成对括号，但不来回弹跳
    (setq show-paren-ring-bell-on-mismatch t)
    (setq show-paren-style 'parentheses)
    ))

;;add the % jump function in vim
;;ref: http://docs.huihoo.com/homepage/shredderyin/emacs_elisp.html
;;ref: emacs FAQ info doc "Matching parentheses"
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)


;; (setq undo-tree-auto-save-history t)     ;;可以撤销关闭重启前的修改
;; (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))


;;----------------------------------------------------------------------------
;; line-numbers
;;----------------------------------------------------------------------------
;;(add-hook 'find-file-hook 'linum-mode)
;;(setq linum-format "%d| ")               ;;set format
;; (setq display-line-numbers-type 'visual)
(add-hook 'find-file-hook 'display-line-numbers-mode)

;;----------------------------------------------------------------------------
;; title
;;----------------------------------------------------------------------------
(setq frame-title-format " %f")       ;; 标题栏显示 %f 缓冲区完整路径 %p 页面百分数 %l 行号
;; (setq column-number-mode t)           ;; 模式栏显示列号
;; (setq line-number-mode t)             ;; 模式栏显示行号

;; ----------------------------------------------------------------------------------------------------------------------------
;;ee;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)


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
;; Copy/paste
;;----------------------------------------------------------------------------
(setq select-active-regions t)
(setq x-select-enable-clipboard-manager nil
      select-enable-clipboard           t
      select-enable-primary             nil)
(delete-selection-mode 1)              ;; 粘贴或键入的文本即可替换活动区域，该替换动作隐含一个Backspace键（‘DEL’）删除所选文本的动作。

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

;;----------------------------------------------------------------------------
;; 二次选择高亮
;;----------------------------------------------------------------------------
(use-package volatile-highlights
  :config
  (volatile-highlights-mode))

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

(provide 'init-editor)
;;; init-editor.el ends here
