;;; init-editor.el --- Initialize editor configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; -----------------------------------------------------------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)         ;; 用'y'和'n'来代替频繁地输入'yes’和'no'
(blink-cursor-mode -1)                    ;; 取消光标闪烁
;; (setq-default cursor-type 'bar)           ;; 设置光标为小长条形状.设置这个，智能改变光标形状不起作用。

;; ----------------------------------------------------------------------------------------------------------------------------
;;ee;; (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;; (add-hook 'god-mode-disabled-hook 'my-update-cursor)


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
