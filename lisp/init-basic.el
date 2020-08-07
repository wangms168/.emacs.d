;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Basic configuration.
;;
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Personal information
(setq user-full-name centaur-full-name)
(setq user-mail-address centaur-mail-address)

;; Key Modifiers
(when sys/win32p
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key

  ;; (w32-register-hot-key [s-])
  (w32-register-hot-key [s-t]))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)
    ))

;; grep matches with background yellow and foreground black
(setenv "GREP_COLORS" "ms=30;43:mc=30;43:sl=01;37:cx=:fn=35:ln=32:bn=32:se=36")

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
(xterm-mouse-mode 1)                         ;; 终端下鼠标响应
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
 ;; '(blink-cursor-mode nil)                  ;; 取消光标闪烁
 ;; '(font-use-system-font t)
 '(show-paren-mode t)                      ;;光标位于括号之后显示匹配的括号
 )

;; Miscs
(display-splash-screen)                   ;;Welcome to GNU Emacs
;;(display-about-screen)                  ;;About Emacs
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
(with-eval-after-load 'hl-line (set-face-background hl-line-face "#212121" ))


;; Start server
;; (use-package server
;;  :ensure nil
;;  :hook (after-init . server-mode))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (add-hook 'after-init-hook (lambda ()
;;                              (unless (or (daemonp) (server-running-p))
;;                                (server-start)
;;                                (setq server-raise-frame t))))

;;----------------------------------------------------------------------------
;; emacs-backup-config
;;----------------------------------------------------------------------------
;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    ;; (load custom-file))
    (load-file custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

;; https://www.emacswiki.org/emacs/ForceBackups
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
;; Default and per-save backups go here:
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup/per-save" user-emacs-directory))))
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist `(("." . ,(expand-file-name "backup/per-session" user-emacs-directory))))
	  (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          ".cache"
                          ".cask"
                          ".elfeed"
                          "bookmarks"
                          "cache"
                          "ido.*"
                          "persp-confs"
                          "recentf"
                          "url"
                          "COMMIT_EDITMSG\\'")))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
