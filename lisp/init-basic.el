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
