(use-package lsp-mode
  :diminish lsp-mode
  :hook (prog-mode . lsp)
  :bind (("s-b" . xref-find-definitions)
	 ("s-]" . xref-find-definitions)
	 ("s-[" . evil-jump-backward))
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-diagnostic-functions '(lsp--flymake-backend nil))
  :config
  (setq lsp-inhibit-message t
	lsp-message-project-root-warning t
	create-lockfiles nil
	lsp-session-file (expand-file-name "backup/lsp-session" user-emacs-directory ))


  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (require 'lsp-clients)
  )


(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq scroll-margin 0))

(provide 'init-lsp)
