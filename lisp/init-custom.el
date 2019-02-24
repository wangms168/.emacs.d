;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; (defgroup centaur nil
;;   "Centaur Emacs customizations."
;;   :group 'convenience)

(defcustom centaur-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set Centaur logo.nil means official logo."
  :type 'string)

(defcustom centaur-full-name "wangms168"
  "Set user full name."
  :type 'string)

(defcustom centaur-mail-address "wms_88@163.com"
  "Set user email address."
  :type 'string)

;; (defcustom centaur-proxy "127.0.0.1:1087"
;;   "Set network proxy."
;;   :type 'string)

(defcustom centaur-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)))

(defcustom centaur-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Classic theme" classic)
          (const :tag "Doom theme" doom)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Daylight theme" daylight)
          symbol))

(defcustom centaur-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :type 'boolean)

;; (defcustom centaur-lsp 'lsp-mode
;;   "Set language server."
;;   :type '(choice
;;           (const :tag "LSP Mode" 'lsp-mode)
;;           (const :tag "eglot" 'eglot)
;;           nil))

;; (defcustom centaur-benchmark nil
;;   "Enable the init benchmark or not."
;;   :type 'boolean)

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
