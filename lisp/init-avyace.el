(use-package avy
  :functions (hydra-avy hydra-viewer)
  :bind
  ("C-'"   . avy-resume)
  ("C-:"   . avy-goto-char-2-below)
  ("C-;"   . avy-goto-char)
  ("M-j"   . hydra-avy/body)
  ("C-M-v" . hydra-viewer/body)
  :preface
  ;; fixed cursor scroll-up
  (defun scroll-up-in-place (n)
    (interactive "p")
    (forward-line (- n))
    (scroll-down n))
  ;; fixed cursor scroll-down
  (defun scroll-down-in-place (n)
    (interactive "p")
    (forward-line n)
    (scroll-up n))
  ;; yank inner sexp
  (defun yank-inner-sexp ()
    (interactive)
    (backward-list)
    (mark-sexp)
    (copy-region-as-kill (region-beginning) (region-end)))
  :config
  (when (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "C-:") 'avy-goto-char)
      (global-set-key (kbd "C-;") 'avy-goto-char-2-below)))

  (use-package avy-zap
    :bind
    ("M-z" . avy-zap-to-char-dwim)
    ("M-z" . avy-zap-up-to-char-dwim))

  (with-eval-after-load 'hydra
    (defhydra hydra-viewer (:color pink :hint nil)
      "
                                                                        ╔════════╗
   Char/Line^^^^^^  Word/Page^^^^^^^^  Line/Buff^^^^   Paren                              ║ Window ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
       ^^_k_^^          ^^_u_^^          ^^_g_^^       _(_ ← _y_ → _)_
       ^^^↑^^^          ^^^↑^^^          ^^^↑^^^       _,_ ← _/_ → _._
   _h_ ← _d_ → _l_  _H_ ← _D_ → _L_  _a_ ← _K_ → _e_
       ^^^↓^^^          ^^^↓^^^          ^^^↓^
       ^^_j_^^          ^^_n_^^          ^^_G_
  ╭──────────────────────────────────────────────────────────────────────────────╯
                           [_q_]: quit, [_<SPC>_]: center
          "
      ("j" scroll-down-in-place)
      ("k" scroll-up-in-place)
      ("l" forward-char)
      ("d" delete-char)
      ("h" backward-char)
      ("L" forward-word)
      ("H" backward-word)
      ("u" scroll-up-command)
      ("n" scroll-down-command)
      ("D" delete-word-at-point)
      ("a" mwim-beginning-of-code-or-line)
      ("e" mwim-end-of-code-or-line)
      ("g" beginning-of-buffer)
      ("G" end-of-buffer)
      ("K" kill-whole-line)
      ("(" backward-list)
      (")" forward-list)
      ("y" yank-inner-sexp)
      ("." backward-forward-next-location)
      ("," backward-forward-previous-location)
      ("/" avy-goto-char :exit t)
      ("<SPC>" recenter-top-bottom)
      ("q" nil))

    (defhydra hydra-avy (:color pink :hint nil)
      "
                                                                        ╔════════╗
        ^^Goto^^        Kill^^        Yank^^        Move^^        Misc            ║  Jump  ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
    _c_ ← char^^        [_k_] region  [_y_] region  [_m_] region  [_n_] line number
    _a_ ← char2 → _b_   [_K_] line    [_Y_] line    [_M_] line    [_v_] Goto viewer
    _w_ ← word  → _W_   [_z_] zap^^^^                             [_o_] Goto clock
    _l_ ← line  → _e_   ^^^^^                                     _,_ ← f!y → _._
  ╭──────────────────────────────────────────────────────────────────────────────╯
                      [_q_]: quit, [_i_]: imenu, [_<SPC>_]: resume
"
      ("c" avy-goto-char :exit t)
      ("a" avy-goto-char-2 :exit t)
      ("b" avy-goto-char-below :exit t)
      ("w" avy-goto-word-1 :exit t)
      ("W" avy-goto-word-1-below :exit t)
      ("l" avy-goto-line :exit t)
      ("e" avy-goto-end-of-line :exit t)
      ("M" avy-move-line)
      ("m" avy-move-region)
      ("K" avy-kill-whole-line)
      ("k" avy-kill-region)
      ("Y" avy-copy-line :exit t)
      ("y" avy-copy-region :exit t)
      ("n" goto-line :exit t)
      ("o" org-clock-jump-to-current-clock :exit t)
      ("z" avy-zap-to-char-dwim :exit t)
      ("v" hydra-viewer/body :exit t)
      ("<SPC>" avy-resume :exit t)
      ("o" org-clock-jump-to-current-clock :exit t)
      ("i" counsel-imenu :exit t)
      ("," flymake-goto-previous-error)
      ("." flymake-goto-next-error)
      ("q" nil))))

(use-package ace-window
  :functions hydra-frame-window/body
  :bind
  ("C-M-o" . hydra-frame-window/body)
  ;; ("M-t m" . ladicle/toggle-window-maximize)
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  :preface
  (defvar is-window-maximized nil)
  (defun ladicle/toggle-window-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
	  (balance-windows)
	(maximize-window))
      (setq is-window-maximized
	    (not is-window-maximized))))
  (defun hydra-title(title) (propertize title 'face `(:inherit font-lock-warning-face :weight bold)))
  (defun command-name(title) (propertize title 'face `(:foreground "#f8f8f2")))
  (defun spacer() (propertize "." 'face `(:foreground "#282a36")))
  :config
  (use-package rotate
    :load-path "~/Developments/src/github.com/Ladicle/dotfiles/common/emacs.d/elisp/emacs-rotate"
    :bind
    ("M-o SPC" . rotate-layout))
  (with-eval-after-load 'hydra
    (defhydra hydra-frame-window (:color pink :hint nil)
      "
                                                          ╔════════╗
      ^^Size^^    ^Zoom^   ^Split^    Frame^^             Buffer    ║ Window ║
  ────────────────────────────────────────────────────────╨────────╜
        ^_k_^       _+_      _-_      [_i_] select        _,_ ← switch  → _._
        ^^↑^^       ^↑^      ^↑^      [_s_] swap          [_d_] delete
    _h_ ←   → _l_   ^ ^      ^ ^      [_o_] next          [_D_] delete abd kill
        ^^↓^^       ^↓^      ^↓^      [_m_] maximize      [_r_] recentf
        ^_j_^       _=_      _/_      [_O_] delete others [_k_] select
  ╭────────────────────────────────────────────────────────────────╯
                      [_q_]: quit, [_<SPC>_]: resume
"

      ;;       (format
      ;;        (format "%s" (propertize "                                                                       ╔════════╗
      ;;     ((%s))^^^^^^^^   ((%s))^^^^  ((%s))^^  ((%s))^^  ((%s))^^^^^^  ((%s))^   ║ Window ║
      ;; ^^^^^^ ──────────────────────────────────────────────────────────────────────╨────────╜
      ;;         ^_k_^        %s_+_         _-_       %s     _,_ ← %s → _._^  %s
      ;;         ^^↑^^          ^↑^         ^↑^       %s
      ;;     _h_ ←   → _l_   ^^%s%s^^^^^    ^%s    ^^^%s^^^^     %s
      ;;         ^^↓^^          ^↓^         ^↓^       %s^^       %s
      ;;         ^_j_^        %s_=_         _/_       %s
      ;; ^^^^^^ ┌──────────────────────────────────────────────────────────────────────────────┘
      ;;                            [_q_]: %s, [_<SPC>_]: %s" 'face `(:inherit font-lock-doc-face)))
      ;;        (hydra-title "Size")
      ;;        (hydra-title "Zoom")
      ;;        (hydra-title "Split")
      ;;        (hydra-title "Window")
      ;;        (hydra-title "Buffer")
      ;;        (hydra-title "Misc")
      ;;        (all-the-icons-material "zoom_in" :height .85 :face 'font-lock-doc-face)
      ;;        (command-name "_o_ther")
      ;;        (command-name "page")
      ;;        (command-name "_r_centf")
      ;;        (command-name "_s_wap")
      ;;        (all-the-icons-faicon "slideshare" :height .85 :face 'font-lock-doc-face)
      ;;        (command-name "_p_mode")
      ;;        (command-name "w_i_ndow")
      ;;        (command-name "_m_aximize")
      ;;        (command-name "_s_witch")
      ;;        (command-name "_d_elete")
      ;;        (command-name "_D_elete")
      ;;        (all-the-icons-material "zoom_out" :height .85 :face 'font-lock-doc-face)
      ;;        (command-name "del_O_thers")
      ;;        (command-name "quit")
      ;;        (command-name "rotate")
      ;;        )

      ("K" kill-current-buffer :exit t)
      ("D" kill-buffer-and-window :exit t)
      ("O" delete-other-windows  :exit t)
      ("F" toggle-frame-fullscreen)
      ("i" ace-window)
      ("s" ace-swap-window :exit t)
      ("d" ace-delete-window)
      ("m" ladicle/toggle-window-maximize :exit t)
      ("=" text-scale-decrease)
      ("+" text-scale-increase)
      ("-" split-window-vertically)
      ("/" split-window-horizontally)
      ("h" shrink-window-horizontally)
      ("k" shrink-window)
      ("j" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("," previous-buffer)
      ("." next-buffer)
      ("o" other-window)
      ("p" presentation-mode)
      ("r" counsel-recentf :exit t)
      ("s" switch-to-buffer :exit t)
      ("D" kill-buffer-and-window)
      ("<SPC>" rotate-layout)
      ("q" nil)))
  )


(provide 'init-avyace)
