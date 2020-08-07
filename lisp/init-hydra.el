;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

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
;; Nice looking hydras.
;;

;;; Code:

(use-package hydra
  :defer 0.1
  :init
  (bind-key "\\" 'hydra-master/body)
  :config
  (setq lv-use-separator t)
  (set-face-attribute 'hydra-face-pink nil :foreground "deep sky blue" :weight 'bold)

  (eval-and-compile
    (defhydra hydra-common (:color pink)
      ("<ESC>" nil "quit")))

  (defhydra hydra-master (:color pink :idle 0.4)
    "
                                                                       ╭───────┐
                                                                       │ Index │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_a_] bookmarks    [^h^]               [_o_] organization  [^v^]
  [_b_] buffers      [_i_] internet      [_p_] project       [_w_] window
  [_c_] flycheck     [_j_] jump          [_q_] exit          [_x_] shell
  [_d_] development  [_k_] spell         [_r_] register      [^y^]
  [_e_] emacs        [_l_] lisp          [_s_] search        [^z^]
  [_f_] file         [_m_] media         [_t_] text
  [_g_] git          [_n_] narrow        [^u^]
--------------------------------------------------------------------------------
    "
    ("<SPC>" joe-alternate-buffers "alternate buffers")
    ("<ESC>" nil "quit")
    ("\\" (insert "\\") "\\")
    ("a"     hydra-bookmarks/body nil)
    ("b"     hydra-buffers/body nil)
    ("c"     hydra-flycheck/body nil)
    ("d"     hydra-development/body nil)
    ("e"     hydra-emacs/body nil)
    ("f"     hydra-file/body nil)
    ("g"     hydra-git/body nil)
    ("i"     hydra-internet/body nil)
    ("j"     hydra-jump/body nil)
    ("k"     hydra-spell/body nil)
    ("l"     hydra-lisp/body nil)
    ("m"     hydra-media/body nil)
    ("n"     hydra-narrow/body nil)
    ("o"     hydra-organization/body nil)
    ("p"     hydra-project/body nil)
    ("q"     hydra-exit/body nil)
    ("r"     hydra-register/body nil)
    ("s"     hydra-search/body nil)
    ("t"     hydra-text/body nil)
    ("w"     ace-window nil)
    ("x"     hydra-system/body nil))

  (defhydra hydra-bookmarks (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                   ╭───────────┐
       List                          Do                            │ Bookmarks │
╭──────────────────────────────────────────────────────────────────┴───────────╯
  [_l_] list bookmarks            [_j_] jump to a bookmark
   ^ ^                            [_m_] set bookmark at point
   ^ ^                            [_s_] save bookmarks
--------------------------------------------------------------------------------
    "
    ("l" counsel-bookmark)
    ("j" bookmark-jump)
    ("m" bookmark-set)
    ("s" bookmark-save))

  (defhydra hydra-buffers (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                     ╭─────────┐
  Switch                 Do                                          │ Buffers │
╭────────────────────────────────────────────────────────────────────┴─────────╯
  [_b_] switch             [_d_] kill the buffer

  [_i_] ibuffer            [_r_] toggle read-only mode
  [_a_] alternate          [_u_] revert buffer changes
   ^ ^                     [_w_] save buffer
--------------------------------------------------------------------------------
    "
    ("a" joe-alternate-buffers)
    ("b" ivy-switch-buffer)
    ("d" kill-this-buffer)
    ("i" ibuffer)
    ("r" read-only-mode)
    ("u" joe-revert-buffer)
    ("w" save-buffer))

  (defhydra hydra-flycheck (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
   Navigate          Show Errors                  Do                │ Flycheck │
╭───────────────────────────────────────────────────────────────────┴──────────╯
   ^_p_^revious     [_l_] list errors           [_t_] toggle Flycheck
      ^^↑^^         [_d_] clear all errors      [_c_] select checker
    ^_f_^irst        ^ ^                        [_r_] run via compile
      ^^↓^^          ^ ^                        [_h_] describe checker
    ^_n_^ext
--------------------------------------------------------------------------------
      "
    ("c" flycheck-select-checker)
    ("h" flycheck-describe-checker)
    ("d" flycheck-clear)
    ("f" flycheck-first-error)
    ("l" flycheck-list-errors)
    ("n" flycheck-next-error :color red)
    ("p" flycheck-previous-error :color red)
    ("r" flycheck-compile)
    ("t" flycheck-mode))

  (defhydra hydra-development (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                 ╭─────────────┐
     Code                   Web                 Quickrun         │ Development │
╭────────────────────────────────────────────────────────────────┴─────────────╯
  [_d_] search docs (at point) [_c_] Web Colors          [_q_] buffer
   ^ ^                         [_h_] HTTP header         [_v_] region
   ^ ^                         [_m_] HTTP method         [_x_] shell
   ^ ^                         [_r_] HTTP relation       [_p_] with arg
   ^ ^                         [_s_] HTTP status code    [_o_] only compile
   ^ ^                         [_t_] Media types         [_R_] replace
   ^ ^                         [_g_] RESTclient          [_e_] eval/print
   ^ ^                         [_f_] RFC doc
  [_l_] lines of code          [_F_] RFC index
--------------------------------------------------------------------------------
      "
    ("d" devdocs-search)
    ("c" counsel-colors-web)
    ("g" restclient-mode)
    ("f" irfc-visit)
    ("F" irfc-index)
    ("q" quickrun)
    ("v" quickrun-region)
    ("x" quickrun-shell)
    ("p" quickrun-with-arg)
    ("o" quickrun-compile-only)
    ("R" quickrun-replace-region)
    ("e" quickrun-eval-print)
    ("h" http-header)
    ("m" http-method)
    ("r" http-relation)
    ("s" http-status-code)
    ("t" media-type)
    ("l" cloc))

  (defhydra hydra-emacs (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Execute       Packages         Help                     Misc        │ Emacs │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_x_] counsel M-x [_p_] list      [_f_] describe function [_t_] change theme
   ^ ^              [_i_] install   [_v_] describe variable [_l_] list emacs process
   ^ ^              [_u_] upgrade   [_m_] info manual       [_c_] init time
   ^ ^               ^ ^            [_k_] bindings          [_e_] benchmark init
   ^ ^               ^ ^            [_b_] personal bindings [_o_] unbound commands
   ^ ^               ^ ^             ^ ^                    [_y_] emacs colors
   ^ ^               ^ ^             ^ ^                    [_z_] list faces
--------------------------------------------------------------------------------
      "
    ("C-h b" counsel-descbinds "bindings")
    ("f" counsel-describe-function)
    ("v" counsel-describe-variable)
    ("b" describe-personal-keybindings)
    ("c" emacs-init-time)
    ("i" package-install)
    ("k" counsel-descbinds)
    ("l" list-processes)
    ("m" info-display-manual)
    ("p" paradox-list-packages)
    ("t" counsel-load-theme)
    ("u" paradox-upgrade-packages)
    ("e" esup)
    ("o" smex-show-unbound-commands)
    ("y" counsel-colors-emacs)
    ("z" counsel-faces)
    ("x" counsel-M-x))

  (defhydra hydra-file (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
     Ivy                    Dired        Ztree                          │ File │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_o_] open file        [_d_] dired         [_z_] diff dirs
  [_e_] open file extern [_r_] ranger
--------------------------------------------------------------------------------
      "
    ("o" counsel-find-file)
    ("e" counsel-find-file-extern)
    ("z" ztree-diff)
    ("d" dired)
    ("r" ranger))


  (defhydra hydra-text (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
 Size  Toggle              Unicode                        Do            │ Text │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  _k_  [_f_] fill column     [_d_] unicode character           [_a_] align with regex
  ^↑^  [_h_] hidden chars    [_e_] evil digraphs table         [_w_] remove trailing ' '
  ^ ^  [_l_] line numbers    [_s_] specific code block         [_n_] count words
  ^↓^  [_t_] trailing ' '    [_u_] unicode character           [_i_] lorem ipsum
  _j_  [_v_] font space      [_p_] character code              [_x_] comment box
  ^ ^  [_c_] comment          ^ ^                              [_q_] boxquote
  ^ ^  [_b_] multibyte chars  ^ ^                              [_m_] iedit (multiple)
  ^ ^   ^ ^                   ^ ^                              [_r_] expand region
  ^ ^   ^ ^                   ^ ^                              [_U_] tabs to spaces
--------------------------------------------------------------------------------
      "
    ("a" align-regexp)
    ("b" toggle-enable-multibyte-characters)
    ("c" comment-line)
    ("d" insert-char)
    ("e" evil-ex-show-digraphs)
    ("f" fci-mode)
    ("h" whitespace-mode)
    ("i" lorem-ipsum-insert-paragraphs)
    ("k" text-scale-increase :color red)
    ("j" text-scale-decrease :color red)
    ("l" linum-mode)
    ("n" count-words)
    ("m" iedit)
    ("p" describe-char)
    ("r" er/expand-region)
    ("s" charmap)
    ("t" joe-toggle-show-trailing-whitespace)
    ("u" counsel-unicode-char)
    ("v" variable-pitch-mode)
    ("w" whitespace-cleanup)
    ("U" untabify)
    ("q" hydra-boxquote/body)
    ("x" comment-box))

  (defhydra hydra-git (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                         ╭─────┐
   Magit                          VC                    Timemachine      │ Git │
╭────────────────────────────────────────────────────────────────────────┴─────╯
  [_s_] status              [_d_] diffs between revisions  [_t_] timemachine
  [_B_] blame mode          [_b_] edition history
  [_l_] file log
--------------------------------------------------------------------------------
      "
    ("B" magit-blame-mode)
    ("b" vc-annotate)
    ("d" vc-diff)
    ("l" magit-file-log)
    ("s" magit-status)
    ("t" git-timemachine))

  (defhydra hydra-internet (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
    Browse       Search              Social               Post      │ Internet │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_w_] eww      [_b_] DuckDuckGo       [_f_] elfeed            [_i_] imgur
  [_u_] url      [_e_] DuckDuckGo (eww) [_x_] stack overflow
   ^ ^           [_m_] google maps
   ^ ^           [_d_] wordnik
--------------------------------------------------------------------------------
      "
    ("w" eww)
    ("u" browse-url-at-point)
    ("b" (joe-duckduckgo-search t))
    ("e" (joe-duckduckgo-search nil))
    ("m" google-maps)
    ("d" define-word-at-point)
    ("f" elfeed)
    ("x" sx-tab-newest)
    ("i" imgur-post))

  (defhydra hydra-jump (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
  Window          Word/Char        Line         iSearch                 │ Jump │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_w_] jump        [_j_] word         [_l_] jump     [_i_] jump
  [_d_] close       [_p_] all words    [_y_] copy
  [_z_] maximize    [_b_] subword      [_m_] move
  [_s_] swap        [_c_] char         [_v_] copy region
   ^ ^              [_a_] two chars
--------------------------------------------------------------------------------
      "
    ("w" ace-window)
    ("d" ace-delete-window)
    ("z" ace-maximize-window)
    ("s" ace-swap-window)
    ("j" avy-goto-word-1)
    ("p" avy-goto-word-0)
    ("b" avy-goto-subword-0)
    ("c" avy-goto-char)
    ("a" avy-goto-char-2)
    ("l" avy-goto-line)
    ("y" avy-copy-line)
    ("m" avy-move-line)
    ("v" avy-copy-region)
    ("i" avy-isearch))

  (defhydra hydra-spell (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
    Flyspell               Ispell                      Gtranslate      │ Spell │
╭──────────────────────────────────────────────────────────────────────┴───────╯
  [_k_] correct word       [_w_] check word            [_g_] en ⇆ es
  [_n_] next error         [_t_] toggle dictionary     [_G_] any lang
  [_f_] toggle flyspell    [_d_] change dictionary
  [_p_] toggle prog mode
--------------------------------------------------------------------------------
      "
    ("w" ispell-word)
    ("d" ispell-change-dictionary)
    ("t" joe-switch-dictionary)
    ("g" google-translate-smooth-translate)
    ("G" google-translate-query-translate)
    ("f" flyspell-mode)
    ("p" flyspell-prog-mode)
    ("k" flyspell-correct-word-generic)
    ("n" flyspell-goto-next-error))

  (defhydra hydra-lisp (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
    Elisp              Bug hunter                                       │ Lisp │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_r_] eval region    [_f_] file
  [_s_] eval sexp      [_i_] init-file
  [_b_] eval buffer
--------------------------------------------------------------------------------
      "
    ("f" bug-hunter-file)
    ("i" bug-hunter-init-file)
    ("r" eval-region)
    ("b" eval-buffer)
    ("s" eval-last-sexp))

  (defhydra hydra-narrow (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
    Narrow                                                            │ Narrow │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_f_] narrow to defun
  [_p_] narrow to page
  [_r_] narrow to region
  [_w_] widen
--------------------------------------------------------------------------------
      "
    ("f" narrow-to-defun)
    ("p" narrow-to-page)
    ("r" narrow-to-region)
    ("w" widen))

  (defhydra hydra-project (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                  ╭────────────┐
  Files             Search          Buffer             Do         │ Projectile │
╭─────────────────────────────────────────────────────────────────┴────────────╯
  [_f_] file          [_a_] ag          [_b_] switch         [_g_] magit
  [_l_] file dwim     [_A_] grep        [_v_] show all       [_p_] commander
  [_r_] recent file   [_s_] occur       [_V_] ibuffer        [_i_] info
  [_d_] dir           [_S_] replace     [_K_] kill all
  [_o_] other         [_t_] find tag
  [_u_] test file     [_T_] make tags
  [_h_] root
                                                                      ╭────────┐
  Other Window      Run             Cache              Do             │ Fixmee │
╭──────────────────────────────────────────────────╯ ╭────────────────┴────────╯
  [_F_] file          [_U_] test        [_kc_] clear         [_x_] TODO & FIXME
  [_L_] dwim          [_m_] compile     [_kk_] add current   [_X_] toggle
  [_D_] dir           [_c_] shell       [_ks_] cleanup
  [_O_] other         [_C_] command     [_kd_] remove
  [_B_] buffer
--------------------------------------------------------------------------------
      "
    ("a"   projectile-ag)
    ("A"   projectile-grep)
    ("b"   projectile-switch-to-buffer)
    ("B"   projectile-switch-to-buffer-other-window)
    ("c"   projectile-run-async-shell-command-in-root)
    ("C"   projectile-run-command-in-root)
    ("d"   projectile-find-dir)
    ("D"   projectile-find-dir-other-window)
    ("f"   projectile-find-file)
    ("F"   projectile-find-file-other-window)
    ("g"   projectile-vc)
    ("h"   projectile-dired)
    ("i"   projectile-project-info)
    ("kc"  projectile-invalidate-cache)
    ("kd"  projectile-remove-known-project)
    ("kk"  projectile-cache-current-file)
    ("K"   projectile-kill-buffers)
    ("ks"  projectile-cleanup-known-projects)
    ("l"   projectile-find-file-dwim)
    ("L"   projectile-find-file-dwim-other-window)
    ("m"   projectile-compile-project)
    ("o"   projectile-find-other-file)
    ("O"   projectile-find-other-file-other-window)
    ("p"   projectile-commander)
    ("r"   projectile-recentf)
    ("s"   projectile-multi-occur)
    ("S"   projectile-replace)
    ("t"   projectile-find-tag)
    ("T"   projectile-regenerate-tags)
    ("u"   projectile-find-test-file)
    ("U"   projectile-test-project)
    ("v"   projectile-display-buffer)
    ("V"   projectile-ibuffer)
    ("X"   fixmee-mode)
    ("x"   fixmee-view-listing))

  (defhydra hydra-exit (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                        ╭──────┐
   Quit                                                                 │ Exit │
╭───────────────────────────────────────────────────────────────────────┴──────╯
  [_c_] exit emacs (standalone or client)
  [_s_] shutdown the emacs daemon
--------------------------------------------------------------------------------
      "
    ("c" save-buffers-kill-terminal)
    ("s" save-buffers-kill-emacs))

  (defhydra hydra-register (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                    ╭──────────┐
   Logs                        Registers                Undo        │ Register │
╭───────────────────────────────────────────────────────────────────┴──────────╯
  [_c_] commands history       [^e^] emacs registers    [_u_] undo tree
  [_o_] messages               [_r_] evil registers
  [_l_] lossage (keystrokes)   [_m_] evil marks
  [_d_] diff buffer with file  [_k_] kill ring
--------------------------------------------------------------------------------
      "
    ("d" joe-diff-buffer-with-file)
    ("k" counsel-yank-pop)
    ("l" view-lossage)
    ("c" counsel-command-history)
    ("m" evil-show-marks)
    ("o" view-echo-area-messages)
    ("r" evil-show-registers)
    ("u" undo-tree-visualize))

  (defhydra hydra-search (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
   Files                             Buffer                           │ Search │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_a_] regex search (Ag)           [_b_] by word
  [_r_] regex search (rg)           [_o_] by word (opened buffers)
  [_p_] regex search (pt)           [_w_] by word (multi)
  [_g_] regex search (grep)         [_h_] by word (grep or swiper)
  [^f^] find                        [_t_] tags & titles
  [_l_] locate                      [_s_] semantic
--------------------------------------------------------------------------------
      "
    ("a" (let ((current-prefix-arg "-."))
	   (call-interactively 'counsel-ag)))
    ("r" (let ((current-prefix-arg "-."))
	   (call-interactively 'counsel-rg)))
    ("p" (let ((current-prefix-arg "-."))
	   (call-interactively 'counsel-pt)))
    ("g" rgrep)
    ("l" counsel-locate)
    ("b" swiper)
    ("o" swiper-all)
    ("h" counsel-grep-or-swiper)
    ("t" counsel-imenu)
    ("s" counsel-semantic)
    ("w" swiper-multi))

  (defhydra hydra-system (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                      ╭────────┐
   Terminals                     System                               │ System │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_s_] new multi-term           [_c_] shell command
  [_n_] next multi-term          [_a_] aync shell command
  [_p_] previous multi-term      [_m_] man page
  [_d_] dedicated multi-term     [_l_] list system process
  [_e_] eshell
--------------------------------------------------------------------------------
      "
    ("a" async-shell-command)
    ("c" shell-command)
    ("e" eshell)
    ("m" man)
    ("l" proced)
    ("s" multi-term)
    ("n" multi-term-next)
    ("p" multi-term-previous)
    ("d" multi-term-dedicated-toggle))

  (defhydra hydra-media (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                       ╭───────┐
   Mingus              Mpd                     Volume                  │ Media │
╭──────────────────────────────────────────────────────────────────────┴───────╯
 [_m_] mingus         [_n_] next song          [_-_] volume down
 [_f_] search         [_p_] previous song      [_+_] volume up
 [_l_] playlist       [_c_] clear playlist
 [_a_] All            [_t_] pause
  ^ ^                 [_s_] stop
  ^ ^                 [_d_] start daemon
--------------------------------------------------------------------------------
      "
    ("m" mingus)
    ("f" mingus-search)
    ("c" mingus-clear)
    ("n" mingus-next)
    ("p" mingus-prev)
    ("t" mingus-toggle)
    ("s" mingus-stop)
    ("d" mingus-start-daemon)
    ("l" mingus-load-playlist)
    ("a" mingus-load-all)
    ("-" mingus-vol-down)
    ("\+" mingus-vol-up))

  (defhydra hydra-organization (:color pink :hint nil :idle 0.4 :inherit (hydra-common/heads))
    "
                                                                ╭──────────────┐
     Tasks            Org mode               Comms      Others  │ Organization │
╭───────────────────────────────────────────────────────────────┴──────────────╯
  [_a_] agenda      [_c_] capture             [_m_] mail      [_x_] speed type
  [_l_] agenda list [_p_] pomodoro            [_t_] contacts
  [_d_] calendar    [_s_] search headings     [_h_] add location
   ^ ^              [_g_] open location gmaps
   ^ ^              [_f_] archive subtree
--------------------------------------------------------------------------------
      "
    ("a" org-agenda)
    ("c" org-capture)
    ("s" counsel-org-agenda-headlines)
    ("d" cfw:open-org-calendar)
    ("g" org-location-google-maps)
    ("h" org-address-google-geocode-set)
    ("l" org-agenda-list)
    ("f" org-archive-subtree)
    ("m" mu4e)
    ("p" org-pomodoro)
    ("t" org-contacts)
    ("x" speed-type-text))

  (defhydra hydra-leader ( :color pink :hint nil :idle 0.4)
    "
                                                                      ╭────────┐
   Toggle                        Do                                   │ Leader │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  [_c_] comment                  [_a_] align with regex
  [_f_] fill column              [_p_] show character code
  [_h_] hidden chars             [_i_] insert unicode character
  [_e_] trailing whitespace      [_<SPC>_] remove trailing whitespaces
  [_v_] font space               [_u_] undo tree
   ^ ^                           [_j_] jump word
   ^ ^                           [_x_] comment box
   ^ ^                           [_r_] expand region
   ^ ^                           [_m_] iedit (multiple edit)
   ^ ^                           [_g_] google translate
   ^ ^                           [_s_] swiper
   ^ ^                           [_t_] counsel imenu
   ^ ^                           [_q_] quick-calc
--------------------------------------------------------------------------------
      "
    ("<escape>" nil "quit")
    ("a" align-regexp)
    ("c" comment-line)
    ("r" er/expand-region)
    ("f" fci-mode)
    ("g" google-translate-smooth-translate)
    ("h" whitespace-mode)
    ("i" counsel-unicode-char)
    ("j" avy-goto-word-1)
    ("m" iedit-mode)
    ("n" count-words)
    ("p" describe-char)
    ("e" joe-toggle-show-trailing-whitespace)
    ("u" undo-tree-visualize)
    ("v" variable-pitch-mode)
    ("<SPC>" whitespace-cleanup)
    ("s" joe-swiper)
    ("t" counsel-imenu)
    ("q" (quick-calc t))
    ("x" comment-box)))

;; (eval-when-compile
;;   (require 'init-custom))

;; (use-package hydra
;;   :commands (hydra-default-pre
;;              hydra-keyboard-quit
;;              hydra--call-interactively-remap-maybe
;;              hydra-show-hint
;;              hydra-set-transient-map))


(use-package posframe :ensure t)
(use-package hydra-posframe
  :load-path "site-lisp/hydra-posframe"
  :custom
  (hydra-posframe-parameters
   '((left-fringe . 5)
     (right-fringe . 5)))
  :custom-face
  (hydra-posframe-border-face ((t (:background "#6272a4"))))
  :hook (after-init . hydra-posframe-enable))

(use-package pretty-hydra
  :functions set-package-archives centaur-load-theme
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")                 ;; :color amaranth
					     :color amaranth :quit-key "q")
    ("Basic"
     (("n" display-line-numbers-mode "line number" :toggle t)
      ("N" linum-mode "legacy line number" :toggle t)
      ("a" aggressive-indent-mode "aggressive indent" :toggle t)
      ("h" hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("P" flyspell-mode "spell check" :toggle t)
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
      ("L" page-break-lines-mode "page break lines" :toggle t))
     "Highlight"
     (("l" global-hl-line-mode "line" :toggle t)
      ("p" show-paren-mode "paren" :toggle t)
      ("s" symbol-overlay-mode "symbol" :toggle t)
      ("r" rainbow-mode "rainbow" :toggle t)
      ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
       "whitespace" :toggle show-trailing-whitespace)
      ("R" rainbow-delimiters-mode "delimiter" :toggle t)
      ("i" highlight-indent-guides-mode "indent" :toggle t)
      ("t" hl-todo-mode "todo" :toggle t))
     "Coding"
     (("f" flycheck-mode "flycheck" :toggle t)
      ("F" flymake-mode "flymake" :toggle t)
      ("o" origami-mode "folding" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
     "Version Control"
     (("v" diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("m" diff-hl-margin-mode "margin gutter" :toggle t)
      ("E" diff-hl-dired-mode "dired gutter" :toggle t))
     ;; "Theme"
     ;; (("d" (centaur-load-theme 'default) "default"
     ;;   :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'default)))
     ;;  ("c" (centaur-load-theme 'classic) "classic"
     ;;   :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'classic)))
     ;;  ("g" (centaur-load-theme 'light) "light"
     ;;   :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'light)))
     ;;  ("y" (centaur-load-theme 'daylight) "daylight"
     ;;   :toggle (eq (centuar-current-theme) (centaur--standardize-theme 'daylight)))
     ;;  ("M" doom-modeline-mode "modern mode-line" :toggle t)
     ;;  ("T" (let ((ivy-initial-inputs-alist '((counsel-load-theme . "doom-"))))
     ;; 	(counsel-load-theme))
     ;;   "others"))
     "Package Archive"
     (("k m" (progn (setq centaur-package-archives 'melpa)
		    (set-package-archives centaur-package-archives))
       "melpa" :toggle (eq centaur-package-archives 'melpa))
      ("k i" (progn (setq centaur-package-archives 'melpa-mirror)
		    (set-package-archives centaur-package-archives))
       "melpa mirror" :toggle (eq centaur-package-archives 'melpa-mirror))
      ("k c" (progn (setq centaur-package-archives 'emacs-china)
		    (set-package-archives centaur-package-archives))
       "emacs china" :toggle (eq centaur-package-archives 'emacs-china))
      ("k n" (progn (setq centaur-package-archives 'netease)
		    (set-package-archives centaur-package-archives))
       "netease" :toggle (eq centaur-package-archives 'netease))
      ("k t" (progn (setq centaur-package-archives 'tencent)
		    (set-package-archives centaur-package-archives))
       "tencent" :toggle (eq centaur-package-archives 'tencent))
      ("k u" (progn (setq centaur-package-archives 'tuna)
		    (set-package-archives centaur-package-archives))
       "tuna" :toggle (eq centaur-package-archives 'tuna))))))


(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))
(define-key dired-mode-map "." 'hydra-dired/body)

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
