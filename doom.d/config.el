;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Giorgos Mitsis"
      user-mail-address "geo_mits@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; This sets the line spacing
(setq-default line-spacing 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; (defun make-youtube-link (youtube_id)
;;   (browser-url (concat "https:/ww.youtube.com/embed/" youtube_id))
;; )

;; (after! org
;;   (org-link-set-parameters "yt" #'make-youtube-link)
;; )
(use-package! balanced-windows
  :config
  (balanced-windows-mode))

(use-package prettier)

(if (locate-dominating-file default-directory ".prettierrc")
        (prettier-mode +1)
        (add-hook 'before-save-hook 'tide-format-before-save))

(setq
 projectile-project-search-path '("~/Projects/")
 org-log-done 'time
 org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
 org-fancy-priorities-list '("HIGH" "MID" "LOW")

 org-journal-date-format "%a, %Y-%m-%d"
 org-journal-file-format "%Y-%m-%d.org"
 org-journal-enable-agenda-integration t

 undo-limit 80000000 ; Raise undo-limit to 80Mb
 scroll-margin 2
 evil-vsplit-window-right t
 evil-split-window-below t

 auto-save-default t
)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(map! :map general-override-mode-map
      "C-l" #'evil-window-right
      "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up)

(map! :leader
      :desc "Move window to the right"
      "w C-l" #'+evil/window-move-right)

(map! :leader
      :desc "Move window to the left"
      "w C-h" #'+evil/window-move-left)

(map! :leader
      :desc "Move window down"
      "w C-j" #'+evil/window-move-down)

(map! :leader
      :desc "Move window up"
      "w C-k" #'+evil/window-move-up)

(map! :leader
      :desc "Move split up"
      "v v" #'evil-window-vsplit)

(map! :leader
      :desc "Vertical split"
      "v v" #'evil-window-vsplit)

(map! :leader
      :desc "Horizontal split"
      "s s" #'evil-window-split)

(map! :leader
      :desc "Close current window"
      "b q" #'kill-current-buffer)

(map! :leader
      :desc "Open correct word suggestions"
      "z z" #'flyspell-correct-word-before-point)

(map! :leader
      :desc "Save current buffer"
      "f w" #'save-buffer)

(map! :leader
      :desc "Open calendar"
      "o c" #'calendar)

(map! :leader
      :prefix-map ("n o" . "open documents")
      :desc "Open Brag document"
      "b" #'(lambda () (interactive) (find-file "~/org/brag.org")))

(map! :leader
      :prefix-map ("n o" . "open documents")
      :desc "Open todo document"
      "t" #'(lambda () (interactive) (find-file "~/org/todo.org")))

(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; (add-hook 'quit-window-hook 'balance-windows)
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(evil-window-split evil-window-vsplit)
;;   (consult-buffer))

(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(after! org
        (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org"
        "Inbox")
                "* TODO %? %U \n"
                :prepend t)
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
                "* %?"))))

(map!
      :map calendar-mode-map
      :n "e" #'org-journal-read-entry)
