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

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
(setq doom-symbol-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))

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

(let ((path-from-shell (replace-regexp-in-string
                        "[ \t\n]*$" ""
                        (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
  (setenv "PATH" path-from-shell)
  (setq exec-path (split-string path-from-shell path-separator)))

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
      :prefix-map ("n n" . "open documents")
      :desc "Open Brag document"
      "b" #'(lambda () (interactive) (find-file "~/org/brag.org")))

(map! :leader
      :prefix-map ("n n" . "open documents")
      :desc "Open todo document"
      "t" #'(lambda () (interactive) (find-file "~/org/todo.org")))

(map! :leader
      :prefix-map ("n n" . "open documents")
      :desc "Open notebook"
      "n" #'(lambda () (interactive) (find-file "~/org/notebook.org")))

(map! :leader
      :prefix-map ("n n" . "open documents")
      :desc "Open requests"
      "r" #'(lambda () (interactive) (find-file "~/org/requests.org")))

(map! :leader
      :desc "Open daily journal"
      "n d" #'org-roam-dailies-goto-date)

(map! :leader
      :desc "Search with avy with timer"
      "f a" #'evil-avy-goto-char-timer)

(map! :leader
      :desc "Search with vertico/project-search"
      "/" #'+vertico/project-search)

(map! :leader
      :desc "Expand diff show hunk"
      "g h" #'diff-hl-show-hunk)

(map! "s-<f12>" #'prot-window-popup-org-capture)

(after! dape
  (setq dape-setup-defaults nil))  ;; if dape is being loaded

(map! :leader "d" nil) ; unbind top-level dape prefix

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "d" #'dap-debug
      :desc "dap repl"          "p" #'dap-ui-repl
      :desc "dap debug last"    "l" #'dap-debug-last
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap disconnect"    "q" #'dap-disconnect
      :desc "dap debug test"    "s" #'my/dap-python-test-at-point

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

(defun my/run-make-command (target)
  ;; Run 'make <target>' in the project root.
  (let ((default-directory (projectile-project-root)))
    (async-shell-command (format "make %s" target))))

(map! :leader
      :desc "Run bump-patch" "m b p" #'(lambda () (interactive) (my/run-make-command "bump-patch"))
      :desc "Run bump-minor" "m b m" #'(lambda () (interactive) (my/run-make-command "bump-minor"))
      :desc "Run bump-major" "m b M" #'(lambda () (interactive) (my/run-make-command "bump-major")))

(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; (add-hook 'quit-window-hook 'balance-windows)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (projectile-find-file))

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
          ("i" "Ideas" entry (file+headline "~/org/ideas.org"
                                            "Inbox")
           "* %?")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?"))))

(map!
 :map calendar-mode-map
 :n "e" #'org-journal-read-entry)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(advice-add 'evil-ex-search-next :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

(setq neo-window-width 55)

(pixel-scroll-precision-mode t)

;; Customize avy to search all windows
(setq avy-all-windows t)

;; Set dates in calendar to european
(setq calendar-date-style 'european)

(defun my/org-roam-dailies-find-date-format (date)
  "Format DATE for use with org-roam-dailies-find-date."
  (format-time-string "%d-%m-%Y" date)) ; Customize the date format here

                                        ;Popup window functionality by Protesilaos also known as Prot
(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))

(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))

(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)

;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)

(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)

(declare-function tmr "tmr" (time &optional description acknowledgep))
(defvar tmr-timer-created-functions)

;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
(prot-window-define-with-popup-frame tmr)

(add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame)

;;;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)

(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;;; The emacsclient calls that need ot be bound to system-wide keys

;; emacsclient -e '(prot-window-popup-org-capture)'
;; emacsclient -e '(progn (x-focus-frame nil) (prot-window-popup-org-capture))'
;; emacsclient -e '(prot-window-popup-tmr)'

(after! org-agenda
  ;; Add a hook to save all Org buffers after changing a TODO state
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers))

;; Run unittests
(defun run-python-buffer-unittest ()
  "Run `python -m unittest` on the current buffer or project."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (test-command (if buffer-file-name
                           (format "python -m unittest %s"
                                   (string-replace "/" "."
                                                   (file-name-sans-extension
                                                    (file-relative-name buffer-file-name default-directory))))
                         "python -m unittest discover")))
    (compile test-command)))

(defun run-python-unittest ()
  "Run `python -m unittest` on the current buffer or project."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (test-command (if buffer-file-name (format "python -m unittest discover -t . -s tests -p '*_tests.py' -vc")
                         "python -m unittest discover")))
    (compile test-command)))

;; Keybinding only active in python-mode
(after! python
  (map! :map python-mode-map
        :localleader
        :desc "Run Python buffer Unittests" "t v" #'run-python-buffer-unittest
        :desc "Run Python Unittests" "t A" #'run-python-unittest))

(after! (:and lsp-mode ruby-mode)
  (add-to-list 'lsp-language-id-configuration '(ruby-mode . "ruby"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruby-lsp"))
    :major-modes '(ruby-mode)
    :server-id 'ruby-lsp)))

;; Mocha configuration
(use-package! mocha
  :after rjsx-mode
  :config
  (map! :localleader
        :map rjsx-mode-map
        "t a" #'mocha-test-project
        "t f" #'mocha-test-file
        "t s" #'mocha-test-at-point
        "t d" #'mocha-debug
        :desc "Activate node version" "a" #'nvm-use))

(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

(after! (:and dap-mode python)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)

  (defun my/dap-python-test-at-point ()
    "Run unittest for the test under the cursor."
    (interactive)
    (let ((test-method (replace-regexp-in-string "\\." "::"(python-info-current-defun))))
      (if test-method
          (dap-debug
           (list :type "python"
                 :args (format "--color=yes %s::%s" (buffer-file-name) test-method)
                 :cwd nil
                 :module "pytest"
                 :request "launch"
                 :debugger 'debugpy
                 :name (format "Python :: Test %s" test-method)))
        (message "No test method found at point!")))))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(defhydra my-window-resize (:hint nil)
  "
  Resize Window
  _-_ shrink height   _+_ increase height
  _<_ shrink width    _>_ increase width
  _q_ quit
  "
  ("-" evil-window-decrease-height)
  ("+" evil-window-increase-height)
  ("<" evil-window-decrease-width)
  (">" evil-window-increase-width)
  ("q" nil "quit" :exit t))

(map! :leader "w r" #'my-window-resize/body)
