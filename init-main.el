;;; init-main.el --- My main init file.

;; Author: Vaibhav Karve

;;; Commentary:
;;  ===========

;; I want to keep this file as minimal as possible. It is important
;; that this file remain self-contained.

;; Refer to the old lit file for copying code.
;; ~/projects/old_emacs_init/lit-init.org

;;; Code:
;;  =====

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "http://melpa.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("org-elpa" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Quelpa installation
;; ===================
;; The following command boostraps quelpa.
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
;; Next, we install quelpa-use-package, which installs use-package as a dependency.
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; Independent install of use-package (TODO: Can this section be removed?)
;; ==================================
(eval-and-compile
  (setq
   ;; Makes sure to download new packages if they aren't already downloaded
   use-package-always-ensure t
    ;; Package install logging. Packages break, it's nice to know why.
   use-package-verbose t
   use-package-expand-minimally t
   package-check-signature nil))
(eval-when-compile (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;; Make Emacs aware of the system we are on
;;  ========================================


;; `f.el' is a modern API for working with files and directories in
;; Emacs.
(use-package f)
(defconst vk-home "/Users/vaibhav" "Home directory.")
(defconst vk-emacs-dir (f-join vk-home ".emacs.d"))
(defconst vk-org-dir (f-join vk-home "org"))
(defconst vk-gtd (f-join vk-org-dir "gtd.org"))
(defconst vk-journal-dir (f-join vk-org-dir "journal"))
(defconst vk-roam-dir (f-join vk-org-dir "roam"))


;;; Org-mode settings
;;  =================
;; TODO: set up org-indent mode.

;; `org-tempo' helps with insertion of org structure templates, i.e.
;; code blocks inside org-mode. These templates are stored in
;; `org-structure-template-alist'.
(require 'org-tempo)
;; To enable shift-selection of text in org-mode, we enable the following.
(setq org-support-shift-select t)
;; Enable `org-indent-mode' for hiding "***" in deep org headlines.
(add-hook 'org-mode-hook 'org-indent-mode)
;; Default list of org todo keywords.
(setq org-todo-keywords
      '((sequence "NEXT(n)" "PROG(p)" "ZERO(z)" "REVIEW(r)" "WAITING(w)" "|" "DONE(d)")))


(org-babel-do-load-languages
 'org-babel-load-languages
 (quote (
         ;; ...
         (powershell . t))))

(setq package-selected-packages
      '(doom-themes
	monokai-theme
	delight
	beacon
	golden-ratio
	neotree))
(package-install-selected-packages)


;;; Theme and minimal UI
;;  ====================
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
(use-package monokai-theme)

(use-package delight
  :config
  (delight '((abbrev-mode nil)
	     (eldoc-mode nil "eldoc")
	     (emacs-lisp-mode "Elisp" :major)
	     (auto-revert-mode " ar" "autorevert"))))




;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el") ;; Without this emacs will dump generated custom settings in this file. No bueno.
(load custom-file 'noerror)

;;; OS specific config
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Emacs feels like it's developed with linux in mind, here are some mac UX improvments
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX"))

;; Some linux love, too
(when *is-a-linux*
  (setq x-super-keysym 'meta))


(use-package s)  ;; A string-manipulation library.
(defun vk-show-path-environment-variable ()
  "Return the value of $PATH environment variable."
  (interactive)
  (let ((path-var (exec-path-from-shell-getenv "PATH")))
    (print (s-split ":" path-var))))


;; Fullscreen by default, as early as possible. This tiny window is not enough
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(display-time-mode 1)
(display-battery-mode 1)
(global-hl-line-mode 1)
;; Disable global linum but enable line numbers. The latter is faster
;; than the former.
(global-display-line-numbers-mode 1)
;; When switching to a new buffer, use beacon-mode to light up the
;; curson line. This way we know where the position is immediately.
;; This is a global minor-mode.
(use-package beacon
  :delight
  :config
  (beacon-mode 1))

;; Golden-ratio allows for dynamic resizing of window sizes.
(use-package golden-ratio
  :delight " g"
  :config
  (golden-ratio-mode 1))

(use-package neotree
  :custom
  (neo-theme 'classic)
  (neo-smart-open t)
  (neo-window-fixed-size nil))
(use-package all-the-icons)




;;;========================================
;;; Python
;;;========================================

;; `python.el' provides `python-mode' which is the builtin major-mode for the
;; Python language.

(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))


;; Hide the modeline for inferior python processes.  This is not a necessary
;; package but it's helpful to make better use of the screen real-estate at our
;; disposal. See: https://github.com/hlissner/emacs-hide-mode-line.

(use-package hide-mode-line
  :ensure t
  :defer t
  :hook (inferior-python-mode . hide-mode-line-mode))


(setq compile-command "just ")

;;<OPTIONAL> I use poetry (https://python-poetry.org/) to manage my python environments.
;; See: https://github.com/galaunay/poetry.el.
;; There are alternatives like https://github.com/jorgenschaefer/pyvenv.
;; (use-package poetry
;;   :ensure t
;;   :defer t
;;   :config
;;   ;; Checks for the correct virtualenv. Better strategy IMO because the default
;;   ;; one is quite slow.
;;   (setq poetry-tracking-strategy 'switch-buffer)
;;   :hook (python-mode . poetry-tracking-mode))

(setenv "WORKON_HOME" "~/.venvs/")

;; (use-package pipenv
;;   :hook (python-ts-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))




;; <OPTIONAL> Numpy style docstring for Python.  See:
;; https://github.com/douglasdavis/numpydoc.el.  There are other packages
;; available for docstrings, see: https://github.com/naiquevin/sphinx-doc.el
(use-package numpydoc
  :ensure t
  :defer t
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))



;;; Inline static analysis

;; Enabled inline static analysis
;; (add-hook 'prog-mode-hook #'flymake-mode)




;;; Completion
;;  ==========

;; This creates a dropdown menu with completion options.
(use-package company
  :delight ""
  :custom
  (company-idle-delay 0 "Show dropdown immediately.")
  (company-dabbrev-downcase nil "Don't downcase everything that is completed.")
  (company-dabbrev-ignore-case nil "Ignore case when collecting completion candidates.")
  (company-minimum-prefix-length 1 "Single character will trigger the dropdown.")
  (company-selection-wrap-around t "Dropdown menu wrap around top and bottom.")
  (company-show-quick-access nil nil nil "I don't need the numbers.")
  (company-tooltip-limit 10 "Show me as many options as possible at a time.")
  (company-format-margin-function nil "Disable margin icon in dropdown.")
  (company-tooltip-width-grow-only t "Tooltip width monotonically increases.")
  :config
  (global-company-mode 1))


;; Helm is Emac's incremental completion and selection narrowing
;; framework. helm-M-x gives us a popup of options for selecting
;; functions via M-x.
(use-package helm
  :delight
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (helm-mode 1)
  (setq
   ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-inside-p t
   ;; move to end or beginning of source when reaching top or bottom of source.
   helm-move-to-line-cycle-in-source t
   helm-echo-input-in-header-line t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-locate-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-candidate-number-limit 150)
  (customize-set-variable
   helm-completion-style 'emacs
   "Keep it at value 'emacs' and use Emacs's 'completion-styles instead."))
(setq completion-styles '(flex))



(use-package which-key
  :delight
  :config
  (setq which-key-separator " "
        which-key-prefix-prefix "+")
  (which-key-mode t))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

;; We first load support for expanding, editing, and adding new code snippets.
(use-package yasnippet
  :config
  (yas-global-mode 1))
;; Then we load the standard collection of snippets.
(use-package yasnippet-snippets
  :after yasnippet)

;; To get yasnippet to work with new tree-sitter modes, we add some advice.
(advice-add 'yas--modes-to-activate :around
 (defun yas--get-snippet-tables@tree-sitter (orig-fn &optional mode)
   (funcall orig-fn
            (or (car (rassq (or mode major-mode) major-mode-remap-alist))
                mode))))


;;; Project management and Version control
;;  ======================================

;; Use dired for all file-level operations. `dired-dwim-target' is
;; useful for copying files from one folder to another. Go to dired,
;; split your window, split-window-vertically & go to another dired
;; directory. When you will press C to copy, the other dir in the split
;; pane will be the default destination.
(require 'dired)
(use-package dired
  :disabled  ;; This package is not loading correctly.
  :custom
  (dired-dwim-target t "Useful for copying files from one folder to another."))


;; We need something to manage the various projects we work on
;; and for common functionality like project-wide searching, fuzzy file finding etc.
(use-package projectile
  :delight " proj"
  :config
  (projectile-mode t) ;; Enable this immediately
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-completion-system 'helm "Ideally the minibuffer should aways look similar")
  (projectile-require-project-root nil))

;; Counsel and projectile should work together.
(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-refresh-status-buffer nil))  ;; only automatically refresh the current
				      ;; Magit buffer, but not the status
				      ;; buffer. The status buffer is only
				      ;; refreshed automatically if it is the
				      ;; current buffer.
(use-package forge :after magit)
(setq auth-sources '("~/.authinfo"))
(setq vc-handled-backends '(Git))
(setq ghub-use-workaround-for-emacs-bug 'force)

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;;; Text Editing
;;  ============


;; Enable ansi color codes in compilation buffers.
(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))


(use-package iedit
  :config
  (setq-default iedit-is-narrowed nil))
(setq-default fill-column 80)
(setq sentence-end-double-space nil)
(setq show-paren-delay 0)
(show-paren-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; In order to delete text when we select a region and hit backspace/delete.
(delete-selection-mode t)


;; I rarely open PDFs in Emacs. But when I do, I want them to be in
;; continuous viewing mode.
(setq doc-view-continuous t)


;; Increase default font size.
(set-face-attribute 'default nil :height 150)


;; Justfiles use just-mode.
(use-package just-mode)
(use-package justl)

(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)


;;; Ligatures
;;; =========
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia and Fira Code ligatures in programming modes
  (ligature-set-ligatures 'text-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;; Remap old major modes to new tree-sitter modes.
(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))


(use-package envrc
  :config (envrc-global-mode))

;; To remove the lag from eglot.
(setq eglot-events-buffer-size 0)


(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))
(add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode)

(use-package isortify
  :ensure t
  :defer t
  :hook (python-ts-mode . isortify-mode)
  :hook (python-ts-mode . python-isort-on-save-mode))


;; GitHub Copilot (using quelpa)
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :custom
  (copilot-indent-offset-warning-disable t))
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Manuall specify the LSP server that Eglot is supposed to use.
(add-to-list 'eglot-server-programs
	     '(python-ts-mode . ("pyright")))

;;; TODO
;;  ====

;; undo-tree
;; neotree
;; all-the-icons
;; which-key
;; org-journal
;; org-capture
;; org-roam
;; pytest
;; unicode-fonts
;; lean-mode
;; monokai-theme
;; smartparens
;; use-package (can use :custom and :bind to cleanup stuff).

(setq inhibit-startup-screen t
      initial-buffer-choice (f-join vk-home "projects" "scoringengine" "justfile")
      scroll-step 1
      display-time-default-load-average nil)

;; To get pyright error messages to show up as file-links in compilation mode,
;; we need to add the following regex. Snippet taken from https://robbmann.io/emacsd/

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))



(message (format "Finished loading %s" (f-this-file)))
(provide 'init-main.el)
;;; init-main.el ends here
