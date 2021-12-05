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

;; Bootstrap use-package.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq
   ;; Makes sure to download new packages if they aren't already downloaded
   use-package-always-ensure t
    ;; Package install logging. Packages break, it's nice to know why.
   use-package-verbose t
   use-package-expand-minimally t
   package-check-signature nil))
 
;; Slurp environment variables from the shell.  a.k.a. The Most Asked
;; Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; Make Emacs aware of the system we are on
;;  ========================================

;; `f.el' is a modern API for working with files and directories in
;; Emacs.
(use-package f)
(defconst vk-home "/home/vaibhav")
(defconst vk-emacs-dir (f-join vk-home "/" ".emacs.d"))
(defconst vk-org-dir (f-join vk-home "/" "org"))
(defconst vk-gtd (f-join vk-org-dir "/" "gtd.org"))


;;; Theme and minimal UI
;;  ====================
(use-package doom-themes
  :init
  (load-theme 'doom-one t))
(use-package monokai-theme)

(use-package delight
  :config
  (delight '((abbrev-mode nil)
	     (eldoc-mode nil "eldoc")
	     (emacs-lisp-mode "Elisp" :major)
	     (auto-revert-mode " arev" "autorevert"))))




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

(defun vk-show-path-environment-variable ()
  "Return the value of $PATH environment variable."
  (interactive)
  (let ((path-var (exec-path-from-shell-getenv "PATH")))
    (string-replace ":" "\n" path-var)))


;; Fullscreen by default, as early as possible. This tiny window is not enough
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;; Completion
;;  ==========

;; This creates a dropdown menu with completion options.
(use-package company
  :delight ""
  :custom
  (company-idle-delay 0 "Show dropdown immediately.")
  (company-dabbrev-downcase nil "Don't downcase everything that is completed.")
  (company-minimum-prefix-length 1 "Single character will trigger the dropdown.")
  (company-selection-wrap-around t "Dropdown menu wrap around top and bottom.")
  (company-show-numbers nil "I don't need the numbers.")
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



;;; Project management and Version control
;;  ======================================

;; We need something to manage the various projects we work on
;; and for common functionality like project-wide searching, fuzzy file finding etc.
(use-package projectile
  :delight " proj"
  :config
  (projectile-mode t) ;; Enable this immediately
  (setq projectile-enable-caching t ;; Much better performance on large projects
        projectile-completion-system 'helm ;; Ideally the minibuffer should aways look similar
        projectile-require-project-root nil)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Counsel and projectile should work together.
(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

(use-package magit)


;;; Code completion
;;  ===============


;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :delight " flyc"
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Support for Language Server Protocol. This is what will give us
;; IDE-like functionality for programming in any language with a
;; supported lsp-mode. This mode comes packaged with flymake (for
;; compilation, though this is obsolete) and `completion-at-point'
;; (which will serve as a backed for `company'-driven completion).
(use-package lsp-mode
  :commands lsp
  :custom
  (gc-cons-threshold 100000000) ;; Setting it to 100MB
  (read-process-output-max (* 1024 1024))) ;; Setting it to 1MB

;; This package contains all the higher level UI modules of lsp-mode,
;; like flycheck support and code lenses.
(use-package lsp-ui)
;; Add support for python-lsp.
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ;; or lsp-deferred


;;; Text Editing
;;  ============
(use-package iedit)
(setq fill-column 75)
(setq sentence-end-double-space nil)
(setq show-paren-delay 0)
(show-paren-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; In order to delete text when we select a region and hit backspace/delete.
(delete-selection-mode t)



;; I rarely open PDFs in Emacs. But when I do, I want them to be in
;; continuous viewing mode.
(setq doc-view-continuous t)

(message (format "Finished loading %s" (f-this-file)))
(provide 'init-main.el)
;;; init-main.el ends here
