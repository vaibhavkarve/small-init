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
(package-refresh-contents)
(package-install 'use-package)
(require 'use-package)
(setq package-check-signature nil)
(setq
 ;; Makes sure to download new packages if they aren't already downloaded
 use-package-always-ensure t
 ;; Package install logging. Packages break, it's nice to know why.
 use-package-verbose t)

;; Slurp environment variables from the shell.  a.k.a. The Most Asked
;; Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


;;; Theme and minimal UI
;;  ====================
(use-package doom-themes
  :init
  (load-theme 'doom-one))


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

;; Fullscreen by default, as early as possible. This tiny window is not enough
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;; Completion
;;  ==========

;; Make M-x and other mini-buffers sortable, filterable
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t))

(use-package counsel
  :after ivy
  :init
  (counsel-mode 1)
  :bind (:map ivy-minibuffer-map))

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper))

;;; Projectile
;;  ==========

;; We need something to manage the various projects we work on
;; and for common functionality like project-wide searching, fuzzy file finding etc.
(use-package projectile
  :init
  (projectile-mode t) ;; Enable this immediately
  :config
  (setq projectile-enable-caching t ;; Much better performance on large projects
        projectile-completion-system 'ivy)) ;; Ideally the minibuffer should aways look similar

;; Counsel and projectile should work together.
(use-package counsel-projectile
  :init
  (counsel-projectile-mode))


;;; Company
;;  =======

;; Company is the best Emacs completion system.
(use-package company
  :bind (("C-." . company-complete))
  :custom
  (company-idle-delay 0) ;; I always want completion, give it to me asap
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 10 "The more the merrier.")
  :config
  (global-company-mode) ;; We want completion everywhere

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Package for interacting with language servers
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil ;; Flymake is outdated
        lsp-headerline-breadcrumb-mode nil)) ;; I don't like the symbols on the header a-la-vscode, remove this if you like them.


(use-package magit)

(use-package delight
  :config
  (delight '((abbrev-mode nil)
	     (eldoc-mode nil)
	     (emacs-lisp-mode "Elisp" :major)
	     (auto-revert-mode "arev"))))


(message (format "Finished loading %s" (f-this-file)))
(provide 'init-main.el)
;;; init-main.el ends here
