;;; init.el --- Vaibhav's init.el file

;; Author: Vaibhav Karve

;;; Commentary:
;;  ===========

;; This is the file Emacs loads at startup. This file will only contains
;; imports from other files.

;; I started using Emacs some time in 2019. I started this particular file
;; on 17 November 2021 after declaring Emacs bankrupcy.

;;; Code:
;;  =====

(defconst vk-init-file "~/.emacs.d/init-main.el"
  "The name of this init file, can be opened using `vk-open-init-file'.")
(defun vk-open-init-file ()
  "Open the file stored in variable `vk-init-file'."
  (interactive)
  (find-file vk-init-file))

;; Set to true if we want to debug the init. Otherwise set to nil.
(setq debug-on-error nil)


(load-file "~/.emacs.d/init-main.el")  ;; The bare minimum Emacs config I need.

;; Everything else is optional and should be commented out if the file is
;; missing or not needed.
;;(load-file "~/.emacs.d/init-org-roam.el")  ;; Set up org-roam for atomic note-taking.
(load-file "~/.emacs.d/init-utils.el") ;; Utility functions defined by me.
;; (load-file "~/.emacs.d/init-lean.el") ;; Set up for the lean theorem prover.

;; It is hard to ever stop customizing Emacs. I put all my "experimental"
;; code in the following init. This file is not guaranteed to always be in
;; a stable form.

;; (load-file "~/.emacs.d/init-experimental.el")
(load-file "~/.emacs.d/imenu-list.el")



(message (format "Finished loading %s" (f-this-file)))
(provide 'init.el)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/Users/vaibhav/org/blufor.org" "/Users/vaibhav/org/gtd.org" "/Users/vaibhav/org/notes.org" "/Users/vaibhav/org/object_oriented_principles.org" "/Users/vaibhav/org/purplefor.org" "/Users/vaibhav/org/purplefor_todos.org" "/Users/vaibhav/org/ref.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
