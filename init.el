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

(defconst vk-init-file load-file-name
  "The name of this init file.

This value will be used to define a function for opening the init
file interactively.")
(defun vk-open-init-file ()
  "Open the file stored in variable `vk-init-file'."
  (interactive)
  (find-file vk-init-file)
  )

;; Set to true if we want to debug the init. Otherwise set to nil.
(setq debug-on-error nil)


(load-file "~/emacs/init-main.el")  ;; The bare minimum Emacs config I need.

;; Everything else is optional and should be commented out if the file is
;; missing or not needed.
;;(load-file "~/emacs/init-org-roam.el")  ;; Set up org-roam for atomic note-taking.
;;(load-file "~/emacs/init-utils.el") ;; Utility functions defined by me.
;; (load-file "~/emacs/init-lean.el") ;; Set up for the lean theorem prover.

;; It is hard to ever stop customizing Emacs. I put all my "experimental"
;; code in the following init. This file is not guaranteed to always be in
;; a stable form.

;; (load-file "~/emacs/init-experimental.el")




(message (format "Finished loading %s" (f-this-file)))
(provide 'init.el)
;;; init.el ends here
