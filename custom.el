(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-compilation-mode t)
 '(copilot-indentation-alist
   '((latex-mode tex-indent-basic) (nxml-mode nxml-child-indent) (python-mode 4)
     (python-ts-mode 4)
     (web-mode web-mode-markup-indent-offset web-mode-html-offset)
     (apache-mode apache-indent-level) (awk-mode c-basic-offset)
     (bpftrace-mode c-basic-offset) (c++-mode c-basic-offset)
     (c++-ts-mode c-basic-offset c-ts-mode-indent-offset)
     (c-mode c-basic-offset) (c-ts-mode c-basic-offset c-ts-mode-indent-offset)
     (cmake-mode cmake-tab-width)
     (cmake-ts-mode cmake-tab-width cmake-ts-mode-indent-offset)
     (coffee-mode coffee-tab-width) (cperl-mode cperl-indent-level)
     (crystal-mode crystal-indent-level) (csharp-mode c-basic-offset)
     (csharp-ts-mode c-basic-offset csharp-ts-mode-indent-offset)
     (css-mode css-indent-offset) (css-ts-mode css-indent-offset)
     (d-mode c-basic-offset) (elixir-ts-mode elixir-ts-indent-offset)
     (emacs-lisp-mode lisp-indent-offset) (enh-ruby-mode enh-ruby-indent-level)
     (erlang-mode erlang-indent-level) (ess-mode ess-indent-offset)
     (f90-mode f90-associate-indent f90-continuation-indent f90-critical-indent
	       f90-do-indent f90-if-indent f90-program-indent f90-type-indent)
     (feature-mode feature-indent-offset feature-indent-level)
     (fsharp-mode fsharp-continuation-offset fsharp-indent-level
		  fsharp-indent-offset)
     (gdscript-mode gdscript-indent-offset) (groovy-mode groovy-indent-offset)
     (go-ts-mode go-ts-mode-indent-offset)
     (haskell-mode haskell-indent-spaces haskell-indent-offset
		   haskell-indentation-layout-offset
		   haskell-indentation-left-offset
		   haskell-indentation-starter-offset
		   haskell-indentation-where-post-offset
		   haskell-indentation-where-pre-offset shm-indent-spaces)
     (haxor-mode haxor-tab-width) (hcl-mode hcl-indent-level)
     (html-ts-mode html-ts-mode-indent-offset) (idl-mode c-basic-offset)
     (jade-mode jade-tab-width) (java-mode c-basic-offset)
     (java-ts-mode c-basic-offset java-ts-mode-indent-offset)
     (js-mode js-indent-level) (js-ts-mode js-indent-level)
     (js-jsx-mode js-indent-level sgml-basic-offset) (js2-mode js2-basic-offset)
     (js2-jsx-mode js2-basic-offset sgml-basic-offset)
     (js3-mode js3-indent-level) (json-mode js-indent-level)
     (json-ts-mode json-ts-mode-indent-offset)
     (jsonian-mode jsonian-default-indentation) (julia-mode julia-indent-offset)
     (kotlin-mode kotlin-tab-width)
     (kotlin-ts-mode kotlin-ts-mode-indent-offset)
     (latex-mode . editorconfig-set-indentation-latex-mode)
     (lisp-mode lisp-indent-offset) (livescript-mode livescript-tab-width)
     (lua-mode lua-indent-level) (lua-ts-mode lua-ts-indent-offset)
     (matlab-mode matlab-indent-level) (meson-mode meson-indent-basic)
     (mips-mode mips-tab-width) (mustache-mode mustache-basic-offset)
     (nasm-mode nasm-basic-offset) (nginx-mode nginx-indent-level)
     (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
     (objc-mode c-basic-offset) (octave-mode octave-block-offset)
     (perl-mode perl-indent-level) (php-mode c-basic-offset)
     (php-ts-mode php-ts-mode-indent-offset) (pike-mode c-basic-offset)
     (protobuf-mode c-basic-offset) (ps-mode ps-mode-tab)
     (pug-mode pug-tab-width) (puppet-mode puppet-indent-level)
     (python-mode . editorconfig-set-indentation-python-mode)
     (python-ts-mode . editorconfig-set-indentation-python-mode)
     (rjsx-mode js-indent-level sgml-basic-offset) (ruby-mode ruby-indent-level)
     (ruby-ts-mode ruby-indent-level) (rust-mode rust-indent-offset)
     (rust-ts-mode rust-indent-offset rust-ts-mode-indent-offset)
     (rustic-mode rustic-indent-offset) (scala-mode scala-indent:step)
     (scss-mode css-indent-offset) (sgml-mode sgml-basic-offset)
     (sh-mode sh-basic-offset sh-indentation)
     (swift-mode swift-mode:basic-offset)
     (bash-ts-mode sh-basic-offset sh-indentation)
     (slim-mode slim-indent-offset) (sml-mode sml-indent-level)
     (tcl-mode tcl-indent-level tcl-continued-indent-level)
     (terra-mode terra-indent-level) (toml-ts-mode toml-ts-mode-indent-offset)
     (typescript-mode typescript-indent-level)
     (typescript-ts-base-mode typescript-ts-mode-indent-offset)
     (verilog-mode verilog-indent-level verilog-indent-level-behavioral
		   verilog-indent-level-declaration verilog-indent-level-module
		   verilog-cexp-indent verilog-case-indent)
     (web-mode (web-mode-indent-style lambda (size) 2)
	       web-mode-attr-indent-offset web-mode-attr-value-indent-offset
	       web-mode-code-indent-offset web-mode-css-indent-offset
	       web-mode-markup-indent-offset web-mode-sql-indent-offset
	       web-mode-block-padding web-mode-script-padding
	       web-mode-style-padding)
     (yaml-mode yaml-indent-offset) (yaml-ts-mode yaml-indent-offset)))
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef"
     "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948"
     "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138"
     "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b"
     "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea"
     "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(golden-ratio-exclude-buffer-names '(""))
 '(golden-ratio-exclude-modes '(neotree-mode))
 '(helm 'emacs t)
 '(jq-indent-offset 0)
 '(lsp-pylsp-plugins-pylint-enabled nil)
 '(lsp-pyright-venv-directory "\"~/miniconda3/envs/\"")
 '(mailcap-user-mime-data '(("open" "*" nil)))
 '(markdown-disable-tooltip-prompt t)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(2048-game agent-shell agent-shell-attention aidermacs all-the-icons autothemer
	       beacon blacken company compat copilot copilot-chat counsel
	       counsel-projectile csv-mode delight dictionary diffview dired
	       docker dockerfile-mode doom-themes eca editorconfig eglot ein
	       elpy emojify envrc epresent exec-path-from-shell fira-code-mode
	       flycheck flycheck-pycheckers flymake-mypy flymake-ruff forge
	       general git-timemachine golden-ratio haskell-emacs haskell-mode
	       hasklig-mode helm helm-command helm: hide-mode-line iedit
	       isortify ivy jq-mode just-mode justl ligature lsp-diagnostics
	       lsp-haskell lsp-pyright lsp-python-ms lsp-ui magit magit-delta
	       markdownfmt monokai monokai-theme neotree nix-mode numpydoc
	       ob-powershell olivetti org org-journal org-roam org-super-agenda
	       org-tree-slide origami pet pipenv poetry presentation projectile
	       projectile-ripgrep pylint python python-isort quelpa
	       quelpa-use-package realgud replace ripgrep ruff-format
	       shell-maker smartparens string-inflection swagg swiper traad
	       tramp tree-sitter tree-sitter-langs treesit treesit-auto
	       use-package vdiff vterm which-key yaml yaml-mode yasnippet
	       yasnippet-snippets))
 '(python-flymake-command '("pyright"))
 '(python-shell-virtualenv-root ".venv/")
 '(pyvenv-tracking-mode t)
 '(safe-local-variable-values
   '((lisp-indent-local-overrides (cond . 0) (interactive . 0))
     (just-indent-offset . 4)))
 '(venv-location "/Users/vaibhav/.venvs/")
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
