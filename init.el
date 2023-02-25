;;; init.el --- init
;;; Commentary:
;;; TODO: lsp-ui setup, GUD setup, imenu, occur
;;; init.el, todos in ~/.emacs.d/emacs.org
;; -*- lexical-binding: t; -*-
;;; Code:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "CTDB" :slant normal :weight normal :height 108 :width normal)))))

;;(dap-mode vdiff nasm-mode hydra elfeed-goodies elfeed-dashboard elfeed-summary elfeed-autotag elfeed-tube elfeed-tube-mpv elfeed pdf-tools hl-todo arduino-cli-mode platformio-mode lsp-javacomp javadoc-lookup company-arduino arduino-mode orgit org-roam-ui org-contrib magit-todos magit-org-todos ivy-emms exwm eshell-vterm erc-hl-nicks erc-colorize emms-state emms-info-mediainfo eldoc-cmake cmake-project projectile)

;; package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(use-package straight
  :config
  (setq straight-use-package-by-default t))

(load "/usr/share/emacs/site-lisp/site-gentoo")
;; theme
(load-theme 'modus-vivendi nil)

;; other packages

;; global minor modes
(use-package which-key
  :config
  (which-key-setup-side-window-right)(which-key-mode))
(use-package flycheck
	:demand t
  :config
  (global-flycheck-mode))
(use-package flycheck-color-mode-line
	:demand t
	:requires (flycheck)
	:hook (flycheck-mode))
(use-package flycheck-rust
	:defer t
	:requires (flycheck)
	:hook (rust-mode . flycheck-rust-setup))
(electric-pair-mode)
(use-package beacon
  :config
  (beacon-mode))
(global-undo-tree-mode)
(use-package ivy
  :config
  (ivy-mode))
(use-package company
	:straight nil
	:demand t
	:hook (prog-mode)
  :custom ((company-minimum-prefix-length 1) (company-idle-delay 0.0)))
(use-package company-c-headers
	:requires (company)
	:config (add-to-list 'company-backends 'company-c-headers))
(use-package company-ebuild
	:straight nil
	:defer t
	:requires (company)
	:hook (ebuild-mode))
(global-auto-revert-mode)
(tool-bar-mode -1)
(use-package counsel
  :config
  (counsel-mode))
(use-package diff-hl
	:hook (prog-mode text-mode)
	:config (diff-hl-margin-mode))
(when (display-graphic-p)
  (use-package all-the-icons
		:demand t))
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(use-package color-identifiers-mode
  :defer t
  :config
  (global-color-identifiers-mode))
(use-package swiper
	:demand t
  :bind ("C-s" . swiper))
(use-package marginalia
	:demand t
	:config (marginalia-mode)
	:bind
	(:map minibuffer-local-map
				("M-A" . marginalia-cycle)))
(use-package embark
	:demand t
	:bind ("C-M-SPC" . embark-act))
(save-place-mode)
(use-package hideshow
	:straight nil
	:demand t
	:hook (prog-mode . hs-minor-mode))
(use-package vlf
	:defer t)
;; (use-package uncrustify-mode
;; 	:defer t
;; 	:hook (c-mode))
(use-package iedit
	:demand t)
(use-package amx
	:config (amx-mode))
(use-package consult
	:defer t)
(use-package projectile
	:demand t
	:bind (:map projectile-mode-map
							("C-c p" . projectile-command-map))
	:config (projectile-mode))
(use-package helpful
	     :demand t
	     :bind
			 (("C-h f" . helpful-callable)
		    ("C-h v" . helpful-variable)
		    ("C-h k" . helpful-key)
				("C-h F" . helpful-function)
				("C-h C" . helpful-command)
				("C-c C-d" . helpful-at-point)))

;; global functions
(defun save-format ()
	"Format buffer before save."
	(interactive)
	(add-hook 'before-save-hook 'lsp-format-buffer))

(defun sudo-save ()
	"Save current file as sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; minor hooks

;; global settings
(global-display-line-numbers-mode)
(menu-bar--display-line-numbers-mode-relative)
(setq-default shell-file-name "/bin/zsh")
(scroll-bar-mode -1)
(setq-default tab-width 2 indent-tabs-mode t)
(setq debug-on-error t)
(setq load-prefer-newer t)
(setq sentence-end-double-space t)
(setq-default compile-command "make -j16")
(setq make-backup-files nil select-enable-clipboard t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq warning-suppress-log-types '(comp))
(setq warning-suppress-types '(comp))
(defalias 'term 'vterm)

;; global keybindings
(defun turn-off-minibuffer-buffer ()
	"Turn off ivy-mode to get rid of the separate buffer for the minibuffer."
	(interactive)
	(ivy-mode -1)
	(counsel-mode -1))

(global-set-key (kbd "C-c r") 'eval-buffer)
(global-set-key (kbd "C-r") 'replace-string)

;; Major modes without extra config
(use-package zig-mode
	:after (lsp-mode))
(use-package cmake-mode
	:straight nil)
(use-package cmake-project
	:hook ((c-mode . (lambda () (when (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (project-root (project-current)))))
      (cmake-project-mode)))))
	:hook ((c++-mode . (lambda () (when (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (project-root (project-current)))))
      (cmake-project-mode))))))

;; Other
(use-package eww
	:straight nil
	:defer t)
(use-package dired
	:straight nil
	:defer t
	:hook (dired-mode . dired-omit-mode)
	:hook (dired-mode . (lambda () (image-dired-display-thumbs 1))))
(use-package vterm
  :defer t)
(use-package eshell-vterm
	:demand t
	:after eshell
	:config ((eshell-vterm-mode) (defalias 'eshell/v 'eshell-exec-visual)))

;; magit
(use-package magit
	:demand t
  :bind
  (("C-c g c" . magit-commit)
   ("C-c g s" . magit-status)
   ("C-c g b s" . magit-branch-checkout)
   ("C-c g p s" . magit-patch-save)))

;; LSP ; todo: fix
(use-package lsp-mode
	:defer t
	:after (which-key)
	:hook ((c-mode c++-mode zig-mode rust-mode) (lsp-mode . lsp-enable-which-key-integration))
	:bind (:map lsp-mode-map
							("M-n" . lsp-find-defintion)
							("M-m" . lsp-find-references))
	:config (setq lsp-modeline-diagnostics-enable t lsp-modeline-diagnostics-scope :workspace lsp-modeline-code-actions-enable t lsp-modeline-code-actions-segments '(icon count name) lsp-enable-on-type-formatting t))

(use-package lsp-ui
	:demand t
	:requires (lsp-mode)
	:config
	:bind (:map lsp-ui-mode-map
							([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
							([remap xref-find-references] . lsp-ui-peek-find-references)
							("C-c C-f" . lsp-ui-doc-focus-frame)
							("C-c C-q" . lsp-ui-doc-focus-frame)
							("C-c f b" . lsp-ui-peek-jump-backward)
							("C-c f f" . lsp-ui-peek-jump-forward))
	:custom
	((lsp-ui-sideline-delay 0.5) (lsp-ui-sideline-show-diagnostics t) (lsp-ui-sideline-show-code-actions nil) (lsp-ui-sideline-show-hover t) (lsp-ui-doc-enable t) (lsp-ui-doc-delay 0.5) (lsp-ui-doc-show-with-cursor t) (lsp-ui-doc-show-with-mouse t) (lsp-ui-doc-postion 'top) (lsp-ui-peek-enable t) (lsp-ui-peek-show-directory t) (lsp-ui-imenu-enable t) (lsp-ui-imenu-auto-refresh t)))
(use-package lsp-ivy
	:requires (lsp-mode))
(use-package consult-lsp
	:requires (lsp-mode consult))
;; Debug
(defun debug-gud ()
	"GUD setup for Rust and core files."
	(interactive)
	(cond ((and (or (equal major-mode 'c-mode) (equal major-mode 'c++-mode)) (file-exists-p "./core"))
				 ((setq gud-gdb-command-name "gdb -i=mi ./core") (gud-gdb)))
				((equal major-mode 'rust-mode) (rust-gdb))))
(use-package dap-mode
	:demand t)

(define-key c-mode-map (kbd "TAB") 'c-indent-line-or-region) ; TODO: fix

;; emms
;; (global-set-key (kbd "C-c m s") (lambda () (interactive) (emms-play-directory-tree "~/Music")))
;; (use-package emms
;;  :straight nil
;; 	:demand t
;; 	:config ((emms-all) (emms-default-players) (setq emms-repeat-playlist t emms-random-playlist t emms-source-file-default-directory "~/Music"))
;; 	:bind (:map emms-mode-map
;; 							("C-c m p" . emms-stop)
;; 							("C-c m r" . emms-start)))

;; ERC (IRC)
(use-package erc
  :defer t
  :config
  ((setq erc-auto-query 'window-noselect)
   (setq erc-nick "Pay08" erc-server "irc.libera.chat" erc-port 6667)
   (setq erc-modules '(button completion fill list match readonly ring scrolltobottom smiley stamp spelling unmorse hl-nicks netsplit fill track networks autojoin noncommands irccontrols move-to-prompt menu))
	 (erc-update-modules))
	:hook
	(erc-mode . (lambda () (undo-tree-mode -1) (flycheck-mode -1) (eldoc-mode -1) (counsel-mode -1) (ivy-mode -1) (company-mode -1)))
	:hook (erc-echo-notice-hook . erc-echo-notice-in-minibuffer))

;; mu4e
(load "/usr/share/emacs/site-lisp/mu4e/mu4e")

;; Rust config
(use-package rustic
	:after (lsp-mode)
	:defer t)

;; C/C++ config
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1)  ;; clangd is fast

(use-package disaster
	:bind (:map c-mode-map
							("C-c d" . disaster)
							:map c++-mode-map
							("C-c d" . disaster)))

(use-package c-eldoc
	:requires (eldoc)
	:defer t
	:hook ((c-mode c++-mode) . c-turn-on-eldoc-mode))

(use-package c-mode
	:straight nil
	:after (lsp-mode)
	:bind (:map c-mode-map
				 ("C-c o" . ff-find-other-file-other-window))
	:custom ((gc-cons-treshold (* 100 1024 1024)) (read-process-output-max (* 1024 1024)) (lsp-idle-delay 0.1)))
(use-package c++-mode
	:straight nil
	:bind (:map c++-mode-map
							("C-c o" . ff-find-other-file-other-window)))

;; irony ; TODO: fix server
(use-package irony
	:defer t
	:hook ((c-mode c++-mode arduino-mode) (irony-mode . irony-cdb-autosetup-compile-options)))
(use-package company-irony
	:requires (irony company)
	:config
	(add-to-list 'company-backends 'company-irony))
(use-package company-irony-c-headers
	:requires (company-irony)
	:config (add-to-list 'company-backends 'company-irony-c-headers))
(use-package flycheck-irony
	:requires (irony flycheck)
	:hook (flycheck-mode . flycheck-irony-setup))
(use-package irony-eldoc
	:requires (irony eldoc)
	:hook (irony-mode . irony-eldoc))

;; Shared Lisp config
(use-package paredit
	:defer t
	:hook
	(lisp-mode eval-expression-minibuffer-setup lisp-interaction-mode slime-repl-mode emacs-lisp-mode)
	:bind
	(:map paredit-mode-map
				("M-<right>" . paredit-forward-slurp-sexp)
				("M-<left>" . paredit-forward-barf-sexp)
				("C-<left>" . paredit-backward-slurp-sexp)
				("C-<right>" . paredit-backward-barf-sexp))
	:config ((global-unset-key (kbd "M-r"))
					 (global-set-key (kbd "M-r") 'move-to-window-line-top-bottom))) ; todo bind paredit-forward and paredit-backward and paredit-kill

(use-package rainbow-delimiters
	:defer t
  :hook (lisp-mode emacs-lisp-mode))

(use-package highlight-function-calls
	:hook (lisp-mode emacs-lisp-mode))

(use-package lisp-extra-font-lock
	:config (lisp-extra-font-lock-global-mode))

;; Common Lisp config
(use-package slime
	:defer t
	:init (slime-setup '(slime-fancy slime-asdf slime-banner slime-autodoc))
	:hook (lisp-mode)
	:config
	(setq inferior-lisp-program "/usr/bin/sbcl --noinform --no-linedit")
	:bind
	(:map slime-mode-map
				("C-c s C-q" . slime-close-parens-in-sexp)
				 ("C-c s C-c" . slime-compile-defun)
				 ("C-c s C-l" . slime-load-file)
				 ("C-c s C-k" . slime-compile-and-load-file)
				 ("C-c C-d h" . slime-hyperspec-lookup))
	:after (company)
	:defer t
	:config
	(setq slime-company-completion 'fuzzy slime-company-after-completion 'slime-company-just-one-space))

;; ASM config (NASM, gas)


;; Arduino
(use-package arduino-mode
	:defer t)
(use-package company-arduino
	:requires (arduino-mode company irony-mode company-irony company-c-headers)
	:hook (irony-mode . company-arduino-turn-on)
	:config (setq company-c-headers-path-system 'get-system-path-company-c-headers))

(defun get-system-path-company-c-headers ()
	"Return the system include path for the current buffer."
	(let ((default '("/usr/include/" "/usr/local/include/")))
		(company-arduino-append-include-dirs default t)))

;; ebuild
(use-package ebuild-mode)

;; eselect

;; Org
(use-package org
	:straight nil
	:defer t
	:config (add-to-list 'org-file-apps '(directory . emacs)))
(use-package org-auto-tangle
	:after (org)
  :defer t
  :hook (org-mode))
(use-package org-indent
	:straight nil
	:after (org)
	:defer t
	:hook (org-mode))
(use-package org-modern
	:after (org)
  :demand t
  :hook ((org-mode) (org-agenda-finalize . org-modern-agenda)))

(use-package org-babel
	:straight nil
  :after (org)
  :defer t
  :config
  ((org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)
   (arduino . t)
   (lisp . t)
   (makefile . t)
   (org . t)
   (calc . t)))
(setq org-src-preserve-indentation t org-src-fontify-natively t org-confirm-babel-evaluate nil)))

;; GUD


;; ligatures
(use-package ligature
	:demand t
	:config
	(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
																			 ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
																			 "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
																			 "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
																			 "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
																			 "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
																			 "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
																			 "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
																			 "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
																			 "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
	(global-ligature-mode t))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp) comp)))
