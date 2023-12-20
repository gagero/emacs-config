;;; init.el --- init
;;; Commentary:
;;; TODO: ASM setup
;;; init.el, todos in ~/.emacs.d/emacs.org
;; -*- lexical-binding: t; -*-
;;; Code:

;;(erc-hl-nicks erc-colorize sly-stepper)

;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

(use-package haki-theme
	:custom (haki-region "#2e8b6d")
	:config (load-theme 'haki t))
(add-to-list 'load-path "~/.emacs.d/info+")

;; global minor modes
(use-package corfu
	:after (emacs)
	:custom ((corfu-auto t) (corfu-cycle t) (corfu-preselect 'prompt) (corfu-quit-no-match t))
	:hook (prog-mode . corfu-mode)
	:bind (:map corfu-map
							("TAB" . corfu-next)
							([tab] . corfu-next)
							([backtab] . corfu-previous)
							("S-TAB" . corfu-previous)
							("M-SPC" . corfu-separator)))
(use-package kind-icon
	:after (corfu)
	:custom (kind-icon-default-face 'corfu-default)
	:config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package orderless
	:init (setq completion-styles '(orderless basic partial-completion emacs22  initials substring) completion-category-defaults nil completion-category-overrides '((file (styles . (partial-completion))))))
(use-package cape
	:config
	(setq completion-at-point-functions (list (cape-capf-super #'cape-keyword #'cape-file #'cape-dabbrev))))
(use-package which-key
  :config
  (which-key-setup-side-window-right)
	(which-key-mode))
(use-package beacon
  :config
  (beacon-mode))
(use-package undo-tree
  :config (global-undo-tree-mode))
(use-package flycheck
	:config (global-flycheck-mode)
	(flycheck-define-checker zig
		"A zig syntax checker using zig's `ast-check` command."
		:command ("zig" "ast-check" (eval (buffer-file-name)))
		:error-patterns
		((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
		:modes zig-mode
		(flycheck-add-next-checker 'c/c++-clang 'c/c++-cppcheck))
  (add-to-list 'flycheck-checkers 'zig)
  :bind (:map flycheck-mode-map)
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error))
(use-package info+
	:ensure nil)
(use-package vertico
	:config (vertico-mode))
(use-package diff-hl
	:hook (prog-mode text-mode)
	:config (diff-hl-margin-mode))
(use-package marginalia
  :config (marginalia-mode)
  :bind
  (:map minibuffer-local-map
				("M-A" . marginalia-cycle)))
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))
(use-package iedit)
(use-package embark-consult)
(use-package consult
	:after (orderless)
	:custom (consult-line-start-from-top t)
	:bind ("C-s" . consult-line) ("M-i" . consult-imenu) ("C-x b" . consult-buffer) ("C-x p b" . consult-project-buffer))
(use-package embark
  :bind
	("C-M-SPC" . embark-act)
	("C-." . embark-dwim)
	("M-/" . embark-export))
(use-package eat
	:hook (eshell-load . eat-eshell-mode))
(use-package xclip
  :config (xclip-mode))
(use-package treesit-auto
  :config (global-treesit-auto-mode)
	(setq treesit-auto-langs '(c cpp cmake make commonlisp json markdown)))
(use-package hl-todo
	:config (global-hl-todo-mode))
(use-package yasnippet
  :config (yas-global-mode)
  :bind
  ("C-M-n" . yas-next-field)
  ("C-M-p" . yas-prev-field))
(use-package yasnippet-snippets)
(use-package magit-todos
  :after (magit)
  :config (magit-todos-mode))
(with-eval-after-load 'dired '(require dired-x))

;; global functions

(defun end-newline ()
	"Insert newline below point."
	(interactive)
	(move-end-of-line nil)
	(newline-and-indent))

(defun begin-newline ()
	"Insert newline above point."
	(interactive)
	(move-beginning-of-line nil)
	(newline-and-indent))

(defun indent-buffer ()
	"Indent the entire buffer."
	(interactive)
	(indent-region 0 (buffer-size)))

(defalias 'kb-copy-file
   (kmacro "M-< C-SPC M-> M-w"))

;; Emacs

(use-package emacs
	:ensure nil
	:after (consult compile)
	:config
	(scroll-bar-mode -1)
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(global-auto-revert-mode)
	(save-place-mode)
	(menu-bar--display-line-numbers-mode-relative)
	(electric-pair-mode)
	(recentf-mode)
	(global-auto-revert-mode)
	(undelete-frame-mode)
	(show-paren-mode)
	(setq-default shell-file-name "bash" tab-width 2 indent-tabs-mode t compile-command "make -j16" treesit-font-lock-level 4)
	(setopt use-short-answers t)
	(org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (lisp . t)
     (makefile . t)
     (org . t)
     (calc . t)
		 (C . t)
		 (eshell . t)
		 (scheme . t)))
  (setq org-src-preserve-indentation t org-src-fontify-natively t org-confirm-babel-evaluate nil)
	:hook (xref-after-update-hook . outline-minor-mode)
	(prog-mode . display-line-numbers-mode)
	(proced-post-display-hook . proced-toggle-auto-update)
	:bind (("C-r" . replace-string) ("C-c C-c" . compile) ("C-j" . end-newline) ("C-x p C-f" . project-find-file)))
(setq  debug-on-error t load-prefer-newer t sentence-end-double-space t make-backup-files nil select-enable-clipboard t next-line-add-newlines t show-paren-context-when-offscreen t compilation-auto-jump-to-first-error 'first-known completion-cycle-threshold 3 tab-always-indent 'complete gc-cons-threshold (* 100 1024 1024) read-process-output-max (* 1024 1024) browse-url-browser-function #'eww-browse-url)

;; Major modes without extra config
(use-package zig-mode
	:init (defvar zig-pairs '((?| . ?|)))
	(defun zig-add-electric-pairs ()
		(setq-local electric-pair-pairs (append electric-pair-pairs zig-pairs)))
	:hook (zig-mode . zig-add-electric-pairs)
	:config (setq-local compile-command "zig build-exe"))

;; LSP
(use-package eglot
	;; TODO: --compile-commands-dir per project based on dir-locals
	:ensure nil
	:init (with-eval-after-load 'eglot
					(add-to-list 'eglot-server-programs
											 `(c++-ts-mode . ("clangd" "--clang-tidy" "--compile-commands-dir=/tmp"))) ;; temp
					(add-to-list 'eglot-server-programs
											 `(c++-mode . ("clangd" "--clang-tidy" "--compile-commands-dir=/tmp"))) ;; temp
)
	:requires (orderless cape)
	
	:config (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) (setq completion-category-overrides '((eglot (styles orderless))))
	(add-hook 'eglot-managed-mode-hook #'(lambda () (setq-local completion-at-point-functions
																															(list (cape-capf-super #'eglot-completion-at-point #'cape-keyword #'cape-file #'cape-dabbrev #'cape-line #'cape-dict)))))
	:bind (:map eglot-mode-map
							("C-c C-e C-a" . eglot-code-actions)
							("C-c C-e a" . eglot-code-actions)
							("C-c C-e C-r" . eglot-rename)
							("C-c C-e r" . eglot-rename)
							("C-c C-e C-f" . eglot-format-buffer)
							("C-c C-e f" . eglot-format-buffer))
	:hook
	(c-ts-mode . eglot-ensure)
	(zig-mode . eglot-ensure)
	(c++-ts-mode . eglot-ensure))

;; magit
(use-package magit
	:bind
  (("C-c g" . magit)))
(use-package forge
	:after (magit))

;; ERC (IRC)
(use-package erc
  :config
	(setq erc-auto-query 'window-noselect)
  (setq erc-nick "gerogaga" erc-server "irc.libera.chat" erc-port 6667)
  (setq erc-modules '(button completion fill list match readonly ring scrolltobottom smiley stamp unmorse netsplit fill track networks autojoin noncommands irccontrols move-to-prompt menu))
  (erc-update-modules)
  :hook
  (erc-mode . (lambda () (undo-tree-mode -1) (eldoc-mode -1)))
  :hook (erc-echo-notice-hook . erc-echo-notice-in-minibuffer))

(use-package disaster
  :bind (:map c-ts-mode-map
							("C-c C-d" . disaster)
							:map c++-ts-mode-map
							("C-c C-d" . disaster)))

(use-package c-ts-mode
  :ensure nil
  :bind (:map c-ts-mode-map
							("C-c C-o" . ff-find-other-file-other-window)))
(use-package c++-ts-mode
  :ensure nil
  :bind (:map c++-ts-mode-map
							("C-c C-o" . ff-find-other-file-other-window)))

;; Shared Lisp config
(use-package paredit
  :hook
  (emacs-lisp-mode . paredit-mode)
	(lisp-mode . paredit-mode)
  :bind
  (:map paredit-mode-map
				("M-<right>" . paredit-forward-slurp-sexp)
				("M-<left>" . paredit-forward-barf-sexp)
				("C-<left>" . paredit-backward-slurp-sexp)
				("C-<right>" . paredit-backward-barf-sexp)
				("M-r" . move-to-window-line-top-bottom)
				("C-k" . paredit-kill)
				("C-M-f" . paredit-forward)
				("C-M-b" . paredit-backward))
	:config
	(add-hook 'sly-mrepl-mode-hook #'(lambda () (paredit-mode -1))))

(use-package lisp-extra-font-lock
  :config (lisp-extra-font-lock-global-mode))

(use-package rainbow-delimiters ;; TODO: remove?
	:hook (lisp-mode emacs-lisp-mode scheme-mode))

;; Common Lisp config
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl --noinform --no-linedit")
	:hook (sly-mrepl-mode . corfu-mode) (sly-mrepl-mode . paredit-mode))
(use-package sly-macrostep
	:requires (sly))
(use-package sly-asdf
	:requires (sly))
(use-package sly-quicklisp
	:requires (sly))

;; Org
(use-package org
  :ensure nil
  :config (add-to-list 'org-file-apps '(directory . emacs)))
(use-package org-indent
  :ensure nil
  :requires org
  :hook (org-mode))
(use-package org-modern
  :requires org
  :hook (org-mode) (org-agenda-finalize . org-modern-agenda))

;; Testing
(use-package affe)
(use-package projectile)
(use-package platformio-mode)
(use-package workgroups
	:custom (wg-prefix-key (kbd "C-c w")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
	 '(workgroups platformio-mode projectile affe org-modern sly-quicklisp sly-asdf sly-macrostep sly rainbow-delimiters lisp-extra-font-lock paredit disaster forge magit zig-mode yasnippet-snippets xclip with-editor which-key vertico undo-tree treesit-auto orderless marginalia kind-icon iedit hl-todo haki-theme flycheck embark-consult eat diff-hl corfu cape beacon))
 '(warning-suppress-types '((comp) (emacs) comp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow")))))
;;; init.el ends here
