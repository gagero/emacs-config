;;; init.el --- init
;;; Commentary:
;;; TODO: ASM setup, GUD setup, occur, xref keybindings
;;; init.el, todos in ~/.emacs.d/emacs.org
;; -*- lexical-binding: t; -*-
;;; Code:

;;(erc-hl-nicks erc-colorize sly-stepper)

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
(setq straight-use-package-by-default t)

(use-package haki-theme
	:demand t
	:custom (haki-region "#2e8b6d")
	:config (load-theme 'haki t))
(add-to-list 'load-path "~/.emacs.d/info+")

;; global minor modes
(use-package corfu
	:demand t
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
	:demand t
	:after (corfu)
	:custom (kind-icon-default-face 'corfu-default)
	:config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
(use-package orderless
	:demand t
	:init (setq completion-styles '(orderless basic partial-completion emacs22  initials substring) completion-category-defaults nil completion-category-overrides '((file (styles . (partial-completion))))))
(use-package cape
	:demand t
	:config
	(setq completion-at-point-functions (list (cape-super-capf #'cape-keyword #'cape-file #'cape-dabbrev))))
(use-package which-key
	:demand t
  :config
  (which-key-setup-side-window-right)
	(which-key-mode))
(use-package beacon
  :config
  (beacon-mode))
(use-package undo-tree
  :demand t
  :config (global-undo-tree-mode))
(use-package flycheck
  :demand t
	:config (global-flycheck-mode)
	(flycheck-define-checker zig
		"A zig syntax checker using zig's `ast-check` command."
		:command ("zig" "ast-check" (eval (buffer-file-name)))
		:error-patterns
		((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
		:modes zig-mode)
  (add-to-list 'flycheck-checkers 'zig)
  :bind (:map flycheck-mode-map)
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error))
(use-package info+
	:straight nil
	:demand t)
(use-package vertico
	:demand t
	:config (vertico-mode))
(use-package diff-hl
	:hook (prog-mode text-mode)
	:config (diff-hl-margin-mode))
(use-package marginalia
  :demand t
  :config (marginalia-mode)
  :bind
  (:map minibuffer-local-map
				("M-A" . marginalia-cycle)))
(use-package hideshow
  :straight nil
  :demand t
  :hook (prog-mode . hs-minor-mode))
(use-package iedit
	:demand t)
(use-package consult
  :demand t
	:after (orderless)
	:custom (consult-line-start-from-top t)
	:bind ("C-s" . consult-line) ("M-i" . consult-imenu) ("C-x b" . consult-buffer) ("C-x p b" . consult-project-buffer))
(use-package embark
  :demand t
  :bind
	("C-M-SPC" . embark-act)
	("C-." . embark-dwim)
	("M-/" . embark-export))
(use-package embark-consult)
(use-package eat
	:demand t
	:hook (eshell-load . eat-eshell-mode))
(use-package xclip
  :demand t
  :config (xclip-mode))
(use-package treesit-auto
  :config (global-treesit-auto-mode))
(use-package hl-todo
	:demand t
	:config (hl-todo-mode))
(with-eval-after-load 'dired '(require dired-x))

;; global functions

(defun end-newline ()
	"Inserts newline below point."
	(interactive)
	(move-end-of-line)
	(newline-and-indent))

(defun begin-newline ()
	"Inserts newline above point."
	(interactive)
	(move-beginning-of-line)
	(newline-and-indent))

(defun indent-buffer ()
	"Indents the entire buffer."
	(interactive)
	(indent-region 0 (buffer-size)))

(use-package emacs
	:straight nil
	:demand t
	:after (consult)
	:config
	(scroll-bar-mode -1)
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(global-auto-revert-mode)
	(pixel-scroll-precision-mode)
	(save-place-mode)
	(menu-bar--display-line-numbers-mode-relative)
	(electric-pair-mode)
	(recentf-mode)
	(global-auto-revert-mode)
	(undelete-frame-mode)
	(show-paren-mode)
	(setq-default shell-file-name "bash" tab-width 2 indent-tabs-mode t compile-command "make -j16")
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
	:bind (("C-r" . replace-string) ("C-c C-c" . compile) ("C-j" . end-newline) ("C-x p C-f" . project-find-file)))
(setq  debug-on-error t load-prefer-newer t sentence-end-double-space t make-backup-files nil select-enable-clipboard t next-line-add-newlines t show-paren-context-when-offscreen t compilation-auto-jump-to-first-error 'first-known completion-cycle-threshold 3 tab-always-indent 'complete gc-cons-threshold (* 100 1024 1024) read-process-output-max (* 1024 1024) browse-url-browser-function #'eww-browse-url)

;; Major modes without extra config
(use-package zig-mode
	:init (defvar zig-pairs '((?| . ?|)))
	(defun zig-add-electric-pairs ()
		(setq-local electric-pair-pairs (append electric-pair-pairs zig-pairs)))
	:hook (zig-mode . zig-add-electric-pairs)
	:demand t
	:config (setq-local compile-command "zig build-exe"))

;; LSP
(use-package eglot
	:straight nil
	:requires (orderless cape)
	:demand t
	:config (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) (setq completion-category-overrides '((eglot (styles orderless))))
	(add-hook 'eglot-managed-mode-hook #'(lambda () (setq-local completion-at-point-functions
																															(list (cape-super-capf #'eglot-completion-at-point #'cape-keyword #'cape-file #'cape-dabbrev #'cape-line #'cape-dict)))))
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
  :demand t
	:bind
  (("C-c g" . magit)))
(use-package forge
	:demand t
	:after (magit))

;; Debugging
(defun debug-gud ()
  "GUD setup for core files."
  (interactive)
  (when (and (or (equal major-mode 'c-mode) (equal major-mode 'c++-mode)) (file-exists-p "./core"))
		(setq gud-gdb-command-name "gdb -i=mi ./core") (gud-gdb)))

;; ERC (IRC)
(use-package erc
  :demand t
  :config
	(setq erc-auto-query 'window-noselect)
  (setq erc-nick "gerogaga" erc-server "irc.libera.chat" erc-port 6667)
  (setq erc-modules '(button completion fill list match readonly ring scrolltobottom smiley stamp spelling unmorse netsplit fill track networks autojoin noncommands irccontrols move-to-prompt menu))
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
  :straight nil
  :bind (:map c-ts-mode-map
							("C-c C-o" . ff-find-other-file-other-window)))
(use-package c++-ts-mode
  :straight nil
  :bind (:map c++-ts-mode-map
							("C-c C-o" . ff-find-other-file-other-window)))

;; Shared Lisp config
(use-package paredit
  :demand t
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

(use-package rainbow-delimiters
 	:demand t
	:hook (lisp-mode emacs-lisp-mode scheme-mode))

;; Common Lisp config
(use-package sly
  :demand t
  :config
  (setq inferior-lisp-program "sbcl --noinform --no-linedit")
	:hook (sly-mrepl-mode . corfu-mode) (sly-mrepl-mode . paredit-mode))
(use-package sly-macrostep
	:demand t
	:requires (sly))
(use-package sly-asdf
	:demand t
	:requires (sly))
(use-package sly-quicklisp
	:demand t
	:requires (sly))

;; Org
(use-package org
  :straight nil
  :demand t
  :config (add-to-list 'org-file-apps '(directory . emacs)))
(use-package org-auto-tangle
  :requires org
  :demand t
  :hook (org-mode))
(use-package org-indent
  :straight nil
  :requires org
  :demand t
  :hook (org-mode))
(use-package org-modern
  :requires org
  :demand t
  :hook (org-mode) (org-agenda-finalize . org-modern-agenda))

;; Testing
;; (use-package combobulate)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(warning-suppress-types '((comp) (emacs) comp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow")))))
;;; init.el ends here
