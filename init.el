;;; init.el --- init
;;; Commentary:
;;; TODO: ASM setup, GUD setup, occur
;;; init.el, todos in ~/.emacs.d/emacs.org
;; -*- lexical-binding: t; -*-
;;; Code:

;;(hydra elfeed-goodies elfeed-dashboard elfeed-summary elfeed-autotag elfeed-tube elfeed-tube-mpv pdf-tools hl-todo org-contrib magit-todos magit-org-todos erc-hl-nicks erc-colorize sly sly-stepper)

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
	:custom (haki-region "#2e8b6d"))
(load-theme 'haki t)

;; load Guix packages
(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp")

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
	(setq-local completion-at-point-functions (list (cape-super-capf #'cape-keyword #'cape-file #'cape-dabbrev))))
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
(use-package tempel
  :custom (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))
(use-package tempel-collection
	:requires (tempel)
	:demand t)
(use-package eglot-tempel
	:straight nil
	:requires (eglot tempel)
	:config (eglot-tempel-mode))
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
(use-package guix
	:straight nil
	:demand t)
(use-package vertico
	:demand t
	:config (vertico-mode))
(use-package diff-hl
	:hook (prog-mode text-mode)
	:config (diff-hl-margin-mode))
(use-package color-identifiers-mode
  :demand t
  :config
  (global-color-identifiers-mode))
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
(use-package vlf
  :demand t)
(use-package iedit
	:demand t)
(use-package consult
  :demand t
	:after (orderless)
	:custom (consult-line-start-from-top t)
	:bind ("C-s" . consult-line))
(use-package embark
  :demand t
  :bind
	("C-M-SPC" . embark-act)
	("C-." . embark-dwim)
	("M-/" . embark-export))
(use-package embark-consult)
(use-package helpful
  :demand t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)
   ("C-c C-d" . helpful-at-point)))
(use-package eat
	:demand t
	:hook (eshell-load . eat-eshell-mode))
(use-package elfeed
  :demand t)
(use-package xclip
  :demand t
  :config (xclip-mode))
(use-package treesit-auto
  :config (global-treesit-auto-mode))
(use-package bufler
	:demand t
	:config (bufler-mode)
	:bind
	("C-x b" . bufler-switch-buffer)
	("C-x C-b" . bufler-list))
(with-eval-after-load 'dired '(require dired-x))

;; global functions

(defun create-tags (dir-name)
  "Create tags file in DIR-NAME."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

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
	:config
	(scroll-bar-mode -1)
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(global-auto-revert-mode)
	(pixel-scroll-precision-mode)
	(save-place-mode)
	(global-display-line-numbers-mode)
	(menu-bar--display-line-numbers-mode-relative)
	(electric-pair-mode)
	(recentf-mode)
	(global-auto-revert-mode)
	(undelete-frame-mode)
	(show-paren-mode)
	(defalias 'yes-or-no-p 'y-or-n-p)
	(defalias 'term 'ansi-term)
	(setq-default shell-file-name "bash")
	(setq-default tab-width 2 indent-tabs-mode t)
	(setq-default compile-command "make -j16")
	:hook (xref-after-update-hook . outline-minor-mode)
	:bind (("M-i" . imenu) ("C-r" . replace-string) ("C-c C-c" . compile) ("C-j" . end-newline))
	:custom
	(debug-on-error t) (load-prefer-newer t) (sentence-end-double-space t) (make-backup-files nil) (select-enable-clipboard t) (next-line-add-newlines t) (show-paren-context-when-offscreen t) (compilation-auto-jump-to-first-error 'first-known) (completion-cycle-threshold 3) (tab-always-indent 'complete))

;; Major modes without extra config

(use-package zig-mode
	:init (defvar zig-pairs '((?| . ?|)))
	(defun zig-add-electric-pairs ()
		(setq-local electric-pair-pairs (append electric-pair-pairs zig-pairs)))
	:hook (zig-mode . zig-add-electric-pairs)
	:demand t
	:config (setq compile-command "zig build-exe"))
(use-package rustic
	:demand t
	:config (setq compile-command "cargo build"))

;; LSP
(use-package eglot
	:straight nil
	:requires (orderless cape)
	:after (tempel)
	:demand t
	:custom (completion-category-overrides '((eglot (styles orderless))))
	:config (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) (setq completion-category-overrides '((eglot (styles orderless))))
	:hook (eglot-managed-mode . (setq-local completion-at-point-functions
																					 (list (cape-super-capf #'eglot-completion-at-point #'tempel-expand #'cape-file))))
	:hook (eglot-managed-mode . eglot-cape-tempel-capf))

;; magit
(use-package magit
  :demand t
	:bind
  (("C-c g" . magit)))
(use-package forge
	:demand t
	:after (magit))
;; Debug
(defun debug-gud ()
  "GUD setup for Rust and core files."
  (interactive)
  (cond ((and (or (equal major-mode 'c-mode) (equal major-mode 'c++-mode)) (file-exists-p "./core"))
				 ((setq gud-gdb-command-name "gdb -i=mi ./core") (gud-gdb)))
				((equal major-mode 'rust-mode) (rust-gdb))))

;; ERC (IRC)
(use-package erc
  :demand t
  :config
(setq erc-auto-query 'window-noselect)
  (setq erc-nick "Pay08" erc-server "irc.libera.chat" erc-port 6667)
  (setq erc-modules '(button completion fill list match readonly ring scrolltobottom smiley stamp spelling unmorse netsplit fill track networks autojoin noncommands irccontrols move-to-prompt menu))
  (erc-update-modules)
  :hook
  (erc-mode . (lambda () (undo-tree-mode -1) (eldoc-mode -1)))
  :hook (erc-echo-notice-hook . erc-echo-notice-in-minibuffer))

;; Ement (Matrix)
(use-package ement
	:demand t)

;; C/C++ config
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))  ;; clangd is fast

(use-package disaster
  :bind (:map c-mode-map
							("C-c C-d" . disaster)
							:map c++-mode-map ;; todo fix
							("C-c C-d" . disaster)))

(use-package c-mode
  :straight nil
  :bind (:map c-mode-map
							("C-c C-o" . ff-find-other-file-other-window))
  :custom ((gc-cons-treshold (* 100 1024 1024)) (read-process-output-max (* 1024 1024))))
(use-package c++-mode
  :straight nil
  :bind (:map c++-mode-map
							("C-c C-o" . ff-find-other-file-other-window))
	:custom ((gc-cons-treshold (* 100 1024 1024)) (read-process-output-max (* 1024 1024))))

;; Shared Lisp config
(use-package paredit
  :demand t
  :hook
  (lisp-mode eval-expression-minibuffer-setup lisp-interaction-mode slime-repl-mode emacs-lisp-mode scheme-mode)
  :bind
  (:map paredit-mode-map
				("M-<right>" . paredit-forward-slurp-sexp)
				("M-<left>" . paredit-forward-barf-sexp)
				("C-<left>" . paredit-backward-slurp-sexp)
				("C-<right>" . paredit-backward-barf-sexp)
				("M-r" . move-to-window-line-top-bottom)
				("C-k" . paredit-kill))) ; todo bind paredit-forward and paredit-backward and paredit-kill

(use-package highlight-function-calls
  :hook (lisp-mode emacs-lisp-mode scheme-mode))

(use-package lisp-extra-font-lock
  :config (lisp-extra-font-lock-global-mode))

(use-package rainbow-delimiters
 	:demand t
	:hook (lisp-mode emacs-lisp-mode scheme-mode))

;; Common Lisp config
(use-package slime
  :demand t
  :config (slime-setup '(slime-fancy slime-asdf slime-banner slime-autodoc))
  :hook (lisp-mode)
  :config
  (setq inferior-lisp-program "sbcl --noinform --no-linedit"))

;; Guile Config
(use-package geiser-guile
	:demand t)
(use-package macrostep-geiser
	:demand t)

;; ASM config (gas)

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
  :hook ((org-mode) (org-agenda-finalize . org-modern-agenda)))
;; (use-package org-babel
;; 	:straight nil
;;   :requires org
;;   :demand t
;;   :config
;;   ((org-babel-do-load-languages
;;     'org-babel-load-languages
;;     '((shell . t)
;;       (emacs-lisp . t)
;;       (arduino . t)
;;       (lisp . t)
;;       (makefile . t)
;;       (org . t)
;;       (calc . t)
;; 			(C . t)
;; 			(cpp . t)
;; 			(eshell . t)
;; 			(scheme . t)))
;;    (setq org-src-preserve-indentation t org-src-fontify-natively t org-confirm-babel-evaluate nil)))
(use-package org-download
  :demand t
  :requires org)
;; GUD

;; Python
(use-package jedi
	:demand t
	:hook (python-mode . jedi:setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
	 '(((:application tramp)
			tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
	 '((tramp-connection-local-darwin-ps-profile
			(tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
			(tramp-process-attributes-ps-format
			 (pid . number)
			 (euid . number)
			 (user . string)
			 (egid . number)
			 (comm . 52)
			 (state . 5)
			 (ppid . number)
			 (pgrp . number)
			 (sess . number)
			 (ttname . string)
			 (tpgid . number)
			 (minflt . number)
			 (majflt . number)
			 (time . tramp-ps-time)
			 (pri . number)
			 (nice . number)
			 (vsize . number)
			 (rss . number)
			 (etime . tramp-ps-time)
			 (pcpu . number)
			 (pmem . number)
			 (args)))
		 (tramp-connection-local-busybox-ps-profile
			(tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
			(tramp-process-attributes-ps-format
			 (pid . number)
			 (user . string)
			 (group . string)
			 (comm . 52)
			 (state . 5)
			 (ppid . number)
			 (pgrp . number)
			 (ttname . string)
			 (time . tramp-ps-time)
			 (nice . number)
			 (etime . tramp-ps-time)
			 (args)))
		 (tramp-connection-local-bsd-ps-profile
			(tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
			(tramp-process-attributes-ps-format
			 (pid . number)
			 (euid . number)
			 (user . string)
			 (egid . number)
			 (group . string)
			 (comm . 52)
			 (state . string)
			 (ppid . number)
			 (pgrp . number)
			 (sess . number)
			 (ttname . string)
			 (tpgid . number)
			 (minflt . number)
			 (majflt . number)
			 (time . tramp-ps-time)
			 (pri . number)
			 (nice . number)
			 (vsize . number)
			 (rss . number)
			 (etime . number)
			 (pcpu . number)
			 (pmem . number)
			 (args)))
		 (tramp-connection-local-default-shell-profile
			(shell-file-name . "/bin/sh")
			(shell-command-switch . "-c"))
		 (tramp-connection-local-default-system-profile
			(path-separator . ":")
			(null-device . "/dev/null"))))
 '(elfeed-feeds
	 '("https://www.youtube.com/feeds/videos.xml?channel_id=UCdJdEguB1F1CiYe7OEi3SBg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCtowLlQSH6QRtp0-Z26U17A" "https://www.youtube.com/feeds/videos.xml?channel_id=UCKtix2xNNXdcEfEFnoOnvMw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCRC6cNamj9tYAO6h_RXd5xA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCBa659QWEk1AI4Tg--mrJ2A" "https://www.youtube.com/feeds/videos.xml?channel_id=UCQD3awTLw9i8Xzh85FKsuJA" "https://www.youtube.com/feeds/videos.xml?channel_id=UClyGlKOhDUooPJFy4v_mqPg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCnQC_XGCCI__qrxwgZS27-A" "https://www.youtube.com/feeds/videos.xml?channel_id=UCcnci5vbpLJ-rh_V9yXwawg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCS0N5baNlQWJCUrhCEo8WlA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCD6VugMZKRhSyzWEWA9W2fg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCtHaxi4GTYDpJgMSGy7AeSw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCCnILYoBNuR4qaUOynGWzRg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCJ0-OtVpF0wOKEqT2Z1HEtA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCFLwN7vRu8M057qJF8TsBaA"))
 '(inhibit-startup-screen t)
 '(warning-suppress-types '((emacs) (comp) comp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:underline "red"))))
 '(flymake-warning ((((class color)) (:underline "yellow")))))
;;; init.el ends here
