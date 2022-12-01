;;; init.el --- init
;; -*- lexical-binding: t; -*-
;;; Code:
(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
	 '("4aafea32abe07a9658d20aadcae066e9c7a53f8e3dfbd18d8fa0b26c24f9082c" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(erc-auto-query 'window-noselect)
 '(erc-modules
	 '(button completion fill list match readonly ring scrolltobottom smiley stamp spelling unmorse hl-nicks netsplit fill track networks autojoin noncommands irccontrols move-to-prompt menu))
 '(package-selected-packages
	 '(paredit rainbow-delimiters nasm-mode emms swiper lsp-mode irony git-commit flycheck company company-c-headers hydra ivy mastodon go-translate elfeed-goodies elfeed-dashboard elfeed-summary elfeed-autotag elfeed-tube elfeed-tube-mpv elfeed embark diff-hl evil pdf-tools hl-todo vlf arduino-cli-mode platformio-mode lsp-javacomp javadoc-lookup company-arduino arduino-mode org-modern yasnippet writegood-mode which-key wc-mode undo-tree uncrustify-mode magit tree-sitter-langs rustic orgit org-roam-ui org-contrib org-auto-tangle magit-todos magit-org-todos lsp-ui lsp-ivy ligature ivy-emms irony-eldoc flycheck-rust flycheck-irony exwm eshell-vterm erc-yt erc-yank erc-tweet erc-scrolltoplace erc-image erc-hl-nicks erc-colorize erc emms-state emms-info-mediainfo eldoc-cmake counsel company-irony-c-headers company-irony color-identifiers-mode cmake-project cmake-mode c-eldoc beacon annalist))
 '(save-place-mode t)
 '(warning-suppress-log-types '((comp) (comp) (emacs)))
 '(warning-suppress-types '((comp) (emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "CTDB" :slant normal :weight normal :height 108 :width normal)))))

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents t)
(package-initialize)

;; global minor modes
(require 'lsp-mode)
(require 'which-key)
(which-key-setup-side-window-right)
(which-key-mode)
(require 'flycheck)
(global-flycheck-mode)
(electric-pair-mode)
(require 'beacon)
(beacon-mode)
(global-undo-tree-mode)
(require 'ivy)
(ivy-mode)
(require 'company)
(global-company-mode)
(global-auto-revert-mode)
(tool-bar-mode -1)
(require 'counsel)
(counsel-mode)
(require 'diff-hl)
(global-diff-hl-mode)
(setq make-backup-files nil select-enable-clipboard t)
(require 'rainbow-delimiters)
(rainbow-delimiters-mode)
;(require 'go)
;(setq gts-translate-list )
(desktop-save-mode 1)

;; global hooks
(add-hook 'before-save-hook 'lsp-format-buffer)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; minor hooks
(add-hook 'nxml-mode-hook (lambda () (writegood-mode -1) (whitespace-mode -1)))
(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'vterm-mode-hook (lambda () (evil-mode -1)))
;(add-hook 'eww-mode-hook (lambda () (evil-mode -1))) ; TODO: fix

;; global settings
(global-display-line-numbers-mode)
(menu-bar--display-line-numbers-mode-relative)
(setq-default shell-file-name "/bin/bash")
(scroll-bar-mode -1)
(setq-default tab-width 2 indent-tabs-mode t)
(setq debug-on-error t)

;; global keybindings
(defun turn-off-minibuffer-buffer ()
"Turn off ivy-mode to get rid of the separate buffer for the minibuffer."
(interactive)
  (ivy-mode -1)
  (counsel-mode -1))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c e") 'evil-mode)
(global-set-key (kbd "C-c r") 'eval-buffer)
(global-set-key (kbd "C-c c") 'compile)

  ;; magit keybindings
(global-set-key (kbd "C-c g c") 'magit-commit)
(global-set-key (kbd "C-c g s") 'magit-status)

;; Evil
(setq-default evil-want-keybinding nil)
(require 'evil)
(evil-mode)

;; EXWM
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-0" 1 "DVI-I-1"))
;; (add-hook 'exwm-randr-screen-change-hook
;; 		  (lambda ()
;; 			(start-process-shell-command "xrandr" nil "xrandr --output HDMI-0 --mode 1920x1080 --pos 0x0 --roate-normal --output DVI-I-1 --primary --mode 1920x1080 --pos 1920x0 --rotate-normal")))
;; (exwm-randr-enable)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;; lsp-ui
(require 'lsp-ui)
(require 'lsp-ui-sideline)
(require 'lsp-ui-doc)
(global-set-key (kbd "C-c u d") 'lsp-ui-doc-toggle)
(setq lsp-ui-doc-delay 0.0)
(global-set-key (kbd "C-c u s") 'lsp-ui-sideline-mode)
(setq lsp-ui-sideline-delay 0.0)

;; emms
(require 'emms)
(require 'emms-setup)
(emms-all)
(emms-default-players)
(defvar emms-source-file-default-directory "/home/gabor/Music")
(defvar emms-browser-switch-to-playlist-on-add t)
(setq emms-repeat-playlist t
      emms-random-playlist t)
(global-set-key (kbd "C-c m s") (lambda () (interactive) (emms-play-directory-tree "~/Music")))
(global-set-key (kbd "C-c m p") 'emms-stop)
(global-set-key (kbd "C-c m r") 'emms-start)

;; ERC (IRC)
(require 'erc)
(setq erc-echo-notice-hook 'erc-echo-notice-in-minibuffer)
(setq erc-nick "Pay08"
      erc-server "irc.libera.chat"
      erc-port 6667)
(add-hook 'erc-mode-hook (lambda ()
						   (undo-tree-mode -1)
						   (flycheck-mode -1)
						   (eldoc-mode -1)
						   (counsel-mode -1)
						   (ivy-mode -1)
						   (which-key-mode -1)
						   (company-mode)))

;; mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(load "mu4e")

;; dired
(add-hook 'dired-mode-hook '(image-dired-display-thumbs 1))

;; Rust config
(add-hook 'rustic-mode-hook 'lsp)

;; C/C++ config
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  ;; irony
;; (require 'irony)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (eval-after-load 'company
  ;; '(add-to-list 'company-backends 'company-irony))
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony-c-headers))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; (add-hook 'irony-mode-hook #'irony-eldoc)

;; Shared Lisp config

(add-hook 'lisp-mode-hook (lambda () (lsp-mode -1)))

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)

;; Common Lisp config
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(load "/usr/share/emacs/site-lisp/slime/slime-autoloads")
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-hook 'slime-mode-hook (defun slime-sanitize-bindings ()
			     "Removes SLIME's keybinding on C-c x"
			     (cond ((boundp 'slime-mode-map)
				    (define-key slime-mode-map (kbd "C-c x") nil)
				    (message "SLIME keybinding on C-c x has been sanitized"))
				   ('t (message "SLIME keybindings not sanitized")))))

(add-hook 'lisp-mode-hook 'slime)
(global-set-key (kbd "C-c C-q") 'slime-close-all-parens-in-sexp)
;; Elisp config
(add-hook 'emacs-lisp-mode-hook (lambda () (lsp-mode -1)))
(when (string-equal major-mode "emacs-lisp-mode")
	(add-hook 'after-save-hook '(byte-recompile-directory "~/.emacs.d")))
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; ASM config (NASM)



;; ebuild
(add-hook 'ebuild-mode-hook 'ebuild-repo-mode)

;; Org
(require 'org-auto-tangle)
(require 'org-indent)
(add-hook 'org-mode-hook (lambda () (org-indent-mode) (org-auto-tangle-mode)))
(add-hook 'org-mode-hook (lambda () (yas-minor-mode -1) (evil-mode -1)))
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (emacs-lisp . t)
   (arduino . t)
   (lisp . t)
   (makefile . t)
   (org . t)
   (calc . t)))
(setq org-src-preserve-indentation t org-src-fontify-natively t org-confirm-babel-evaluate nil)

;; Info
;(add-hook 'Info-mode-hook (lambda () (evil-mode -1))) ; TODO: fix
;; ligatures
(ligature-set-ligatures t '("www"))
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
(global-ligature-mode t)


;;; init.el ends here
