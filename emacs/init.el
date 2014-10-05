;; load-path
(let ((default-directory "~/.emacs.d/site-lisp"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq auto-install-save-confirm nil)

;; package
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; base
(setq inhibit-startup-message t)
(menu-bar-mode nil)
(require 'tool-bar)
(tool-bar-mode nil)
(line-number-mode t)
(column-number-mode t)
(which-function-mode t)
(setq-default indent-tabs-mode nil)

;; other
(global-set-key "\C-h" 'delete-backward-char)
(setq frame-title-format (format "%%b - %s:%%f"  (system-name)))
(global-font-lock-mode t)
(require 'scroll-bar)
(set-scroll-bar-mode 'right)
(setq history-length 1000)
(setq undo-limit 100000)
(setq undo-strong-limit 130000)
(setq transient-mark-mode t)
(setq scroll-conservatively 1)
(setq scroll-step 1)
(setq default-frame-alist '((width . 100)
                            (height . 35)
                            (line-spacing . 2)
                            (font . "fontset-14")))
;(setq make-backup-files nil)
(setq auto-save-default nil)
(define-key global-map [?¥] [?\\])
(setq locale-coding-system 'utf-8)
;;; japanese
; (prin1 (font-family-list))
(set-language-environment "UTF-8")
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))
(setq face-font-rescale-alist
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)))
;;; theme
(load-theme 'misterioso t)

;; anything
(require 'anything-startup)
(require 'anything-match-plugin)
(setq anything-quick-update t)
(setq anything-enable-shortcuts 'alphabet)
;;; changed anything prefix key（default F5 a）
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anything-command-map-prefix-key " a")
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp)))))
;;; changed anything display
(define-key anything-map (kbd "M-n") 'anything-next-source)
(define-key anything-map (kbd "M-p") 'anything-previous-source)
(defun anything-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))
(setq anything-display-function 'anything-split-window)
;;; increased resent files
(when (require 'recent nil t)
  (custum-set-variables '(recentf-save-file "~/.emacs.d/recent"))
  (setq recentf-max-saved-items 3000)
  (recentf-mode 1))

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; org
(setq org-log-done 'time)

;; sr-speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq speedbar-use-images nil)
(sr-speedbar-refresh-turn-off)
(global-set-key (kbd "C-^") 'sr-speedbar-toggle)
(add-hook 'speedbar-mode-hook
          '(lambda ()
             (speedbar-add-supported-extension
              '("lisp" "py" "rb" "pl" "scala" "clj" "org" "html" "xml" "*"))))
(provide 'init_speedbar)

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
;(setq inferior-lisp-program "/usr/local/bin/clisp")
;(setq inferior-lisp-program "/usr/local/bin/abcl-no-rlwrap")
;(setq inferior-lisp-program "/usr/local/bin/ccl")
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
;;; popwin on slime
(push '("*slime-apropos*") popwin:special-display-config)
(push '("*slime-macroexpansion*") popwin:special-display-config)
(push '("*slime-description*") popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push '("*slime-xref*") popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push '(slime-repl-mode) popwin:special-display-config)
(push '(slime-connection-list-mode) popwin:special-display-config)
;;; ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; scala-mode
(unless (package-installed-p 'ensime)
  (package-refresh-contents)
  (package-install 'ensime))
(require 'ensime)
(push "/usr/local/bin" exec-path)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; clojure-mode
;;; There needs to install package.el in emacs 23
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)

;; pig-mode
(require 'pig-mode)
