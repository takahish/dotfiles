;; base
(require 'tool-bar)
(require 'scroll-bar)
(menu-bar-mode nil)
(tool-bar-mode nil)
(line-number-mode t)
(column-number-mode t)
(which-function-mode t)
(global-set-key "\C-h" 'delete-backward-char)
(global-font-lock-mode t)
(set-scroll-bar-mode 'right)
(setq-default indent-tabs-mode nil)
(set-language-environment "UTF-8")
(define-key global-map [?¥] [?\\])
(set-default-font "Inconsolata-14")

(custom-set-variables
 '(locale-coding-system 'utf-8)
 '(frame-title-format (format "%%b - %s:%%f"  (system-name)))
 '(inhibit-startup-message t)
 '(history-length 1000)
 '(undo-limit 100000)
 '(undo-strong-limit 130000)
 '(transient-mark-mode t)
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(auto-save-default nil)
 '(make-backup-files nil))

;; increased resent files
(require 'recentf nil t)
(custom-set-variables
 '(recentf-save-file "~/.emacs.d/recentf")
 '(recentf-max-saved-items 3000))
(recentf-mode t)

;; theme
(load-theme 'misterioso t)

;; load-path
(add-to-list 'load-path "~/bin")

;; package
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; required-packages
(defvar required-packages
  '(helm
    popwin
    auto-complete
    slime
    ac-slime
    helm-gtags))

;; no-installed-packages
(defun check-installed (pkgs)
  (if (null pkgs)
      nil
    (if (not (package-installed-p (car pkgs)))
        (cons (car pkgs) (check-installed (cdr pkgs)))
      (check-installed (cdr pkgs)))))

(let ((not-installed-packages
       (check-installed required-packages)))
  (when not-installed-packages
    (package-refresh-contents)
    (dolist (package not-installed-packages)
      (package-install package))))

;; helm
(require 'helm-config)
(defun helm-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))
;;; changed prefix key etc
(custom-set-variables
 '(helm-quick-update t)
 '(helm-enable-shortcuts 'alphabet)
 '(helm-command-prefix-key " a")
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp))))
 '(helm-display-function 'helm-split-window))
;;; changed helm display
(define-key helm-command-map (kbd "M-n") 'helm-next-source)
(define-key helm-command-map (kbd "M-p") 'helm-previous-source)

;; popwin
(require 'popwin)
(custom-set-variables
 '(display-buffer-function 'popwin:display-buffer))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(custom-set-variables
 '(ac-use-menu-map t)
 '(ac-use-fuzzy t))

;; org
(custom-set-variables
 '(org-log-done 'time))

;; common-lisp
(setq-default
 inferior-lisp-program "/usr/local/bin/sbcl")
; inferior-lisp-program "/usr/local/bin/clisp")
; inferior-lisp-program "/usr/local/bin/abcl-no-rlwrap")
; inferior-lisp-program "/usr/local/bin/ccl")

;; slime
(autoload 'slime "slime" nil t)
(with-eval-after-load 'slime
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
  (require 'ac-slime))
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; helm-gtags
(autoload 'helm-gtags-mode "helm-gtags" nil t)
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
             (local-set-key "\M-t" 'helm-gtags-find-tag)
             (local-set-key "\M-r" 'helm-gtags-find-rtag)
             (local-set-key "\M-s" 'helm-gtags-find-symbol)
             (local-set-key "\C-t" 'helm-gtags-pop-stack)))
