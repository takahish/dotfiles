;;; Global setting.
;;; ==========================================================================
(global-set-key "\C-h" 'delete-backward-char) ; bind \C-h to deleting char.
(global-font-lock-mode t)                     ; Font look is True.
(setq-default indent-tabs-mode nil)           ; Indent is space only.
(set-language-environment "UTF-8")            ; Charactor code is UTF-8.
(setq-default inhibit-splash-screen t)        ; Remove splash screen.

;; Theme setting.
(load-theme 'wombat t)

;; Common lisp setting.
(setq-default inferior-lisp-program "/usr/local/bin/sbcl")           ; SBCL
;(setq-default inferior-lisp-program "/usr/local/bin/clisp")          ; CLisp
;(setq-default inferior-lisp-program "/usr/local/bin/abcl-no-rlwrap") ; ABCL
;(setq-default inferior-lisp-program "/usr/local/bin/ccl")            ; ClozureCL


;;; Package setting.
;;; ==========================================================================
(require 'package)

;; Added package-archives.
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Initialization.
(package-initialize)

;; Required packages.
(defvar required-packages
  '(popwin
    helm
    auto-complete
    slime
    ac-slime))

;; Defun helper function.
(defun check-installed (pkgs)
  (if (null pkgs)
      nil
    (if (not (package-installed-p (car pkgs)))
        (cons (car pkgs) (check-installed (cdr pkgs)))
      (check-installed (cdr pkgs)))))

;; Install not installed package.
(let ((not-installed-packages
       (check-installed required-packages)))
  (when not-installed-packages
    (package-refresh-contents)
    (dolist (package not-installed-packages)
      (package-install package))))


;;; Recentf setting.
;;; ==========================================================================
(require 'recentf)
(recentf-mode t)

;; Set custom variables.
(custom-set-variables
 '(recentf-save-file "~/.emacs.d/recentf")
 '(recentf-max-saved-items 3000) ; increased resent files
 '(recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list)))


;;; Whitespace setting.
;;; ==========================================================================
(require 'whitespace)

;; Set custom variables.
(custom-set-variables
 '(whitespace-style
   '(face trailing tabs spaces empty space-mark tab-mark))
 '(whitespace-display-mappings
   '((space-mark ?\u3000 [?\u25a1])
     (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
 '(whitespace-space-regexp "\\(\u3000+\\)"))

;; Whitespace is True.
(global-whitespace-mode t)

;; Hook saving file, delete whitespace before close.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Popwin setting.
;;; ==========================================================================
(require 'popwin)

;; Set custom variables.
(custom-set-variables
  '(display-buffer-function 'popwin:display-buffer)
  '(popwin:popup-window-position 'bottom)
  '(popwin:special-display-config '(("*complitation*" :noselect t))))


;;; Helm setting.
;;; ==========================================================================
(require 'helm)

;; Basic configulation.
(require 'helm-config)

;; Set custom variables.
(custom-set-variables
  '(helm-command-prefix-key "^X a")
  '(helm-display-function #'display-buffer) ; For using popwin:display-buffer.
  '(helm-enable-shortcuts 'alphabet)
  '(helm-quick-update t)
  '(popwin:special-display-config
    (cons '("helm" :regexp t :height 0.4) popwin:special-display-config)))


;;; Auto-complete setting.
;;; ==========================================================================
(require 'auto-complete)
(require 'auto-complete-config)

;; Basic configulation.
(ac-config-default)

;; Set custom variables.
(custom-set-variables
 '(global-auto-complete-mode t)
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
 '(ac-auto-start t))

;; Set key.
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Add mode.
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'sql-mode)


;;; Slime setting.
;;; ==========================================================================
;; Load slime setting when it is used.
(autoload 'slime "slime" nil t)

;; Initialization.
(with-eval-after-load 'slime
  (slime-setup '(slime-repl slime-fancy slime-banner))
  ;; Set custom variables.
  (custom-set-variables
   '(popwin:special-display-config
     (append '(("*slime-apropos*")
               ("*slime-macroexpansion*")
               ("*slime-description*")
               ("*slime-compilation*" :noselect t)
               ("*slime-xref*")
               (sldb-mode :stick t)
               (slime-repl-mode)
               (slime-connection-list-mode))
             popwin:special-display-config)
     ;; auto-complete
     (require 'ac-slime))))

;; Hook lunching slime.
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
