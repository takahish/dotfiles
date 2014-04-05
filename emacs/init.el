;; load-pathを再起的に定義
(let ((default-directory "~/.emacs.d/site-lisp"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; auto-installの設定
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/auto-install/")
;(auto-install-update-emacswiki-package-name t) ;; 必要に応じて使用する
(auto-install-compatibility-setup)
(setq auto-install-save-confirm nil)

;; 基本設定
(setq inhibit-startup-message t) ;; 初期画面を表示しない
(menu-bar-mode nil) ;; メニューバーを表示しない
(require 'tool-bar)
(tool-bar-mode nil) ;; ツールバーを表示しない
(line-number-mode t) ;; 現在の行番号を表示する
(column-number-mode t) ;; 現在の列番号を表示する
(which-function-mode t) ;; 現在の関数を表示する
(setq-default indent-tabs-mode nil) ;; タブを使わない

;; その他の設定
(global-set-key "\C-h" 'delete-backward-char) ;; C-hを1文字削除に変更
(setq frame-title-format (format "%%b - %s:%%f"  (system-name))) ;; フレームタイトルの設定
(global-font-lock-mode t) ;; 色をつける
(require 'scroll-bar)
(set-scroll-bar-mode 'right) ;; スクロールバーを右側に表示する
(setq history-length 1000) ;; 履歴をたくさん表示する
(setq undo-limit 100000)
(setq undo-strong-limit 130000) ;; undoの数を増やす
(setq transient-mark-mode t) ;; 範囲に色をつける
(setq scroll-conservatively 1)
(setq scroll-step 1) ;; スクロールの設定
(add-to-list 'default-frame-alist
	     '(font . "-unknown-Takaoゴシック-normal-normal-normal-*-14-*-*-*-d-0-iso10646-1")) ;; デフォルトのフォントの設定
;(setq make-backup-files nil) ;; バックアップファイルを作らない
(setq auto-save-default nil) ;; 自動保存を行わない
(set-language-environment "UTF-8") ;; 日本語の設定
(define-key global-map [?¥] [?\\]) ;; ¥の代わりにバックスラッシュを入力する
(setq locale-coding-system 'utf-8) ;; ansi-termでの日本語の設定

;; anythingの設定
(require 'anything-startup)
(require 'anything-match-plugin)
(setq anything-quick-update t) ;; 候補が多い時に体感速度を早くする
(setq anything-enable-shortcuts 'alphabet) ;; 候補選択ショートカット
;; anythingのprefixキーを変更する（デフォルトF5 a）
(custom-set-variables '(anything-command-map-prefix-key " a"))
;; anythingの画面の表示方法を変更する
(define-key anything-map (kbd "M-n") 'anything-next-source)
(define-key anything-map (kbd "M-p") 'anything-previous-source)
(defun anything-split-window (buf)
  (split-window)
  (other-window 1)
  (switch-to-buffer buf))
(setq anything-display-function 'anything-split-window)
;; 最近使ったファイルの保存数を増やす
(when (require 'recent nil t)
  (custum-set-variables '(recentf-save-file "~/.emacs.d/recent"))
  (setq recentf-max-saved-items 3000)
  (recentf-mode 1))

;; popwinの設定
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; auto-completeの設定
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; 色（theme）の設定
(require 'color-theme)
(color-theme-initialize)
(color-theme-classic)

;; SLIMEの設定
(setq inferior-lisp-program "/usr/local/bin/sbcl") ;; SBCLをCommon Lisp処理系に設定
;(setq inferior-lisp-program "/usr/local/bin/clisp") ;; CLISPをCommon Lisp処理系に設定
;(setq inferior-lisp-program "/usr/local/bin/abcl-no-rlwrap") ;; ABCLをCommon Lisp処理系に設定
;(setq inferior-lisp-program "/usr/local/bin/ccl") ;; CCLをCommon Lisp処理系に設定
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner)) ;; SLIMEのロード

;; popwinのSLIME設定
(push '("*slime-apropos*") popwin:special-display-config)
(push '("*slime-macroexpansion*") popwin:special-display-config)
(push '("*slime-description*") popwin:special-display-config)
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
(push '("*slime-xref*") popwin:special-display-config)
(push '(sldb-mode :stick t) popwin:special-display-config)
(push '(slime-repl-mode) popwin:special-display-config)
(push '(slime-connection-list-mode) popwin:special-display-config)

;; ac-slimeのロード
(require 'ac-slime)
;; ac-slimeの設定
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; scala-modeの設定
(setq scala-interpreter "/usr/local/bin/scala") ;; scalaの処理系を設定
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
	  (lambda ()
	    (scala-electric-mode)))

;; clojure-modeの設定
;; Emacs 23ではpackage.elを事前時インストールする必要あり
;; （Emacs 24ではpackage.elは組み込まれている）
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)
