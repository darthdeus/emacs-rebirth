;; Saner emacs defaults
(setq ring-bell-function 'ignore) ; Disable the visual bell
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
;(set-face-attribute 'default nil :height 160)
(set-face-attribute 'default nil :height 120)
;(set-face-attribute 'default nil :height 180)

;; Simple helper to interactively set font size
(defun set-font-size (size) (set-face-attribute 'default nil :height size))

;; path settings
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/darth/.cabal/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/darth/.cabal/bin")))
(setq eshell-path-env "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin")

(setq initial-frame-alist '((top . 140) (left . 220) (width . 110) (height . 40)))

;; PACKAGE CONFIG
(require 'package)
;(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

; Apparently needed for the package auto-complete (why?)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(haskell-mode sequential-command rainbow-delimiters projectile grizzl yaml-mode smex
				   flx flx-ido ido-ubiquitous auto-complete paredit undo-tree ack-and-a-half color-theme-sanityinc-tomorrow
                                   dirtree ghc gist magit markdown-mode scss-mode slim-mode evil evil-surround yasnippet)
  "A list of packages installed at launch")

;; Automatically install a pre-defined list of packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; TODO - check if this is always enabled
(require 'rainbow-delimiters)

;; TODO - check how to enable ido for M-x
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous 1)
(flx-ido-mode 1)

; TODO - enable this later
(global-undo-tree-mode 0)
(global-auto-complete-mode 1)
(blink-cursor-mode 0)

(when window-system (scroll-bar-mode 0))
(when tool-bar-mode (tool-bar-mode 0))
(when window-system (menu-bar-mode 0))
(when (not (window-system)) (menu-bar-mode 0))

;; Smarter completion for M-x (ido style, but also msart)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c m s") 'magit-status)

;; Projectile config
(projectile-global-mode)
(setq projectile-enable-caching t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'markdown-mode-hook '(lambda ()
                                 (flyspell-mode 1)
                                 (auto-fill-mode 1)))

(require 'dirtree)

(require 'yasnippet)
(yas-global-mode 1)

;; EVIL MODE
(require 'evil)
(require 'evil-surround)
(evil-mode 1)
(global-evil-surround-mode 1)

(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd "q") nil)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-x h k") 'describe-key)

(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

(define-key evil-motion-state-map (kbd "C-e") nil)

(add-to-list 'evil-insert-state-modes 'inferior-haskell-mode)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)

(blink-cursor-mode 0)

; check why this doesn't work
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)
            (turn-on-haskell-indentation)
            ;; This is only needed when ghc-mod is loaded
            (global-set-key (kbd "s-s") 'ghc-save-buffer)
	    ))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "<f12>") 'haskell-process-reload-devel-main)

(define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;;; PureScript cheat mode
(define-derived-mode purescript-mode haskell-mode "PureScript"
  "Major mode for PureScript")
(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))


; figure out the name of this '(haskell-process-suggest-hoogle-imports f)
  ; '(haskell-process-suggest-remove-import-lines f)
  ;'(haskell-process-auto-import-loaded-modules t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" default)))
 '(haskell-indent-spaces 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(shell-file-name "/bin/bash")
 '(truncate-lines nil))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; Temporarily disabled since ghc-mod provides the same functionality but in a better way
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)

;; (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)




(setq ghc-display-error 'minibuffer)

(defun ac-haskell-mode-setup ()
  (setq ac-sources '(ac-source-ghc-mod)))


(setq ghc-hlint-options '("-fno-warn-unused-do-bind"))

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; TODO - check what this actually does. how does it change the original apropos search?
;(setq apropos-do-all t)

; use display-graphic-p instead of window-system


;; Packages and other emacs things that I want to check out
; http://www.emacswiki.org/emacs/SequentialCommand
; http://www.emacswiki.org/emacs/sequential-command.el
; http://www.emacswiki.org/emacs/sequential-command-config.el
; SICP video course - http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/
; http://ergoemacs.org/emacs/emacs_modernization.html
; http://ergoemacs.org/emacs/elisp.html
; https://www.gnu.org/software/emacs/manual/html_node/eintr/
; http://bzg.fr/learn-emacs-lisp-in-15-minutes.html
; http://www.haskell.org/haskellwiki/Emacs/Keybindings_and_simple_usage
; http://haskell.github.io/haskell-mode/manual/latest/
; http://www.haskell.org/haskellwiki/Emacs/Inferior_Haskell_processes
; http://www.emacswiki.org/emacs/MacOSTweaks
; http://paste.lisp.org/display/132476
; http://www.gnu.org/software/emacs/manual/html_node/eintr/index.html
; What is smex and how it helps me? http://www.emacswiki.org/emacs/Smex
; Setting proper frame size when emacs starts http://www.emacswiki.org/emacs/FrameSize#toc1
;   Maybe also check better settings for default font size (and possibly keybinding zoom in/out ?)
; !!! Yes or No p http://www.emacswiki.org/emacs/YesOrNoP
; IDO http://www.emacswiki.org/emacs/InteractivelyDoThings
; more ido http://www.emacswiki.org/emacs/InteractivelyDoThings#toc8
; ido hacks https://github.com/scottjad/ido-hacks/blob/master/ido-hacks.el
; watch emacs screencasts http://emacsrocks.com/
; http://stackoverflow.com/questions/14836958/updating-packages-in-emacs
; http://ergoemacs.org/emacs/emacs_package_system.html
; xmpfilter in VIM http://justincampbell.me/til/annotate-ruby-code-in-vim-with-xmpfilter
; melpa http://melpa.milkbox.net/#/getting-started
; check out el-get vs elpa vs melpa vs marmelade
; http://www.gnu.org/software/emacs/tour/
; gnu hurd - check it out, see what it does, can it be used? how?
;   http://www.gnu.org/software/hurd/hurd.html
;   http://www.gnu.org/software/hurd/hurd/what_is_the_gnu_hurd.html
;   http://thread.gmane.org/gmane.os.hurd.bugs/18777
;   http://darnassus.sceen.net/gitweb/hurd-web.git/commitdiff/ae16fea583613b94785c262b764a1b68cf722ded
; PRO emacs config http://www.cyber.com.au/~twb/.emacs
; recursive editing levels http://www.gnu.org/software/emacs/manual/html_node/emacs/Recursive-Edit.html
; C-h i
; check out gnu homepage for some more cool stuff http://www.gnu.org/home.en.html
; emacs as word processor - read the whole thread https://lists.gnu.org/archive/html/emacs-devel/2013-11/msg00594.html

; My old config https://gist.github.com/darthdeus/d645f9e87db4cb8a9721

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#282a2e"))))
 '(ghc-face-error ((t (:underline "gray36"))))
 '(ghc-face-warn ((t (:underline "DarkGoldenrod4"))))
 '(mode-line ((t (:background "#282a2e" :foreground "gray39" :box (:line-width 2 :color "#282a2e") :weight normal))))
 '(mode-line-buffer-id ((t (:foreground "#655969"))))
 '(mode-line-highlight ((t (:foreground "#655969" :box nil :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#282a2e" :foreground "gray39" :box (:line-width 2 :color "#282a2e") :weight normal)))))
