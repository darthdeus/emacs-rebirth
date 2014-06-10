;; Saner emacs defaults
(setq ring-bell-function 'ignore) ; Disable the visual bell
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(set-face-attribute 'default nil :height 120)

;; path settings
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/darth/.cabal/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/darth/.cabal/bin")))
(setq eshell-path-env "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin")

(setq initial-frame-alist '((top . 140) (left . 220) (width . 110) (height . 40)))

;; PACKAGE CONFIG
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

; Apparently needed for the package auto-complete (why?)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(haskell-mode sequential-command rainbow-delimiters projectile grizzl yaml-mode
				   flx flx-ido ido-ubiquitous auto-complete paredit undo-tree)
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
(global-undo-tree-mode 1)
(global-auto-complete-mode 1)

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

(require 'dirtree)

(require 'yasnippet)
(yas-global-mode 1)

;; EVIL MODE
(require 'evil)
(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)

(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)

(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

(add-to-list 'evil-insert-state-modes 'inferior-haskell-mode)
(evil-mode 1)

(blink-cursor-mode 0)

; TODO - make this work
; (define-key key-translation-map (kbd ",f") (kbd "C-c p f"))

;(add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; TODO - check auto-fill mode

; check why this doesn't work
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)
      (turn-on-haskell-indentation)
;	    (flymake-mode)
;	    (haskell-doc-mode 1)
;           (haskell-indent-mode 1)
;           (haskell-indent-mode 0)
;            (haskell-simple-indent-mode 1)
;	    (structured-haskell-mode 1)
           ; (ac-haskell-mode-setup)
            ;; This is only needed when ghc-mod is loaded
            (global-set-key (kbd "s-s") 'ghc-save-buffer)
	    ))

(setq ghc-display-error 'minibuffer)

(defun ac-haskell-mode-setup ()
  (setq ac-sources '(ac-source-ghc-mod)))


(setq ghc-hlint-options '("-fno-warn-unused-do-bind"))

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;(require 'flymake-haskell-multi)
;(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

;; TODO - check what this actually does. how does it change the original apropos search?
;(setq apropos-do-all t)

; use display-graphic-p instead of window-system

;(require 'rcodetools)
;(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes (quote ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "c2cfe2f1440d9ef4bfd3ef4cf15bfe35ff40e6d431264b1e24af64f145cffb11" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" default)))
 '(haskell-indent-spaces 4)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#282a2e"))))
 '(ghc-face-error ((t (:underline "gray36"))))
 '(ghc-face-warn ((t (:underline "DarkGoldenrod4"))))
 '(mode-line ((t (:background "#282a2e" :box (:line-width 1 :color "gray21") :weight normal))))
 '(mode-line-inactive ((t (:background "#282a2e" :box (:line-width 1 :color "gray21") :weight normal)))))
