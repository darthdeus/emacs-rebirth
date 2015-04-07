;; Saner emacs defaults
(setq ring-bell-function 'ignore) ; Disable the visual bell
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)

;(set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :height 140)
;(set-face-attribute 'default nil :height 160)
;(set-face-attribute 'default nil :height 180)

(when window-system (scroll-bar-mode 0))
(when tool-bar-mode (tool-bar-mode 0))
(when window-system (menu-bar-mode 0))
(when (not (window-system)) (menu-bar-mode 0))

;; Simple helper to interactively set font size
(defun set-font-size (size) (set-face-attribute 'default nil :height size))

;; path settings
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/darth/.cabal/bin:/Applications/ghc-7.8.3.app/Contents/bin/ghc"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/darth/.cabal/bin")))
(setq exec-path (append exec-path '("/Applications/ghc-7.8.3.app/Contents/bin/ghc")))
(setq eshell-path-env "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin")

;; Default window position
(setq initial-frame-alist '((top . 90) (left . 180) (width . 170) (height . 60)))

;; Compatibility package
(require 'cl)

;; PACKAGE CONFIG
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar packages '(haskell-mode))

(loop for name in packages
      do (progn (unless (fboundp name)
                  (add-to-list 'load-path
                               (concat (file-name-directory (or load-file-name (buffer-file-name)))
                                       "packages/"
                                       (symbol-name name)))
                  (require name))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(sequential-command clojure-mode cider rainbow-delimiters
  projectile grizzl yaml-mode smex flx flx-ido ido-ubiquitous paredit
  undo-tree ack-and-a-half company color-theme-sanityinc-tomorrow
  dirtree ghc gist magit markdown-mode scss-mode slim-mode evil
  evil-surround yasnippet web-mode hindent hi2)
  "A list of packages installed at launch")

;; Automatically install a pre-defined list of packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'haskell-mode)
(load "haskell-mode-autoloads.el")

;; TODO - check how to enable ido for M-x
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous 1)
(flx-ido-mode 1)

;; blinking is annoying
(blink-cursor-mode 0)

(global-undo-tree-mode 0)
(global-company-mode)

;; Lisp settings
(setq inferior-lisp-program "/usr/local/bin/clisp")
(setq slime-contribs '(slime-fancy))

(add-hook 'racket-mode-hook #'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'evil-paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)

;; TODO - try to use this instead of paredit
; (require 'evil-smartparens)
; (add-hook 'clojure-mode-hook #'evil-smartparens-mode)
; (require 'smartparens-config)
; (smartparens-global-mode 1)

;; Coq - temporarily disabled to improve startup time
; (load-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

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
(global-set-key (kbd "s-e") 'eval-region)
(global-set-key (kbd "s-/") 'comment-region)
(global-set-key (kbd "s-?") 'uncomment-region)
(global-set-key (kbd "C-`") 'haskell-interactive-bring)

;; Projectile config
(projectile-global-mode)
(setq projectile-enable-caching t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'markdown-mode-hook '(lambda ()
                                 (flyspell-mode 1)
                                 (auto-fill-mode 1)))

(add-hook 'speedbar-mode-hook '(lambda ()
                                 (local-set-key (kbd "C-l") 'evil-window-right)
                                 (local-set-key (kbd "C-h") 'evil-window-left)
                                 (local-set-key (kbd "C-j") 'evil-window-down)
                                 (local-set-key (kbd "C-k") 'evil-window-up)))

(require 'yasnippet)
(yas-load-directory "~/.emacs.d/snippets" t)
(yas-global-mode 1)

;; EVIL MODE
(require 'evil)
(require 'evil-surround)
(evil-mode 1)
(global-evil-surround-mode 1)

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-normal-state-map (kbd "q") nil)

(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

(define-key evil-visual-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-c") 'evil-exit-visual-state)

(define-key evil-motion-state-map (kbd "C-e") nil)

;; Switching between windows with C-hjkl
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-x h k") 'describe-key)

;; Insert mode as well
(define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-insert-state-map (kbd "C-x h k") 'describe-key)
(define-key evil-insert-state-map (kbd "C-x C-k C-k") 'kill-line)

(add-to-list 'evil-insert-state-modes 'inferior-haskell-mode)

(require 'hi2)
; check why this doesn't work
 (autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)
            ;; (turn-on-haskell-indentation)
            (turn-on-hi2)
            ;; This is only needed when ghc-mod is loaded
            (global-set-key (kbd "s-s") 'ghc-save-buffer)

            (define-key evil-normal-state-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
            (define-key evil-insert-state-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

(setq haskell-tags-on-save t)

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "<f5>") 'neotree-toggle)
(global-set-key (kbd "<f12>") 'haskell-process-reload-devel-main)

(define-key haskell-mode-map [f8] 'haskell-navigate-imports)

; figure out the name of this '(haskell-process-suggest-hoogle-imports f)
'(haskell-process-suggest-remove-import-lines f)
'(haskell-process-auto-import-loaded-modules t)


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
(define-key haskell-mode-map (kbd "C-c i") 'hindent/reformat-decl)

;; (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

(setq ghc-display-error 'minibuffer)
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
 '(idris-loaded-region-face ((t (:background "gray8"))))
 '(mode-line ((t (:background "#282a2e" :foreground "gray39" :box (:line-width 2 :color "#282a2e") :weight normal))))
 '(mode-line-buffer-id ((t (:foreground "#655969"))))
 '(mode-line-highlight ((t (:foreground "#655969" :box nil :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#282a2e" :foreground "gray39" :box (:line-width 2 :color "#282a2e") :weight normal))))
 '(neo-button-face ((t (:foreground "SlateGray3" :underline nil))))
 '(neo-dir-link-face ((t (:foreground "cornflower blue"))))
 '(neo-file-link-face ((t (:foreground "gray83"))))
 '(neo-root-dir-face ((t (:foreground "SlateGray3" :weight bold))))
 '(shm-current-face ((t (:background "#efefef"))))
 '(shm-quarantine-face ((t (:inherit font-lock-error)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-load-path (quote ("src")))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(evil-shift-width 4)
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-popup-errors nil)
 '(haskell-mode-contextual-import-completion nil)
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-log t)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(hindent-style "gibiansky")
 '(neo-theme (quote arrow))
 '(safe-local-variable-values
   (quote
    ((haskell-indent-spaces . 4)
     (hamlet/basic-offset . 4)
     (haskell-process-use-ghci . t)
     (haskell-process-type . cabal-repl))))
 '(shell-file-name "/bin/bash")
 '(shm-auto-insert-bangs t)
 '(shm-auto-insert-skeletons t)
 '(shm-use-presentation-mode t)
 '(truncate-lines nil))
