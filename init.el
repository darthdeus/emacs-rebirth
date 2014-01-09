;; Saner emacs defaults
(setq ring-bell-function 'ignore) ; Disable the visual bell
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
;;(set-face-attribute 'default nil :height 160)

;; path settings
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
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
				   flx flx-ido ido-ubiquitous smex)
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

(tool-bar-mode 0)

;; Smarter completion for M-x (ido style, but also msart)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Projectile config
(projectile-global-mode)
(setq projectile-enable-caching t)


;; TODO - check auto-fill mode

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO - check what this actually does. how does it change the original apropos search?
;(setq apropos-do-all t)


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
; watch emacs screencasts http://emacsrocks.com/

; My old config https://gist.github.com/darthdeus/d645f9e87db4cb8a9721


; http://code.org/teach
