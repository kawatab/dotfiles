;; emacs directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; package management
(require 'package)
(add-to-list 'package-archives
;; '("marmalade" . "http://marmalade-repo.org/packages/"))
   '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defun package-install-with-refresh (package)
(unless (assq package package-alist)
(package-refresh-contents))
(unless (package-installed-p package)
(package-install package)))

;; Initial window configuration
(when window-system
(split-window-horizontally)
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(setq default-frame-alist (append (list '(width .  164)
                                        '(height . 60))
                                  default-frame-alist)))

;; Don't display startup message
(setq inhibit-startup-message t)
;; (ansi-term "/bin/zsh")
(add-hook 'after-init-hook  (lambda() (eshell)))

;; install evil
(package-install-with-refresh 'evil)

;; enable evil
(require 'evil)
(evil-mode 1)

;; install geiser
(package-install-with-refresh 'geiser)

;; set Racket as Default
(setq geiser-active-implementations '(racket))

;; install git-gutter
(package-install-with-refresh 'git-gutter)

;; enable git-gutter mode
(global-git-gutter-mode t)

;; install git-gutter
(package-install-with-refresh 'magit)

;; Other settings
(load-theme 'wombat)
(show-paren-mode t) ;; show parenthesis
(setq make-backup-files nil) ;; don't make backup file
(setq delete-auto-save-files t) ;; delete autosave file when exiting
