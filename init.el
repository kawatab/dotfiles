;; ;; emacs directory
;; (when load-file-name
  ;; (setq user-emacs-directory (file-name-directory load-file-name)))

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

;; Initial window configuration for GUI
(if window-system
    (progn
      (split-window-horizontally)
      (setq truncate-lines nil)
      (setq truncate-partial-width-windows nil)

      (setq default-frame-alist (append (list '(width .  164)
					      '(height . 60))
					default-frame-alist)))
  (menu-bar-mode 0))

(setq inhibit-startup-message t) ;; Don't display startup message
)
;; (ansi-term "/bin/zsh")
(add-hook 'after-init-hook  (lambda() (eshell)))

;; install evil
(package-install-with-refresh 'evil)

;; enable evil
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map "\C-h" 'delete-backward-char)

;; install geiser
(package-install-with-refresh 'geiser)

;; set Racket as Default
(setq geiser-active-implementations '(racket))

;; install git-gutter
(package-install-with-refresh 'git-gutter)

;; enable git-gutter mode
(global-git-gutter-mode t)

;; install magit
(package-install-with-refresh 'magit)

;; install helm
(package-install-with-refresh 'helm)
(helm-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)


;; install yasnippet
(package-install-with-refresh 'yasnippet)

;; CEDET
(global-ede-mode 1)
(semantic-mode 1)

;; Evil or emacs
(require 'cl)
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
			      (nrepl-mode . insert)
			      (pylookup-mode . emacs)
			      (comint-mode . normal)
			      (shell-mode . insert)
			      (git-commit-mode . insert)
			      (git-rebase-mode . emacs)
			      (term-mode . emacs)
			      (help-mode . emacs)
			      (helm-grep-mode . emacs)
			      (grep-mode . emacs)
			      (bc-menu-mode . emacs)
			      (magit-branch-manager-mode . emacs)
			      (rdictcc-buffer-mode . emacs)
			      (dired-mode . emacs)
			      (wdired-mode . normal))
      do (evil-set-initial-state mode state))

;; Other settings
(load-theme 'wombat)
(show-paren-mode t) ;; show parenthesis
(setq make-backup-files nil) ;; don't make backup file
(setq delete-auto-save-files t) ;; delete autosave file when exiting
