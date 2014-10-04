(load "~/.emacs.d/init_appearance.el")
(load "~/.emacs.d/init_package.el")

;; emacs directory
(when load-file-name
  (custom-set-variables '(user-emacs-directory (file-name-directory load-file-name))))

(custom-set-variables
 '(inhibit-startup-screen t)
 '(delete-auto-save-files t)
 '(make-backup-files nil)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(show-paren-mode t)
 '(eshell-history-size 10000)
 '(explicit-shell-file-name "/bin/zsh") ;; set default shell
 '(geiser-active-implementations (quote (racket)))) ;; set Racket as Default

;; (load "~/.emacs.d/init_auto-complete.el")
(load "~/.emacs.d/init_mozc.el")
(load "~/.emacs.d/init_git-gutter.el")
(load "~/.emacs.d/init_helm.el")
(load "~/.emacs.d/init_recentf.el")
(load "~/.emacs.d/init_evil.el")
(load "~/.emacs.d/init_eshell.el")
;; (load "~/.emacs.d/init_gtags.el")

;; key combination
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key evil-insert-state-map "\C-h" 'delete-backward-char)

;; key maps
(require 'cl)
(loop for (mode . state) in '((bc-menu-mode . emacs)
			      (comint-mode . normal)
			      (dired-mode . emacs)
			      (ebrowse-tree-mode . emacs)
			      (git-commit-mode . insert)
			      (git-rebase-mode . emacs)
			      (grep-mode . emacs)
			      (help-mode . emacs)
			      (helm-grep-mode . emacs)
			      (inferior-emacs-lisp-mode . emacs)
			      (magit-branch-manager-mode . emacs)
			      (Man-mode . normal)
			      (nrepl-mode . insert)
			      (pylookup-mode . emacs)
			      (rdictcc-buffer-mode . emacs)
			      (shell-mode . insert)
			      (term-mode . emacs)
			      (undo-tree-mode . emacs)
			      (wdired-mode . normal))
      do (evil-set-initial-state mode state))

;; start up
(add-hook 'after-init-hook (lambda() (eshell)))
