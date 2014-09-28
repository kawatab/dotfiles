(load-theme 'wombat) ;; wombat prefer italic font

;; Initial window configuration
(if window-system
    (progn
      (custom-set-faces
       '(default ((t (:background "black"
		      :family "DejaVu Sans Mono"
		      :foundry "unknown"
		      :slant normal
		      :weight bold
		      :height 98
		      :width normal)))))
      (split-window-horizontally)
      (custom-set-variables
       '(default-frame-alist (append (list '(width .  164)
					   '(height . 60)
					   '(alpha 90 90))
				     default-frame-alist))))
  (progn
    (custom-set-faces
     '(default ((t (:background "black")))))
    (menu-bar-mode 0)))

;; package management
(require 'package)
;; (add-to-list 'package-archives
;; '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
   '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defun package-install-with-refresh (package)
  (unless (assq package package-alist)
    (package-refresh-contents))
  (unless (package-installed-p package)
    (package-install package)))

(package-install-with-refresh 'evil)
(package-install-with-refresh 'geiser)
(package-install-with-refresh 'git-gutter)
(package-install-with-refresh 'magit)
(package-install-with-refresh 'helm)
(package-install-with-refresh 'ggtags)
(package-install-with-refresh 'yasnippet)

;; require for using 'loop'
(require 'cl)

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
 '(explicit-shell-file-name "/bin/zsh") ;; set default shell
 '(geiser-active-implementations (quote (racket)))) ;; set Racket as Default

;; Japanese input environment
(require 'mozc)
(custom-set-variables
 '(default-input-method "japanese-mozc")
 '(mozc-keymap-tsuki mozc-keymap-tsuki-101us))

;; enable git-gutter mode
(global-git-gutter-mode t)

;; enable helm
(helm-mode 1)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; CEDET
(global-ede-mode 1)
(semantic-mode 1)

;;; enable
;; Use setq instead of custom-set-variables.
 (setq evil-cross-lines 'whichwrap
       evil-want-C-u-scroll t
       evil-want-C-w-delete nil
       evil-want-C-w-in-emacs-state t
       evil-search-module 'evil-search
       evil-ex-search-vim-style-regexp t
       evil-ex-search-case 'sensitive
       evil-esc-delay 0)
;; Put require after setting variables
(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map "\C-h" 'delete-backward-char)
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(loop for (mode . state) in '((bc-menu-mode . emacs)
			      (comint-mode . normal)
			      (dired-mode . emacs)
			      (ggtags-global-mode . emacs)
			      (git-commit-mode . insert)
			      (git-rebase-mode . emacs)
			      (grep-mode . emacs)
			      (help-mode . emacs)
			      (helm-grep-mode . emacs)
			      (inferior-emacs-lisp-mode . emacs)
			      (magit-branch-manager-mode . emacs)
			      (nrepl-mode . insert)
			      (pylookup-mode . emacs)
			      (rdictcc-buffer-mode . emacs)
			      (shell-mode . insert)
			      (term-mode . emacs)
			      (undo-tree-mode . emacs)
			      (wdired-mode . normal))
      do (evil-set-initial-state mode state))

(add-hook 'after-init-hook  (lambda() (eshell)))
