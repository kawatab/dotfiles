;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appearance 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
					   ;; '(alpha 90 90)
					   )
				     default-frame-alist))))
  (progn
    (custom-set-faces
     '(default ((t (:background "black")))))
    (menu-bar-mode 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(package-install-with-refresh 'auto-complete)
(package-install-with-refresh 'evil)
(package-install-with-refresh 'geiser)
(package-install-with-refresh 'git-gutter)
(package-install-with-refresh 'magit)
(package-install-with-refresh 'helm)
;; (package-install-with-refresh 'ggtags)
(package-install-with-refresh 'vimrc-mode)
(package-install-with-refresh 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'auto-complete-config)
;; (ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozc settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mozc)
(custom-set-variables
 '(default-input-method "japanese-mozc")
 '(mozc-keymap-tsuki mozc-keymap-tsuki-101us))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git-gutter settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-git-gutter-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(helm-mode 1)
;; (setq helm-ff-auto-update-initial-value nil)
(add-to-list 'helm-completing-read-handlers-alist '(dired . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . nil))
(add-to-list 'helm-completing-read-handlers-alist '(execute-extended-command . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-tag-noselect . nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recentf settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(recentf-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; evil settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vi
(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
    \"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	(let* ((line (string-to-number (match-string 1 (pop args))))
	       (file (pop args)))
	  (find-file file)
	  (goto-line line))
      (find-file (pop args)))))

;; less
(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
    the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	(let* ((line (string-to-number (match-string 1 (pop args))))
	       (file (pop args)))
	  (view-file file)
	  (goto-line line))
      (view-file (pop args)))))

;; more
(defalias 'eshell/more 'eshell/less)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gtags settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'gtags-select-mode-hook
;; 	  '(lambda ()
;; 	     (setq hl-line-face 'underline)
;; 	     (hl-line-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key combination
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key minibuffer-local-map "\C-h" 'delete-backward-char)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
(define-key evil-ex-search-keymap (kbd "C-h") 'delete-backward-char)
(define-key evil-ex-completion-map (kbd "C-h") 'delete-backward-char)

;; key maps
(require 'cl)
(loop for (mode . state) in '((bc-menu-mode . emacs)
			      (comint-mode . normal)
			      ;; (dired-mode . emacs)
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

;; Major mode settings
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (modify-syntax-entry ?_ "w")
	    (gtags-mode 1)))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?_ "w")
	    (gtags-mode 1)))

(add-hook 'javascript-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?_ "w")))

(add-hook 'vimrc-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?_ "w")))

;; start up
(add-hook 'after-init-hook
	  (lambda()
	    (eshell)))
