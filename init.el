;;; init.el --- sjas .emacs initialization file

;;; Commentary:
;;; no info yet.
;;; 1/2015

;;; Code:

(setq package-archives
      '(
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ))
(package-initialize)

(defvar package-list)
(setq package-list
      '(
	2048-game
	color-theme
	cc-mode
	org
	evil
	evil-indent-textobject
	evil-leader
	evil-matchit
	evil-nerd-commenter
	evil-numbers
	evil-paredit
	evil-jumper
	golden-ratio
	helm
	helm-dash
	helm-dirset
	helm-git
	helm-descbinds
	helm-projectile
	markdown-mode
	markdown-mode+
	markdown-toc
	powerline-evil
	auto-complete
	key-chord
	magit
	paredit
	parscope
	rainbow-delimiters
	rainbow-identifiers
	rainbow-mode
	use-package
	vimrc-mode
	solarized-theme
	sos
	spray
	zenburn-theme
	hc-zenburn-theme
	grandshell-theme
	private
	ps-ccrypt
	prodigy
	puppet-mode
	quickrun
	elisp-slime-nav
	ac-helm
	ascii
	bash-completion
	color-theme-wombat
	color-theme-monokai
	color-theme-molokai
	company
	free-keys
	flyparens
	flylisp
	rfringe
	vi-tilde-fringe
	eshell-fringe-status
	fringe-current-line
	flycheck
	flycheck-clojure
	flycheck-irony
	flycheck-package
	flycheck-pos-tip
	flycheck-pyflakes
	flycheck-tip
	flymake
	flymake-easy
	flymake-shell
	flymake-cursor
	flymake-puppet
	python-mode
	tangotango-theme
	))

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
(require 'package)

;(custom-set-variables
; '(initial-frame-alist (quote ((fullscreen . maximized)))))

(defvar color-theme-is-global)
(setq color-theme-is-global t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("49e5a7955b853f70d1fe751b2f896921398b273aa62f47bda961a45f80219581" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

					;(load-theme 'hc-zenburn)
					;(load-theme 'manoj-dark)
					;(load-theme 'wombat)
					;(color-theme-molokai)
;; (color-theme-monokai)
(load-theme 'tangotango t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(ac-config-default)


(global-hl-line-mode) ;; cursorline
(show-paren-mode) ;; show matching parens
(electric-pair-mode) ;; insert closing parens at once
(eldoc-mode)
(column-number-mode 1) ;; show column
;(global-prettify-symbols-mode 1) ;; lambda as shown as symbol

(global-set-key (kbd "C-x C-l") 'eval-buffer)
(global-set-key (kbd "C-h C-f") 'find-function)

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-o") 'list-packages)

					;(global-set-key (kbd "C-x C-b")
					;		(lambda () (interactive)
					;		  (progn
					;		    (buffer-menu)
					;		    (other-window))))

					;(global-set-key (kbd "C-j") 'next-buffer)
					;(global-set-key (kbd "C-j") 'previous-buffer)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(helm-descbinds-mode)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(defvar helm-google-suggest-use-curl-p)
(defvar helm-ff-search-library-in-sexp)
(defvar helm-ff-file-name-history-use-recentf)
(defvar helm-M-x-fuzzy-match)
(defvar helm-buffers-fuzzy-matching)
(defvar helm-recentf-fuzzy-match)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
(helm-autoresize-mode t)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(semantic-mode 1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

					;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
					;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
					;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(recentf-mode 1)
(defvar recentf-auto-cleanup)
(setq recentf-auto-cleanup 'never)
(global-set-key (kbd "C-x C-g") 'recentf-open-files)
					;(global-set-key "\C-xf" 'recentf-open-files)

(golden-ratio-mode 0)
(defvar golden-ratio-inhibit-functions)
(defun pl/helm-alive-p ()
  "Deactive golden ratio when helm is in use iirc."
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p
	   (message "File '%s' successfully renamed to '%s'"
		    name (file-name-nondirectory new-name))))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun move-line-down ()
  "Move current line down and set focus there, too."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(defun move-line-up ()
  "Move current line up and set focus there, too."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (transpose-lines 1))
    (forward-line -1)
    (move-to-column col)))
(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<M-up>") 'move-line-up)


;;; full screen magit-status
					;(defadvice magit-status (around magit-fullscreen activate)
					;  (window-configuration-to-register :magit-fullscreen)
					;  ad-do-it
					;  (delete-other-windows))
					;(defun magit-quit-session ()
					;  "Restores the previous window configuration and kills the magit buffer"
					;  (interactive)
					;  (kill-buffer)
					;  (jump-to-register :magit-fullscreen))
					;(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (forward-line (read-number "Goto line: ")))
    (linum-mode -1)))

					;(defadvice save-buffer (around save-buffer-as-root-around activate)
					;  "Use sudo to save the current buffer."
					;  (interactive "p")
					;  (if (and (buffer-file-name) (not (file-writable-p (buffer-file-name))))
					;      (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
					;	ad-do-it)
					;    ad-do-it))

(defadvice find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-lisp-hook ()
  "Helper function so elisp has proper modes loaded."
  (elisp-slime-nav-mode)
  (eldoc-mode))

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader "SPC")
(defun volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))
(evil-leader/set-key "fw" 'save-buffer)
(evil-leader/set-key "fq" 'kill-buffer)
(evil-leader/set-key "rq" 'volatile-kill-buffer)
(evil-leader/set-key "fe" 'recentf-open-files)
(evil-leader/set-key "ff" 'find-file)
(evil-leader/set-key "1" 'delete-other-windows)
(evil-leader/set-key "2" 'split-window-below)
(evil-leader/set-key "3" 'split-window-right)
(evil-leader/set-key "0" 'delete-window)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "n" 'next-buffer)
(evil-leader/set-key "v" 'previous-buffer)
(evil-leader/set-key "DEL" 'keyboard-quit)
(evil-leader/set-key "c" 'evilnc-comment-or-uncomment-lines)
(evil-leader/set-key "fs" (lambda () (interactive) (insert (concat "date: " (shell-command-to-string "date --rfc-3339=seconds | cut -c1-19")))))

;; window management for professionals
(global-set-key (kbd "<C-tab>") 'other-window)
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-j") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-k") (lambda () (interactive) (other-window -1)))
(evil-leader/set-key "-" 'minimize-window)
(evil-leader/set-key "=" 'maximize-window)
(evil-leader/set-key "SPC" 'balance-windows)

(evil-leader/set-key "sv"
  (lambda () (interactive)
    (progn
      (save-buffer)
      (eval-buffer))))
(evil-leader/set-key "wq"
  (lambda () (interactive)
    (progn
      (save-buffer)
      (kill-buffer))))
 
(defun toggle-window-split ()
  "If two frames are present, toggle horizontal and vertical splitting of them."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(evil-leader/set-key "RET" 'toggle-window-split)

(evil-mode 1)

(evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
  'elisp-slime-nav-describe-elisp-thing-at-point)

(key-chord-mode 1)
(key-chord-define-global "jk" 'evil-normal-state)

(add-hook 'after-init-hook 'global-company-mode)
(auto-complete-mode t)

;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;; 					; Make sure it's not a remote buffer or flymake would not work
;;     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
;;       (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;              (local-file (file-relative-name
;;                           temp-file
;;                           (file-name-directory buffer-file-name))))
;;         (list "pyflakes" (list local-file)))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; (eval-after-load 'flycheck
;;   '(add-to-list 'flycheck-checkers 'irony))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)
(defvar flycheck-mode-map)
(defvar flycheck-keymap-prefix)
(defvar flycheck-command-map)
(define-key flycheck-mode-map flycheck-keymap-prefix (kbd "C-c f"))
(define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

(evil-leader/set-key "j" 'flycheck-next-error)
(evil-leader/set-key "k" 'flycheck-previous-error)
(evil-leader/set-key "l" 'flycheck-list-errors)

(provide 'init)
;;; init.el ends here
