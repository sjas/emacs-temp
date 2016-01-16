;;; Init.el --- sjas .emacs initialization file rebuild

;;; TODO
;;;  * do final cleanup
;;;  * learn orgmode
;;;  * check earlier used packages:

;;	color-theme
;;	cc-mode
;;	golden-ratio
;;	paredit
;;	parscope
;;	use-package
;;	spray
;;	private
;;	ps-ccrypt
;;	quickrun
;;	ascii
;;	python-mode

;; package installment
(require 'cl)
(require 'package)
(setq package-archives
      '(
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "https://melpa.milkbox.net/packages/")
	("elpa" . "http://tromey.com/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")
	))
(package-initialize)

(message "----- DEBUG: package universes initialized.")

(setq my-package-list
      '(
	elisp-slime-nav
	key-chord
	magit
	helm
	helm-git
	helm-dash
	helm-dirset
	helm-descbinds
	helm-projectile
	evil
	evil-leader
	evil-matchit
	evil-numbers
	evil-paredit
	evil-jumper
	evil-nerd-commenter
	powerline-evil
	vimrc-mode
	rainbow-identifiers
	color-theme-monokai
	color-theme-molokai
	rainbow-delimiters
	rainbow-mode
	solarized-theme
	zenburn-theme
	hc-zenburn-theme
	grandshell-theme
	color-theme-wombat
	tangotango-theme
	slime
	slime-company
	;;slime-fuzzy
	;;slime-repl
	markdown-mode
	markdown-mode+
	markdown-toc
	markdown-preview-eww
	bash-completion
	org
	free-keys
	vi-tilde-fringe
	rfringe
	eshell-fringe-status
	fringe-current-line
	fringe-helper
	flylisp
	;;flyparens ;; BROKEN
	flycheck
	flycheck-checkbashisms
	flycheck-color-mode-line
	flycheck-irony
	flycheck-pos-tip
	flycheck-status-emoji
	;;flycheck-tip :: BROKEN
	flycheck-pyflakes
	company
	company-ansible
	company-arduino
	company-c-headers
	company-cmake
	company-coq
	company-go
	company-irony
	company-jedi
	company-quickhelp
	company-statistics
	company-web
	paradox ; show packages with sortable columns
	pacmacs
	regex-tool
	sicp ; info files for SICP
	;; snapshot-timemachine ; step through zfs/btrfs file snapshots
	;; sos ; stackoverflow search
	;; sotlisp ; speed of thought lisp 
	ssh-config-mode ; ~/.ssh/config
	ssh-file-modes ; ~/.ssh/{known_hosts,authorized_keys}
	;; string-inflection ; underscore -> UPCASE -> CamelCase -> lowerCamelCase conversion of names
	;; stumpwm-mode 
	;; svg ; BROKEN
	;; try ; try emacs packages without installing them
	visible-mark 
	visual-ascii-mode
	etags-select
	))

;; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for i in my-package-list
	when (not (package-installed-p i)) do (return nil)
	finally (return t)))

;; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" "Done.")
  ;; install the missing packages
  (dolist (i my-package-list)
    (message "package: %s" i)
    (when (not (package-installed-p i))
      (package-install i))))

(message "----- DEBUG: packages installed")

;; COLOR THEME
(setq color-theme-is-global t)
;;(load-theme 'hc-zenburn)
;;(load-theme 'manoj-dark)
;;(load-theme 'wombat)
;;(color-theme-molokai)
;;(color-theme-monokai)
(load-theme 'tangotango t)

(message "----- DEBUG: color themes loaded")

;; SLIME
(setq inferior-lisp-program "/usr/local/bin/sbcl")
;;(setq slime-contribs '(slime-fancy))
;;(add-to-list 'load-path "~/.emacs.d/slime")
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (defun cliki:start-slime ()
;;   ;; TODO i currently dont know where i got this or what it does
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))
;; (add-hook 'slime-mode-hook 'cliki:start-slime)
;; (slime-setup '(slime-repl))
(require 'slime)
(slime-setup)

(message "----- DEBUG: slime loaded")

;; BASH COMPLETION
(require 'bash-completion)
(bash-completion-setup)
;; (autoload 'bash-completion-dynamic-complete "bash-completion" "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
(require 'flycheck-checkbashisms)
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-checkbashisms-setup))

(message "----- DEBUG: bash stuff loaded")

;; COMPANY MODE
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0) ; immeadiate completion
(setq company-etags-everywhere t)
(setq company-etags-ignore-case t)
(setq company-etags-use-main-table-list t)

(message "----- DEBUG: company mode loaded")

;; PROJECTILE
(setq projectile-global-mode 1)
(setq projectile-enable-caching t)
;; (setq projectile-file-exists-remote-cache-expire nil) ; needed if tram freezes
(setq projectile-switch-project-action 'projectile-find-dir)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-completion-system 'default)

;; FRINGE STUFF
(require 'fringe-current-line)
(global-fringe-current-line-mode t)

;; GENERAL SETUP
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ;; dedicated backup directory
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; remove menubar
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; remove toolbar
;;(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; remove scrollbar
(set-face-attribute 'default nil :height 90)
(defalias 'yes-or-no-p 'y-or-n-p) ;; easier accepting/denying yes-no prompts
(setq global-hl-line-mode t) ;; cursorline
(setq show-paren-mode t) ;; show matching parens
(setq electric-pair-mode t) ;; insert closing parens at once
(setq column-number-mode t) ;; show column
(setq global-prettify-symbols-mode t) ;; lambda as shown as symbol
;; (add-hook 'after-init-hook (setq eldoc-mode t))
(setq eldoc-mode t)
(key-chord-define-global "jk" 'evil-normal-state)
(key-chord-mode t)
(setq semantic-mode t)
(setq recentf-mode t)
(setq recentf-auto-cleanup 'never)

(message "----- DEBUG: general stuff loaded")

;; PROG-MODE-HOOK LIST
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

(message "debugger progmode hooks loaded")

;; EMACS DEVELOPMENT
(setq debug-on-error t)
					;(evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'elisp-slime-nav-describe-elisp-thing-at-point)
(evil-leader/set-key "k" 'elisp-slime-nav-describe-elisp-thing-at-point)
(evil-leader/set-key "sv"
  (lambda () (interactive)
    (progn
      (save-buffer)
      (eval-buffer))))

(message "----- DEBUG: init.el reloadable")

;; EVIL STUFF
(require 'evil-leader)
(evil-leader/set-leader "SPC")
(setq evil-mode t)
(setq global-evil-leader-mode t)

(defun volatile-kill-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))
(evil-leader/set-key "fw" 'save-buffer)
(evil-leader/set-key "fq" 'kill-buffer)
(evil-leader/set-key "rq" 'volatile-kill-buffer)
(evil-leader/set-key "ff" 'recentf-open-files)
(evil-leader/set-key "fe" 'find-file)
(evil-leader/set-key "fj" (lambda (arg) (interactive (list (read-file-name "Enter name of file to be created: "))) (progn (with-temp-buffer (write-file arg)) (find-file-other-window arg))))
(evil-leader/set-key "ee" (lambda () (interactive) (find-file-other-window "~/.emacs.d/init.el")))
(evil-leader/set-key "eb" (lambda () (interactive) (find-file-other-window "~/.bashrc")))
;;  (evil-leader/set-key "esa" (lambda () (interactive) (find-file-other-window "~/.dotfiles/aliases/aliases")))
(evil-leader/set-key "1" 'delete-other-windows)
(evil-leader/set-key "2" 'split-window-below)
(evil-leader/set-key "3" 'split-window-right)
(evil-leader/set-key "0" 'delete-window)
(evil-leader/set-key "b" 'helm-buffers-list)
(evil-leader/set-key "n" 'next-buffer)
(evil-leader/set-key "p" 'previous-buffer)
;;  (evil-leader/set-key "DEL" 'keyboard-quit)
(evil-leader/set-key "c" 'evilnc-comment-or-uncomment-lines)
(evil-leader/set-key "fs" (lambda () (interactive) (progn (kill-whole-line) (insert (concat "date: " (shell-command-to-string "date --rfc-3339=seconds | cut -c1-19")))(evil-previous-line-first-non-blank))))
(evil-leader/set-key "wq"
  (lambda () (interactive)
    (progn
      (save-buffer)
      (kill-buffer))))

(message "----- DEBUG: evil partially loaded")

;; PROPER BUFFER / WINDOW MANAGEMENT
(global-set-key [C-left] 'windmove-left) 
(global-set-key [C-right] 'windmove-right) 
(global-set-key [C-up] 'windmove-up) 
(global-set-key [C-down] 'windmove-down)
(global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer) ;; forget the angle brackets and C-tab wont work
(global-set-key (kbd "<C-tab>") 'next-buffer)             ;; forget the angle brackets and C-S-tab wont work
(global-set-key (kbd "C-j") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-k") (lambda () (interactive) (other-window -1)))
(evil-leader/set-key "-" 'minimize-window)
(evil-leader/set-key "=" 'maximize-window)
(evil-leader/set-key "SPC" 'balance-windows)
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

(message "----- DEBUG: window management loaded")

;; ETAGS
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; HELM
(require 'helm)
(require 'helm-config)

(setq helm-mode t)
(setq helm-autoresize-mode t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-descbinds-mode t)
(setq helm-split-window-in-side-p t) ; open helm buffer inside current window, not occupy whole other window
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x c") 'eval-buffer)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-x C-b")
		(lambda () (interactive)
		  (progn
		    (buffer-menu)
		    (other-window))))
(global-set-key (kbd "C-x C-o") 'list-packages)
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

(message "----- DEBUG: helm stuff loaded")

;;  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;  helm-ff-file-name-history-use-recentf t)

;;  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;  (golden-ratio-mode 0)
;;  (defvar golden-ratio-inhibit-functions)
;;  (defun pl/helm-alive-p ()
;;    "Deactive golden ratio when helm is in use iirc."
;;    (if (boundp 'helm-alive-p)
;;        (symbol-value 'helm-alive-p)))
;;  (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

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
;;  (global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;;  (defun move-line-down ()
;;    "Move current line down and set focus there"
;;    (interactive)
;;    (let ((col (current-column)))
;;      (save-excursion
;;        (forward-line)
;;        (transpose-lines 1))
;;      (forward-line)
;;      (move-to-column col)))
;;  (defun move-line-up ()
;;    "Move current line up and set focus there"
;;    (interactive)
;;    (let ((col (current-column)))
;;      (save-excursion
;;        (transpose-lines 1))
;;      (forward-line -1)
;;      (move-to-column col)))
;;  (global-set-key (kbd "<M-down>") 'move-line-down)
;;  (global-set-key (kbd "<M-up>") 'move-line-up)

;;  ;;; full screen magit-status
;;  (defadvice magit-status (around magit-fullscreen activate)
;;    (window-configuration-to-register :magit-fullscreen)
;;    ad-do-it
;;    (delete-other-windows))
;;  (defun magit-quit-session ()
;;    "Restores the previous window configuration and kills the magit buffer"
;;    (interactive)
;;    (kill-buffer)
;;    (jump-to-register :magit-fullscreen))
;;  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;  (global-set-key [remap goto-line] 'goto-line-with-feedback)
;;  (defun goto-line-with-feedback ()
;;    "Show line numbers temporarily, while prompting for the line number input."
;;    (interactive)
;;    (unwind-protect
;;        (progn
;;          (linum-mode 1)
;;          (forward-line (read-number "Goto line: ")))
;;      (linum-mode -1)))

;;  (defadvice save-buffer (around save-buffer-as-root-around activate)
;;    "Use sudo to save the current buffer."
;;    (interactive "p")
;;    (if (and (buffer-file-name) (not (file-writable-p (buffer-file-name))))
;;        (let ((buffer-file-name (format "/sudo::%s" buffer-file-name)))
;;	ad-do-it)
;;      ad-do-it))

;; (defadvice find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;  (defun my-lisp-hook ()
;;    "Helper function so elisp has proper modes loaded."
;;    (elisp-slime-nav-mode)
;;    (eldoc-mode))

;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     ; Make sure it's not a remote buffer or flymake would not work
;;     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
;;       (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;              (local-file (file-relative-name
;;                           temp-file
;;                           (file-name-directory buffer-file-name))))
;;         (list "pyflakes" (list local-file)))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;;  (eval-after-load 'flycheck
;;   '(add-to-list 'flycheck-checkers 'irony))

;;  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
;;  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;  (add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))

;;  (add-hook 'after-init-hook #'global-flycheck-mode)
;;  (define-key flycheck-mode-map flycheck-keymap-prefix (kbd "C-c f"))
;;  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

;;  (evil-leader/set-key "j" (lambda () (interactive) (flycheck-next-error t)))
;;  (evil-leader/set-key "k" (lambda () (interactive) (flycheck-previous-error)))
;;  (evil-leader/set-key "l" 'flycheck-list-errors)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

(message "----- emacs' init.el reloaded -----")

;;;;(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;;'(custom-safe-themes
;;     (quote
;;      ("49e5a7955b853f70d1fe751b2f896921398b273aa62f47bda961a45f80219581" default)))
;;   '(package-selected-packages
;;     (quote
;;      (zenburn-theme vimrc-mode vi-tilde-fringe use-package tangotango-theme spray sos solarized-theme rfringe rainbow-mode rainbow-identifiers rainbow-delimiters quickrun python-mode puppet-mode ps-ccrypt prodigy private powerline-evil parscope markdown-toc markdown-mode+ magit key-chord helm-projectile helm-git helm-dirset helm-descbinds helm-dash hc-zenburn-theme grandshell-theme golden-ratio fringe-current-line free-keys flyparens flymake-shell flymake-puppet flymake-cursor flylisp flycheck-tip flycheck-pyflakes flycheck-pos-tip flycheck-package flycheck-irony flycheck-clojure evil-paredit evil-numbers evil-nerd-commenter evil-matchit evil-leader evil-jumper evil-indent-textobject eshell-fringe-status elisp-slime-nav company color-theme-wombat color-theme-monokai color-theme-molokai bash-completion ascii ac-helm))))
;;;;  (custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;;   )
