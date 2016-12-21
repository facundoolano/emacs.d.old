;;; facundo customizations

;;; list of required packages
(prelude-require-packages '(drag-stuff monokai-theme nameframe-projectile neotree add-node-modules-path))

;;; sublime like color theme
(disable-theme 'zenburn)
(load-theme 'monokai t)
;; (load-theme 'solarized-dark t)

;;; project tree
(require 'neotree)
(setq neo-theme 'ascii)
(setq projectile-switch-project-action 'neotree-projectile-action)
;(setq projectile-find-file-hook 'neotree-projectile-action)

; yanked from spacemacs
(setq neo-window-width 32
      neo-create-file-auto-open t
      neo-banner-message "Press ? for neotree help"
      neo-show-updir-line nil
      neo-mode-line-type 'neotree
      neo-smart-open t
      neo-dont-be-alone t
      neo-persist-show nil
      neo-show-hidden-files t
      neo-auto-indent-point t
      neo-modern-sidebar t
      neo-vc-integration nil)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)
;; (neotree-toggle)
;;; TODO add function that finds current file in neotree and goes back to buffer.
;;; run it on initial toggle and after projectile find file

;;; move selection with M
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;;; redo with cmd shift z
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;;; disable scrollbar
(scroll-bar-mode -1)

;;; remember window size
(desktop-save-mode 1)

;;; disable line wrapping
(set-default 'truncate-lines t)

;;; helm stuff
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;;; toggle comments

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)

;;; bind projectile/helm stuff to cmd key
(define-key prelude-mode-map (kbd "s-p") nil)
(define-key prelude-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "s-p") 'helm-projectile-find-file)
(global-set-key (kbd "s-P") 'helm-M-x)
(global-set-key (kbd "s-F") 'helm-projectile-grep)

(define-key prelude-mode-map (kbd "C-o") 'crux-smart-open-line)
(define-key prelude-mode-map (kbd "M-o") 'crux-smart-open-line-above)
(define-key prelude-mode-map (kbd "s-o") 'projectile-switch-project)

(setq projectile-globally-ignored-directories (append '("node_modules") projectile-globally-ignored-directories))

;;; open project in new frame
(nameframe-projectile-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; navigate buffers
(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))


(global-set-key (kbd "C-<tab>") 'xah-next-user-buffer)
(global-set-key (kbd "C-S-<tab>") 'xah-previous-user-buffer)


;;; show line numbers, but not on neotree
(setq linum-format 'dynamic)
;(global-linum-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

(defun my/neotree-hook (_unused)
  ;(fringe-mode '(0 . 0))
  (linum-mode -1))
(add-hook 'neo-after-create-hook 'my/neotree-hook)

(fringe-mode '(10 . 0))
(setq-default line-spacing 2)

;;; indent/unindent with tab
;;; TODO add smarts to include half selected lines
;;; FIXME should not indent a region if not selected, just the current line
;;; FIXME should fire completion if in the middle of line and no region selected
;;; https://ignaciopp.wordpress.com/2009/06/17/emacs-indentunindent-region-as-a-block-using-the-tab-key/

(defun my-indent ()
  "If mark is active indent code block, otherwise call company indet or complete."
  (interactive)
  (if mark-active
    (save-mark-and-excursion
     (indent-code-rigidly (region-beginning) (region-end) 2)
     (setq deactivate-mark nil))
    (if (looking-at "\\_>")
      (company-complete-common-or-cycle)
      (indent-according-to-mode))
    )
  )

(defun my-unindent ()
  (interactive)
  (save-mark-and-excursion
   (indent-code-rigidly (region-beginning) (region-end) -2))
   (setq deactivate-mark nil))

(define-key prog-mode-map (kbd "<tab>") 'my-indent)
(define-key js2-mode-map (kbd "<tab>") 'my-indent)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key prog-mode-map (kbd "<backtab>") 'my-unindent)
(define-key js2-mode-map (kbd "<backtab>") 'my-unindent)

;;; js customizations
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(setq js2-basic-offset 2)

(global-set-key (kbd "M-n i") 'npm-install)
;; (global-set-key (kbd "M-n n") 'npm-new)
(global-set-key (kbd "M-n d") 'npm-new-dependency)
;; (global-set-key (kbd "M-n e") 'npm-nodemon-exec)
(global-set-key (kbd "M-n p") 'npm-publish)
(global-set-key (kbd "M-n t") 'npm-test)
(global-set-key (kbd "M-n v") 'npm-version)


(defun new-empty-buffer ()
  "Open a new empty buffer"
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))))


;;; wrap search by default
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))
