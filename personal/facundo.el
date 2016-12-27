;;; facundo customizations

;;; list of required packages
(prelude-require-packages '(drag-stuff monokai-theme nameframe-projectile neotree add-node-modules-path hl-todo js2-highlight-vars parinfer))

;;; sublime like color theme
(disable-theme 'zenburn)
(load-theme 'monokai t)
;; (load-theme 'solarized-dark t)

;;; project tree
(require 'neotree)
(setq neo-theme 'ascii)
;(setq projectile-switch-project-action 'neotree-projectile-action)

; yanked from spacemacs
(setq neo-window-width 32
      neo-create-file-auto-open t
      neo-banner-message "Press ? for neotree help"
      neo-show-updir-line nil
      neo-mode-line-type 'neotree
      neo-smart-open t
      neo-show-hidden-files t
      neo-auto-indent-point t
      neo-vc-integration nil)

(setq neo-toggle-window-keep-p t)

;; FIXME reduce duplication
(defun neotree-project-sync ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name))
        (cw (selected-window)))
    (neotree-show)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))
    (select-window cw)))

(defun neotree-project-toggle ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name))
        (cw (selected-window)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))
    (select-window cw)))

(setq neo-confirm-change-root "Off")
;; sync neotree when finding file with projectile
(add-hook 'projectile-find-file-hook 'neotree-project-sync)
(add-hook 'projectile-grep-finished-hook 'neotree-project-sync)

(global-set-key [f8] 'neotree-project-toggle)
;; (neotree-project-toggle)
(add-hook 'after-init-hook #'neotree-project-sync) ;;; is this working?

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

(defun delete-line-or-region ()
  "Deletes (without copying) the current line or the lines encompassed by the current region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (progn
        (setq beg (region-beginning) end (region-end))
        (save-excursion
          (setq beg (progn (goto-char beg) (line-beginning-position))
                end (progn (goto-char end) (line-end-position)))))
      (setq beg (line-beginning-position) end (line-end-position)))
    (delete-region beg end)
    (delete-char 1)))

(global-set-key (kbd "s-d") 'delete-line-or-region)

;;; bind projectile/helm stuff to cmd key
(define-key prelude-mode-map (kbd "s-p") nil)
(define-key prelude-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "s-p") 'helm-projectile-find-file)
(global-set-key (kbd "s-P") 'helm-M-x)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-F") 'helm-projectile-grep)

(defun my-replace-string ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'replace-string)))

(global-set-key (kbd "M-s-ƒ") 'my-replace-string)

(define-key prelude-mode-map (kbd "C-o") 'crux-smart-open-line)
(define-key prelude-mode-map (kbd "M-o") 'crux-smart-open-line-above)
(define-key prelude-mode-map (kbd "s-o") 'projectile-switch-project)

(setq projectile-globally-ignored-directories (append '("node_modules") projectile-globally-ignored-directories))

;;; open project in new frame
(nameframe-projectile-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; navigate buffers
(defun xah-next-user-buffer ()
  "Switch to the next user buffer within the current project.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (let ((root (projectile-project-root)))
    (next-buffer)
    (let ((i 0))
      (while (< i 20)
        (if (or (not (xah-user-buffer-q)) (not (projectile-project-buffer-p (current-buffer) root)))
            (progn (next-buffer)
                   (setq i (1+ i)))
          (progn (setq i 100))))
      (neotree-project-sync))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer within the current project.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (let ((root (projectile-project-root)))
    (previous-buffer)
    (let ((i 0))
      (while (< i 20)
        (if (or (not (xah-user-buffer-q)) (not (projectile-project-buffer-p (current-buffer) root)))
          (progn (previous-buffer)
                 (setq i (1+ i)))
          (progn (setq i 100))))
      (neotree-project-sync))))
 ;;
(defun xah-user-buffer-q (&optional buffer)
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name buffer) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

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

(defvar my-indentation-offset 2 "My indentation offset. ")

(defun my-indent ()
  "If mark is active indent code block, otherwise call company indet or complete."
  (interactive)
  (if mark-active
    (save-mark-and-excursion
     (indent-code-rigidly (region-beginning) (region-end) my-indentation-offset)
     (setq deactivate-mark nil))
    (if (looking-at "\\_>")
      (company-complete-common-or-cycle)
      (indent-according-to-mode))))

(defun my-unindent ()
  (interactive)
  (save-mark-and-excursion
   (indent-code-rigidly (region-beginning) (region-end) (- my-indentation-offset))
   (setq deactivate-mark nil)))

(define-key prog-mode-map (kbd "<tab>") 'my-indent)
(define-key js2-mode-map (kbd "<tab>") 'my-indent)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key prog-mode-map (kbd "<backtab>") 'my-unindent)
(define-key js2-mode-map (kbd "<backtab>") 'my-unindent)

(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) my-indentation-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-indentation-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(define-key js2-mode-map [(backspace)] 'backspace-whitespace-to-tab-stop)

;;; js customizations
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(setq js2-basic-offset my-indentation-offset)

;;; FIXME this should go in js2 mode only
(global-set-key (kbd "M-n i") 'npm-install)
;; (global-set-key (kbd "M-n n") 'npm-new)
(global-set-key (kbd "M-n d") 'npm-new-dependency)
;; (global-set-key (kbd "M-n e") 'npm-nodemon-exec)
(global-set-key (kbd "M-n p") 'npm-publish)
(global-set-key (kbd "M-n t") 'npm-test)
(global-set-key (kbd "M-n v") 'npm-version)

(defun new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))))

(defun new-empty-buffer-split ()
  "Open a new empty buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))))

(global-set-key (kbd "s-n") 'new-empty-buffer)
(global-set-key (kbd "s-N") 'new-empty-buffer-split)

(defun kill-this-and-next ()
  "Kill the current buffer and move to the next current project one if any, else find file in project. "
  (interactive)
  (let ((next (car (-filter 'xah-user-buffer-q (projectile-project-buffers-non-visible))))
        (pname (projectile-project-name)))
    (prin1 pname)
    (kill-buffer)
    (if next
      (switch-to-buffer next)
      (progn
        (projectile-switch-project-by-name pname) ;; FIXME this is not working
        (projectile-find-file)))))

(defun kill-other-project-buffers ()
  "Kill all user buffers from this project, except the current one."
  (interactive)
  (let ((project-buffers (-filter 'xah-user-buffer-q (projectile-project-buffers-non-visible))))
    (mapcar 'kill-buffer project-buffers)))

(define-key prelude-mode-map (kbd "s-k") nil)
(global-set-key (kbd "s-k") 'kill-this-and-next)
(global-set-key (kbd "s-K") 'kill-other-project-buffers)

;;; wrap search by default
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; highlights todo and fixme
(global-hl-todo-mode t)

;;; parinfer config
(defun disable-smartparens ()
  (turn-off-smartparens-mode)
  (smartparens-global-mode -1)
  (smartparens-mode -1))

(add-hook 'prelude-prog-mode-hook 'disable-smartparens)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(global-set-key (kbd "s-(") 'parinfer-toggle-mode)

;; (setq magit-diff-auto-show nil)
;; (add-hook 'git-commit-mode-hook (lambda () (save-selected-window (magit-process))))
