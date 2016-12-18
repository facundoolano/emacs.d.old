;;; facundo customizations

;;; sublime like color theme
(disable-theme 'zenburn)
(load-theme 'monokai t)
;; (load-theme 'solarized-dark t)

;;; project tree
(require 'neotree)
(neotree-toggle)
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

;;; move selection with M
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;;; redo with cmd shift z
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;;; disable scrollbar
(scroll-bar-mode -1)

;;; remember window size
(desktop-save-mode 1)

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
