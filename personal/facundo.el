;;; facundo.el --- my emacs config                   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Facundo Olano

;; Author: Facundo Olano
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(prelude-require-packages '(drag-stuff monokai-theme nameframe-projectile neotree add-node-modules-path hl-todo js2-highlight-vars parinfer spaceline))

;;; sublime like color theme
(disable-theme 'zenburn)
(load-theme 'monokai t)
(which-function-mode -1)

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
(global-set-key (kbd "s-F") 'helm-projectile-grep)

(defun kill-project-frame ()
  "Delete current frame and kill all project buffers."
  (interactive)
  (mapc 'kill-buffer (projectile-project-buffers))
  (delete-frame))

(global-set-key (kbd "s-w") 'kill-project-frame)

(defun my-replace-string ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'replace-string)))

(global-set-key (kbd "M-s-f") 'my-replace-string)

(define-key prelude-mode-map (kbd "C-o") 'crux-smart-open-line)
(define-key prelude-mode-map (kbd "M-o") 'crux-smart-open-line-above)
(define-key prelude-mode-map (kbd "s-o") 'projectile-switch-project)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(global-set-key (kbd "s-a") 'select-current-line)
(global-set-key (kbd "s-A") 'mark-whole-buffer)

(setq projectile-globally-ignored-directories (append '("node_modules" "coverage") projectile-globally-ignored-directories))

;;; open project in new frame
(nameframe-projectile-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; copy pasted a modified version of the name frame function so it works in emacs for mac
;;; FIXME ask why this is needed maybe open a PR or fork
(defun nameframe-projectile--before-switch-project-hook ()
  "Hook to create/switch to a project's frame."
  (let* ((project-to-switch nameframe-projectile--project-to-switch)  ;; set by advise
         (name (file-name-nondirectory (directory-file-name project-to-switch)))
         (curr-frame (selected-frame))
         (frame-alist (nameframe-frame-alist))
         (frame (nameframe-get-frame name frame-alist)))
    (cond
     ;; project frame already exists
     ((and frame (not (equal frame curr-frame)))
      (select-frame-set-input-focus frame))
     ((not frame)
      (progn (nameframe-make-frame name) (select-frame-set-input-focus (nameframe-get-frame name)))))))

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
;;; FIXME should not indent a region if not selected, just the current line
;;; https://ignaciopp.wordpress.com/2009/06/17/emacs-indentunindent-region-as-a-block-using-the-tab-key/

(defvar my-indentation-offset 2 "My indentation offset. ")

;;; TODO tge whole extend region to line beg/end deal should be factored out to its own function
(defun my-indent ()
  "If mark is active indent code block, otherwise call company indet or complete."
  (interactive)
  (if mark-active
    (save-mark-and-excursion
     (let ((beg (region-beginning)) (end (region-end)))
       (save-excursion
         (setq beg (progn (goto-char beg) (line-beginning-position))
               end (progn (goto-char end) (line-end-position)))
         (indent-code-rigidly beg end my-indentation-offset)))
     (setq deactivate-mark nil))
    (if (looking-at "\\_>")
      (company-complete-common-or-cycle)
      (indent-according-to-mode))))

(defun my-unindent ()
  (interactive)
  (if mark-active
   (save-mark-and-excursion
    (let ((beg (region-beginning)) (end (region-end)))
      (save-excursion
         (setq beg (progn (goto-char beg) (line-beginning-position))
               end (progn (goto-char end) (line-end-position)))
         (indent-code-rigidly beg end (- my-indentation-offset)))
      (setq deactivate-mark nil)))
   (indent-code-rigidly (line-beginning-position) (line-end-position) (- my-indentation-offset))))

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
  "Kill the current buffer and move to the next current project one if any, else find file in project."
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

(defadvice isearch-search (after isearch-no-fail activate)
  "Advice search to be wrapped by default."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; highlights todo and fixme
(require 'hl-todo)
(global-hl-todo-mode t)
(setq hl-todo-activate-in-modes '(prog-mode))

;;; parinfer config
(require 'parinfer)

;; use regular yank to avoid weird region replacement
(define-key parinfer-region-mode-map [remap yank] 'yank)
(define-key parinfer-region-mode-map (kbd "<tab>") 'parinfer-shift-right)
(define-key parinfer-region-mode-map (kbd "<backtab>") 'parinfer-shift-left)

;; redefine defaults to avoid unwanted extensions
(setq parinfer-extensions '(defaults pretty-parens))

(defun disable-smartparens ()
  "Try real hard to disable smartparens everywhere, and still won't work."
  (turn-off-smartparens-mode)
  ;; (smartparens-global-mode -1)
  ;; (smartparens-global-strict-mode -1)
  (smartparens-strict-mode -1)
  (smartparens-mode -1))

(add-hook 'prelude-prog-mode-hook 'disable-smartparens)
(add-hook 'prelude-emacs-lisp-mode-hook 'disable-smartparens)
(add-hook 'emacs-lisp-mode-hook 'disable-smartparens)
(add-hook 'prelude-lisp-coding-hook 'disable-smartparens)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(global-set-key (kbd "s-(") 'parinfer-toggle-mode)

;; a bunch of basic keybindings needed for emacs for mac
(when (eq system-type 'darwin)
  (cua-mode)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (define-key cua--cua-keys-keymap (kbd "C-v") 'scroll-up-command)
  (define-key cua--cua-keys-keymap (kbd "M-v") 'scroll-down-command)
  (global-set-key (kbd "s-x") 'cua-cut-region)
  (global-set-key (kbd "s-c") 'cua-copy-region)
  (global-set-key (kbd "s-v") 'cua-paste)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-l") 'goto-line))

(require 'spaceline-config)
(spaceline-emacs-theme)
(setq powerline-default-separator 'slant)
(spaceline-helm-mode)
(spaceline-toggle-projectile-root-on)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-buffer-encoding-abbrev-off)

(provide 'facundo)
;;; facundo.el ends here
