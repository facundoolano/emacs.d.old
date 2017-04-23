;;; facundo-parinfer.el --- parinfer/lisp edition configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano <facundo@madmobile>
;; Keywords: lisp

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

;;

;;; Code:

(prelude-require-package 'parinfer)
(require 'parinfer)

;; use regular yank to avoid weird region replacement
(define-key parinfer-region-mode-map [remap yank] 'yank)
;; (define-key parinfer-region-mode-map (kbd "<tab>") 'parinfer-shift-right)
;; (define-key parinfer-region-mode-map (kbd "<backtab>") 'parinfer-shift-left)

(define-key parinfer-mode-map (kbd "<tab>") 'parinfer-smart-tab:dwim-right-or-complete)
(define-key parinfer-mode-map (kbd "<backtab>") 'parinfer-smart-tab:dwim-left)

;; enable some paredit commands
(define-key parinfer-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key parinfer-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
(define-key parinfer-mode-map (kbd "C-{") 'sp-forward-barf-sexp)
(define-key parinfer-mode-map (kbd "C-}") 'sp-backward-barf-sexp)

;; Redefine defaults to avoid unwanted extensions
(setq parinfer-extensions '(defaults pretty-parens))
(add-hook 'parinfer-mode-enable-hook #'parinfer--switch-to-paren-mode)

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
(add-hook 'clojure-mode-hook #'electric-pair-mode)
(add-hook 'cider-repl-mode-hook #'parinfer-mode)
(add-hook 'cider-repl-mode-hook #'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
(add-hook 'emacs-lisp-mode-hook #'electric-pair-mode)

(global-set-key (kbd "s-(") 'parinfer-toggle-mode)

(provide 'facundo-parinfer)
;;; facundo-parinfer.el ends here
