;;; facundo-clojure.el --- clojure configuration     -*- lexical-binding: t; -*-

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
(require 'cider)
(setq cider-repl-scroll-on-output nil)

(defun reload-and-eval-in-repl ()
  "Set the ns of the repl to the one in the current buffer, then eval the region of the whole buffer in the repl and switch to it."
  (interactive)
  (cider-repl-set-ns (cider-current-ns))
  (if (region-active-p)
      (cider-insert-region-in-repl (region-beginning) (region-end)))
  (cider-load-buffer-and-switch-to-repl-buffer)
  (cider-repl-return))

(define-key clojure-mode-map (kbd "s-e") 'reload-and-eval-in-repl)

(provide 'facundo-clojure)
;;; facundo-clojure.el ends here
