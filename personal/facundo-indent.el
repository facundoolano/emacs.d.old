;;; facundo-indent.el --- indent and tab behavior commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano; <facundo@madmobile>
;; Keywords: indentation

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

(require 'company)
(require 'company-dabbrev)
(require 'js2-mode)

(defvar my-indentation-offset 2 "My indentation offset.")
(setq js2-basic-offset my-indentation-offset)

;; make tab cycle wrap list
(setq company-selection-wrap-around 1)
(setq company-dabbrev-downcase nil)

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
  "If mark is active shift left the code block, extending the region to include whole lines.  if no mark is set shift the current line."
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

(define-key prog-mode-map (kbd "<tab>") 'my-indent)
(define-key js2-mode-map (kbd "<tab>") 'my-indent)
;; not using this one anymore since its defined in the company-simple-complete:
;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<return>") 'company-select-next)
(define-key prog-mode-map (kbd "<backtab>") 'my-unindent)
(define-key js2-mode-map (kbd "<backtab>") 'my-unindent)
(define-key js2-mode-map [(backspace)] 'backspace-whitespace-to-tab-stop)

(provide 'facundo-indent)
;;; facundo-indent.el ends here
