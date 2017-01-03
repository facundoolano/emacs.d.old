;;; facundo-node.el --- node programming customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano;;; js customizations <facundo@madmobile>
;; Keywords: node.js javascript

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

(prelude-require-package 'add-node-modules-path)

(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

;;; FIXME this should go in js2 mode only
(global-set-key (kbd "M-n i") 'npm-install)
;; (global-set-key (kbd "M-n n") 'npm-new)
(global-set-key (kbd "M-n d") 'npm-new-dependency)
;; (global-set-key (kbd "M-n e") 'npm-nodemon-exec)
(global-set-key (kbd "M-n p") 'npm-publish)
(global-set-key (kbd "M-n t") 'npm-test)
(global-set-key (kbd "M-n v") 'npm-version)

(provide 'facundo-node)
;;; facundo-node.el ends here
