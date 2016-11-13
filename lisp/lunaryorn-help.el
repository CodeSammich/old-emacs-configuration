;;; lunaryorn-help.el --- Personal help enhancements  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2016 Wilfred Hughes

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;         Wilfred Hughes

;; This file is not part of GNU Emacs.

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

;; Personal help utilities.

;;; Code:

(require 'subr-x)

(defun lunaryorn-describe-symbol-at-point ()
  "Describe the symbol at point if any."
  (interactive)
  (when-let (symbol (symbol-at-point))
    (describe-symbol symbol)))

(provide 'lunaryorn-help)
;;; lunaryorn-help.el ends here
