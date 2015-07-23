;;; lunaryorn-scala.el --- Personal Scala tools -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d

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

;; Enhancements for Scala with Ensime and SBT Mode.

;;; Code:

(require 'ensime)

(defun lunaryorn-scala-ensime-mode-line-status ()
  "Create a mode line status for Ensime."
  (condition-case _
      (let ((connection (ensime-connection-or-nil)))
        (cond ((and ensime-mode (not connection))
               (if (ensime-owning-server-process-for-source-file
                    (buffer-file-name))
                   "🔵"
                 "⭕"))
              ((and ensime-mode (ensime-connected-p connection))
               (cond ((not (eq (process-status connection) 'open))
                      (format "❗%s" (process-status connection)))
                     ((ensime-rex-continuations connection)
                      (format "🔨%s" (length
                                     (ensime-rex-continuations connection))))
                     ((not (ensime-analyzer-ready connection))
                      "🔨")
                     (t (let* ((warnings (ensime-num-warnings connection))
                               (errors (ensime-num-errors connection))
                               (face (if (> errors 0) 'error 'warning)))
                          (propertize (format "%s|%s" errors warnings)
                                      'face face)))))
              (ensime-mode "💀")))
    (error "❗")))

(provide 'lunaryorn-scala)
;;; lunaryorn-scala.el ends here
