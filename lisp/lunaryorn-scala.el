;;; lunaryorn-scala.el --- Personal Scala tools -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Sebastian Wiesner

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

(require 'sbt-mode)
(require 'ensime)

(defun lunaryorn-scala-pop-to-sbt (new-frame)
  "Open SBT REPL for this project, optionally in a NEW-FRAME.

Select the SBT REPL for the current project in a new window.  If
the REPL is not yet running, start it.  With prefix arg, select
the REPL in a new frame instead."
  (interactive "P")
  ;; Start SBT when no running, taken from `sbt:command'
  (when (not (comint-check-proc (sbt:buffer-name)))
    (sbt:run-sbt))

  (let ((display-buffer-overriding-action (if new-frame
                                              '(display-buffer-pop-up-frame)
                                            nil)))
    (pop-to-buffer (sbt:buffer-name))))

(provide 'lunaryorn-scala)
;;; lunaryorn-scala.el ends here
