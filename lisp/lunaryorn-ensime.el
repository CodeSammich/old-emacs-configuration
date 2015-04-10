;;; lunaryorn-ensime.el --- Ensime enhancements      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Keywords: tools, convenience

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

;; Enhancements to Ensime
;;
;; Graciously taken from https://github.com/syl20bnr/spacemacs/blob/master/contrib/lang/scala/packages.el

;;; Code:

(require 'ensime)

(defun lunaryorn-ensime-gen-and-restart ()
  "Generate `.ensime' and restart Ensime."
  (interactive)
  (sbt-command "gen-ensime")
  (ensime-shutdown)
  (ensime))

(provide 'lunaryorn-ensime)

;;; lunaryorn-ensime.el ends here
