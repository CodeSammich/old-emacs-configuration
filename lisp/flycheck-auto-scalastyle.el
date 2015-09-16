;;; flycheck-auto-scalastyle.el --- Automatically install Scalastyle  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Keywords: tools, convenience

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

;; Automatically install Scalastyle for Flycheck.

;;; Code:

(require 'flycheck)

(defcustom flycheck-auto-scalastyle-version '("0.7.0" . "2.10")
  "Version of scala style to use for Flycheck.

A pair of `(VERSION . SCALA-VERSION)'."
  :type '(cons (string :tag "Scalastyle version")
               (string :tag "Scala language version"))
  :group 'flycheck
  :safe '(lambda (value)
           (and (consp value) (stringp (car value)) (stringp (cdr value)))))

(defcustom flycheck-auto-scalastyle-jar-dir (locate-user-emacs-file "scalastyle")
  "Directory for installed Scalastyle JARs."
  :type 'directory
  :group 'flycheck)

(defsubst flycheck-auto-scalastyle-jar-name ()
  "Get the file name of the Scalastyle JAR."
  (pcase-let ((`(,version . ,scala-version) flycheck-auto-scalastyle-version))
    (format "scalastyle_%s-%s-batch.jar" scala-version version)))

(defsubst flycheck-auto-scalastyle-url ()
  "Get the URL to download Scalastyle."
  (pcase-let ((`(,version . ,scala-version) flycheck-auto-scalastyle-version))
    (format "https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_%s/%s/%s"
            scala-version version (flycheck-auto-scalastyle-jar-name))))

(defun flycheck-auto-scalastyle-ensure ()
  "Ensure and return the scalastyle JAR for this buffer."
  (let ((file-name (expand-file-name (flycheck-auto-scalastyle-jar-name)
                                    flycheck-auto-scalastyle-jar-dir)))
    (unless (file-exists-p file-name)
      (make-directory flycheck-auto-scalastyle-jar-dir 'parents)
      (message "Downloading scalastyle JAR")
      (url-copy-file (flycheck-auto-scalastyle-url) file-name))
    file-name))

;;;###autoload
(defun flycheck-auto-scalastyle-setup ()
  "Setup Flycheck Scalastyle for this buffer."
  (interactive)
  (setq flycheck-scalastyle-jar (flycheck-auto-scalastyle-ensure)))

(provide 'flycheck-auto-scalastyle)
;;; flycheck-auto-scalastyle.el ends here
