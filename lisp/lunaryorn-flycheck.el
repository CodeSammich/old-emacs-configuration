;;; lunaryorn-flycheck.el --- Additional utilities for Flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Sebastian Wiesner

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

;; Helpers for Flycheck

;;; Code:

(require 'flycheck)
(require 'dash)

(defun lunaryorn-flycheck-set-load-path-for-user-configuration ()
  "Set Flycheck load path for files in user configuration."
  (when (and (buffer-file-name)
             (flycheck-in-user-emacs-directory-p (buffer-file-name)))
    (setq-local flycheck-emacs-lisp-load-path
                (cons (locate-user-emacs-file "lisp/")
                      flycheck-emacs-lisp-load-path))))

(defun lunaryorn-discard-undesired-html-tidy-error (err)
  "Discard ERR if it is undesired.

Tidy is very verbose, so we prevent Flycheck from highlighting
most errors from HTML Tidy."
  ;; A non-nil result means to inhibit further processing (i.e. highlighting)
  ;; of the error
  (and (eq (flycheck-error-checker err) 'html-tidy)
       ;; Only allow warnings about missing tags, or unexpected end tags being
       ;; discarded
       (not (string-match-p (rx (or "missing" "discarding"))
                            (flycheck-error-message err)))))

(defun lunaryorn-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module) package-directory)))))))

(defun lunaryorn-flycheck-mode-line-status ()
  "Create a mode line status text for Flycheck."
  (let* ((menu (mouse-menu-non-singleton flycheck-mode-menu-map))
         (map (make-mode-line-mouse-map 'mouse-1
                                        (lambda ()
                                          (interactive)
                                          (popup-menu menu))))
         (text-and-face
          (pcase flycheck-last-status-change
            (`not-checked nil)
            (`no-checker '("•" . warning))
            (`running '("*" . success))
            (`errored '("!" . error))
            (`finished
             (let* ((error-counts (flycheck-count-errors
                                   flycheck-current-errors))
                    (no-errors (cdr (assq 'error error-counts)))
                    (no-warnings (cdr (assq 'warning error-counts))))
               (cond
                (no-errors
                 (cons (format "•%s •%s" no-errors (or no-warnings 0)) 'error))
                (no-warnings
                 (cons (format "•%s" no-warnings) 'warning))
                (t (cons "•" 'success)))))
            (`interrupted (cons "x" nil))
            (`suspicious '("?" . warning)))))
    (when text-and-face
      (propertize (car text-and-face) 'face (cdr text-and-face)
                  'mouse-face 'mode-line-highlight
                  'local-map map))))

(provide 'lunaryorn-flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; lunaryorn-flycheck.el ends here
