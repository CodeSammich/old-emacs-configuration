;;; lunaryorn-firestarter.el --- Firestarter extensions  -*- lexical-binding: t; -*-

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

;; Additional mode line reporting for Firestarter

;;; Code:

(require 'firestarter)

(defvar-local lunaryorn-firestarter-last-status nil
  "The last state of firestarter.")

(defun lunaryorn-firestarter-track-state (process)
  "Track the state of a firestarter PROCESS in the corresponding buffer."
  (let ((status (process-status process))
        (return-code (process-exit-status process))
        (buffer (get-buffer (process-get process 'buffer-name))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((status (pcase (list status return-code)
                        (`(exit 0) 'success)
                        (_ 'failure))))
          (setq lunaryorn-firestarter-last-status status))
        (force-mode-line-update)))))

(add-hook 'firestarter-reporting-functions #'lunaryorn-firestarter-track-state)

(advice-add 'firestarter :before
            (lambda ()
              (setq lunaryorn-firestarter-last-status 'running)
              (force-mode-line-update))
            '((name . lunaryorn-firestarter-track-running)))

(defun lunaryorn-firestarter-mode-line ()
  "Get a mode line status string for firestarter."
  (let ((base "🔥")
        (status-string (pcase lunaryorn-firestarter-last-status
                         ((guard (not firestarter)) " ")
                         (`running "⚫")
                         (`success "🔵")
                         (`failure "🔴")
                         (_ "⚪"))))
    (concat " " base status-string)))

(provide 'lunaryorn-firestarter)

;;; lunaryorn-firestarter.el ends here
