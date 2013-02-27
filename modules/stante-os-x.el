;;; stante-os-x.el --- Stante Pede Modules: OS X support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012, 2013 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://gihub.com/lunaryorn/stante-pede.git
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.


;;; Commentary:

;; Perform OS X specific setup.  Do nothing, if not running on OS X.

;; Modifier keys
;; -------------
;;
;; Map OS X modifier keys to Emacs modifiers in a reasonable way:
;;
;; Map the left Option key to Meta.
;;
;; Map the left Command key to Meta, too, to preserve a consistent keyboard
;; "feeling" accross various keyboards, and to prevent some bad typing mistakes
;; like accidentally closing Emacs with Command-Q while trying to fill a
;; paragraph with Meta+Q.
;;
;; Map the right Command key to Super.
;;
;; Unmap the right Option key to allow input of accented characters and Unicode
;; symbols.  On various non-US layouts some important characters are only
;; available via Option key, for instance [,],|,@ and others on a German
;; keyboard).
;;
;; Map the Function key to Control to prevent typing mistakes.  Typically this
;; key is the outermost left key, and can easily be pressed accidentally if
;; trying to reach the Control key.

;; Full screen support
;; -------------------
;;
;; Emacs 24.3 provides native full screen support for OS X Lion and newer.  It
;; is recommended to use an Emacs 24.3 pretest from http://emacsformacosx.com/.
;;
;; For earlier releases various patches exist to add this functionality.
;; The Homebrew Formula for Emacs adds such a patch, install Emacs with "brew
;; install Emacs --cocoa".  Note that the patch included in homebrew does not
;; create a separate space for a full screen Emacs.

;; Keybindings
;; -----------
;;
;; S-Enter (right option key + enter) toggles full screen mode, or shows a
;; warning message if full screen mode is not supported.


;;; Code:

(eval-when-compile
  (require 'ido))

(when (stante-is-os-x)
  ;; Find GNU Coreutils (mostly for "ls --dired").
  (let ((gls (executable-find "gls")))
    (if gls
        (setq insert-directory-program gls)
      (message "GNU Coreutils not found.  Install coreutils \
with homebrew, or report an issue with M-x stante-report-issue.")))

  ;; Ignore OS X metadata files in IDO
  (after 'ido
    (add-to-list 'ido-ignore-files "\\`\\.DS_Store\\'")))

;; make this module a no-op if not on OS X GUI.
(after 'ns-win
  ;; Setup modifier maps for OS X
  (setq mac-option-modifier 'meta
        mac-command-modifier 'meta
        mac-function-modifier 'control
        mac-right-option-modifier 'none
        mac-right-command-modifier 'super)

  ;; Do not popup new frames
  (setq ns-pop-up-frames nil)

  (cond
   ((fboundp 'ns-toggle-fullscreen)
    ;; Patched fullschreen support in Homebrew Emacs 24.x
    (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen))
   ((fboundp 'toggle-frame-fullscreen)
    ;; Native fullscreen support in Emacs 24.3
    (global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen))))

(provide 'stante-os-x)

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-os-x.el ends here
