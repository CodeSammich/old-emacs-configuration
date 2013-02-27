;;; stante-init.el --- Stante Pede entry point -*- lexical-binding: t; -*-
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

;; Perform initial Stante Pede setup.
;;
;; Use M-x stante-browse-url to visit the Stante Pede site.

;; Stante Pede paths
;; -----------------
;;
;; Set some variables to provide standard paths to modules and users:
;;
;; `stante-dir' contains the path of the Stante Pede root directory, typically
;; "~/.emacs.d/".
;;
;; `stante-modules-dir' contains the path of the directory containing the
;; modules.  This directory is added to `load-path' so that modules can be
;; loaded directly with `require'.
;;
;; `stante-lib-dir' contains utility libraries used by Stante modules.  Almost
;; all functions in this directory are non-interactive.
;;
;; `stante-var-dir' points to a directory, where modules can store variable data
;; files, like autosave files, recent file lists or whatever.

;; All of theses paths end with a slash to allow for concatenation (see
;; `concat') with relative file names.

;; Package support
;; ---------------

;; Enable support for packages and add the MELPA archive.  See
;; http://melpa.milkbox.net/packages.

;; Custom file
;; -----------
;;
;; Move customize settings to `stante-dir'/custom.el to keep "init.el" free
;; of noise (see `custom-file').  Also load this file, if present.

(when (or (< emacs-major-version 24)
          (and (= emacs-major-version 24) (< emacs-minor-version 1)))
  (error "Stante Pede needs at least GNU Emacs 24.1, but this is Emacs %s.
Please install GNU Emacs 24.1 to use Stante Pede"
         emacs-version))

;;; Code:

;; Stante metadata
(defconst stante-url "https://github.com/lunaryorn/stante-pede"
  "The URL of the Stante Pede project.")

(defun stante-browse-url ()
  "Browse the URL of the Stante Pede project."
  (interactive)
  (browse-url stante-url))

;; Stante directories
(defconst stante-init-file
  (or load-file-name (locate-user-emacs-file "stante-init.el"))
  "The location of the `stante-init' file.")
(defconst stante-dir (file-name-directory stante-init-file)
  "The root directory of Stante Pede.")
(defconst stante-modules-dir (expand-file-name "modules/" stante-dir)
  "The directory containing all Stante Pede modules.")
(defconst stante-lib-dir (expand-file-name "lib/" stante-dir)
  "The directory containing Stante Pede utility libraries.")
(defconst stante-var-dir (expand-file-name "var/" stante-dir)
  "This folder stores automatically generated files.")
(defconst stante-autoloads-file (expand-file-name "stante-lib-autoloads.el"
                                                  stante-lib-dir)
  "Location of the autoload file for the Stante Pede Library.")

;; Stante files
(defconst stante-custom-file (expand-file-name "custom.el" stante-dir)
  "The location of the `custom-file' for Stante Pede.")

(add-to-list 'load-path stante-modules-dir)
(add-to-list 'load-path stante-lib-dir)

;; Load the library autoloads
(load stante-autoloads-file nil t)

;; Create cache directory
(unless (file-exists-p stante-var-dir)
  (make-directory stante-var-dir))

;; Move settings from customize UI out of place.
(eval-after-load 'cus-edit
  '(setq custom-file stante-custom-file))
;; Load customize settings without errors and loading messages
(load stante-custom-file t t)

;; Enable packages
(require 'package)
(package-initialize)
(require 'carton)
(load (expand-file-name "Carton" stante-dir))

;; Local Variables:
;; coding: utf-8
;; End:

;;; stante-init.el ends here
