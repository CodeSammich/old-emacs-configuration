;;; init.el --- Emacs configuration of Sebastian Wiesner -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://gihub.com/lunaryorn/.emacs.d
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

;; Emacs configuration of Sebastian Wiesner, functional programmer and Flycheck
;; maintainer.

;;; Code:

;;; Debugging
(setq message-log-max 10000)


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; Requires

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)


;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; And disable the site default settings
(setq inhibit-default-init t)

;; Warn if the current build is more than a week old
(run-with-idle-timer
 2 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))


;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive shell.  We use a login shell, even though we have
      ;; our paths setup in .zshenv.  However, OS X adds global settings to the
      ;; login profile.  Notably, this affects /usr/texbin from MacTeX
      (setq exec-path-from-shell-arguments '("-l")))

    (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH" "SBT_OPTS" "JAVA_OPTS"))
      (add-to-list 'exec-path-from-shell-variables var))

    (exec-path-from-shell-initialize)

    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.
    (with-eval-after-load 'info
      (dolist (dir (parse-colon-path (getenv "INFOPATH")))
        (when dir
          (add-to-list 'Info-directory-list dir))))))


;;; Customization
(defconst lunaryorn-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file lunaryorn-custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load lunaryorn-custom-file 'no-error 'no-message))


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the
                                        ; workspace
        mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        ;; Just in case we ever need these keys
        mac-function-modifier 'hyper))

(use-package lunaryorn-osx              ; Personal OS X tools
  :if (eq system-type 'darwin)
  :load-path "lisp/"
  :defer t)

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))


;;; Fonts

;; We use these fonts:
;;
;; - Monoid (http://larsenwork.com/monoid/) as default
;; - XITS Math (https://github.com/khaledhosny/xits-math) as fallback for math
;;
;; Source Code Pro (https://github.com/adobe-fonts/source-code-pro) is a good
;; monospace font, too.  An alternative is Consolas.  Another great monospace
;; font is and Pragmata Pro (http://www.fsd.it/fonts/pragmatapro.htm,
;; proprietary, around 200$).
;;
;; Currently this setup only works for OS X, as we rely on Apple's Emoji and
;; Symbol fonts.
;;
;; TODO:  Find Emoji and symbol fonts for Linux and Windows

;; Use Monoid Retina as default font
(set-frame-font "Monoid-12:weight=light" nil t)

;; Font setup
(defun lunaryorn-configure-fonts (frame)
  "Set up fonts for FRAME.

Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ

  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Monoid" :weight 'light)
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(when-let (frame (selected-frame))
  (lunaryorn-configure-fonts frame))
(add-hook 'after-make-frame-functions #'lunaryorn-configure-fonts)


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Hello there!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package lunaryorn-scratch          ; My logo in the scratch buffer
  :commands (lunaryorn-insert-logo
             lunaryorn-insert-logo-into-scratch)
  :init (add-hook 'after-init-hook #'lunaryorn-insert-logo-into-scratch))

(use-package solarized                  ; My colour theme
  :ensure solarized-theme
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (progn (setq solarized-use-variable-pitch nil
               ;; Prefer italics over bold
               solarized-use-less-bold t
               solarized-use-more-italic t
               solarized-distinct-doc-face t ; Emphasize docstrings
               ;; I find different font sizes irritating.
               solarized-height-minus-1 1.0
               solarized-height-plus-1 1.0
               solarized-height-plus-2 1.0
               solarized-height-plus-3 1.0
               solarized-height-plus-4 1.0)

         (load-theme 'solarized-light 'no-confirm)))

(use-package zenburn                    ; Another good low-contrast theme
  :disabled t
  :ensure zenburn-theme
  :defer t
  :init (load-theme 'zenburn 'no-confirm))

(bind-key "C-c t v" #'variable-pitch-mode)


;;; Key setup
(use-package evil                       ; VIM — Let's try this some day
  :ensure t
  :disabled t
  :init (evil-mode 1))

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.4
                which-key-key-replacement-alist
                '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                  ("up"                    . "↑")
                  ("right"                 . "→")
                  ("down"                  . "↓")
                  ("left"                  . "←")
                  ("DEL"                   . "⌫")
                  ("deletechar"            . "⌦")
                  ("RET"                   . "⏎"))
                which-key-description-replacement-alist
                '(("Prefix Command" . "prefix")
                  ("\\`\\?\\?\\'"   . "λ")
                  ;; Remove my personal prefix from all bindings, since it's
                  ;; only there to avoid name clashes, but doesn't add any value
                  ;; at all
                  ("lunaryorn-"     . "")))
  :diminish (which-key-mode . " Ⓚ"))

;; Define prefix commands for my personal key binding groups.  Not specifically
;; important, but plays better with which-key, as it shows the names of prefix
;; commands in its popup
(defmacro lunaryorn-define-group (prefix name &optional map)
  "Define a group at PREFIX with NAME in MAP."
  (let ((command (intern (format "group:%s" name))))
    `(progn
       (define-prefix-command ',command)
       (bind-key ,prefix #',command ,map))))

(lunaryorn-define-group "C-c a" applications)
(lunaryorn-define-group "C-c a o" org)
(lunaryorn-define-group "C-c a S" stackexchange)
(lunaryorn-define-group "C-c a w" eww)
(lunaryorn-define-group "C-c b" buffers)
(lunaryorn-define-group "C-c c" compile-and-comments)
(lunaryorn-define-group "C-c e" errors)
(lunaryorn-define-group "C-c f" files)
(lunaryorn-define-group "C-c f v" file-variables)
(lunaryorn-define-group "C-c h" help)
(lunaryorn-define-group "C-c i" insertion)
(lunaryorn-define-group "C-c k" sexps)
(lunaryorn-define-group "C-c m" major-mode)
(lunaryorn-define-group "C-c n" navigation)
(lunaryorn-define-group "C-c o" multiple-cursors)
(lunaryorn-define-group "C-c p" projects)
(lunaryorn-define-group "C-c s" search-and-symbols)
(lunaryorn-define-group "C-c t" toggles)
(lunaryorn-define-group "C-c v" version-control)
(lunaryorn-define-group "C-c w" windows-and-frames)
(lunaryorn-define-group "C-c x" text)
(lunaryorn-define-group "C-c x a" align)


;; Package manager and init file
(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . package-list-packages-no-fetch))
  :config
  ;; Don't ask for a token, please, and don't bug me about asynchronous updates
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)


;;; The mode line
(setq-default header-line-format
              '(which-func-mode ("" which-func-format " "))
              mode-line-format
              '("%e" mode-line-front-space
                "🐷 "                   ; My branding :)
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (projectile-mode projectile-mode-line)
                (vc-mode (:propertize (:eval vc-mode) face italic))
                " "
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (ensime-mode (" " (:eval
                                   (lunaryorn-scala-ensime-mode-line-status))))
                (isearch-mode " ")
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (anzu--update-mode-line))))
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              ;; Remove which func from the mode line, since we have it in the
              ;; header line
              mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info))

;; Standard stuff
(line-number-mode)
(column-number-mode)

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name in header line
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))


;;; Minibuffer and Helm
(setq history-length 1000               ; Store more history
      use-dialog-box nil                ; Never use dialogs for minibuffer input
      )

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(use-package helm                       ; Powerful minibuffer input framework
  :ensure t
  :bind (("C-c c b" . helm-resume))
  :init (progn (helm-mode 1)
               (with-eval-after-load 'helm-config
                 (warn "`helm-config' loaded! Get rid of it ASAP!")))
  :config (setq helm-split-window-in-side-p t)
  :diminish helm-mode)

(use-package helm-misc                  ; Misc helm commands
  :ensure helm
  :bind (([remap switch-to-buffer] . helm-mini)))

(use-package helm-command               ; M-x in Helm
  :ensure helm
  :bind (([remap execute-extended-command] . helm-M-x)))

;; (use-package helm-eval                  ; Evaluate expressions with Helm
;;   :ensure helm
;;   :bind (("C-c c M-:" . helm-eval-expression-with-eldoc)
;;          ("C-c c *"   . helm-calcul-expression)))

(use-package helm-color                 ; Input colors with Helm
  :ensure helm
  :bind (("C-c i C" . helm-colors)))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c i 8" . helm-unicode))


;;; Buffer, Windows and Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))
(setq-default line-spacing 0.1)         ; A bit more spacing between lines

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
                      "*compilation"      ; Compilation buffers
                      "*Warnings*"        ; Emacs warnings
                      "*sbt"              ; SBT REPL and compilation buffer
                      "*SQL"              ; SQL REPL
                      "*shell"            ; Shell window
                      ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(use-package frame                      ; Frames
  :bind (("C-c w f" . toggle-frame-fullscreen))
  :init (progn
          ;; Kill `suspend-frame'
          (global-set-key (kbd "C-z") nil)
          (global-set-key (kbd "C-x C-z") nil))
  :config (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package lunaryorn-buffers          ; Personal buffer tools
  :load-path "lisp/"
  :commands (lunaryorn-do-not-kill-important-buffers)
  :init (add-hook 'kill-buffer-query-functions
                  #'lunaryorn-do-not-kill-important-buffers))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package helm-buffers               ; Helm for buffer management
  :ensure helm
  :defer t
  :config (setq helm-buffers-fuzzy-matching t))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config (setq ibuffer-formats
                '((mark modified read-only vc-status-mini " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        (vc-status 16 16 :left)
                        " "
                        filename-and-process)
                  (mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " " filename-and-process)
                  (mark " "
                        (name 16 -1)
                        " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package window                     ; Standard window functions
  :bind (("C-c w =" . balance-windows)
         ("C-c w k" . delete-window)
         ("C-c w /" . split-window-right)
         ("C-c w -" . split-window-below)
         ("C-c w m" . delete-other-windows)))

(use-package lunaryorn-window           ; Personal window utilities
  :load-path "lisp/"
  :defer t
  :bind (("C-c w q" . lunaryorn-quit-bottom-side-windows)
         ("C-c w d" . lunaryorn-toggle-current-window-dedication)
         ("C-c w b" . lunaryorn-switch-to-minibuffer-window)))

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down)))

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

(use-package ace-window                 ; Fast window switching
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c w w" . ace-window)))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config (progn
            ;; Save desktops a minute after Emacs was idle.
            (setq desktop-auto-save-timeout 60)

            (dolist (mode '(magit-mode git-commit-mode))
              (add-to-list 'desktop-modes-not-to-save mode))))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t r" . writeroom-mode)))


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files                      ; Core commands for files
  :bind (("C-c f z" . revert-buffer)
         ("C-c f /" . revert-buffer))
  :config
  ;; Use GNU ls for Emacs
  (when-let (gnu-ls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gnu-ls)))

(use-package ffap                       ; Find files at point
  :defer t
  ;; Please stop pinging random hosts!  See
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

(use-package tramp                      ; Access remote files
  :defer t
  :config
  ;; Store auto-save files locally
  (setq tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

(use-package server                     ; The server of `emacsclient'
  :defer t
  :init (server-mode)
  :diminish (server-buffer-clients . " ⓒ"))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (progn
    (require 'dired-x)

    (setq dired-auto-revert-buffer t    ; Revert on re-visiting
          ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
          ;; uses human-readable sizes, and `-F' appends file-type classifiers
          ;; to file names (for better highlighting)
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks t   ; -F marks links with @
          ;; Inhibit prompts for simple recursive operations
          dired-recursive-copies 'always
          ;; Auto-copy to other Dired split window
          dired-dwim-target t)

    (when (or (memq system-type '(gnu gnu/linux))
              (string= (file-name-nondirectory insert-directory-program) "gls"))
      ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
      ;; `--group-directories-first' lists directories before files, and `-v'
      ;; sorts numbers in file names naturally, i.e. "image1" goes before
      ;; "image02"
      (setq dired-listing-switches
            (concat dired-listing-switches " --group-directories-first -v")))))

(use-package dired-x                    ; Additional tools for Dired
  :bind (("C-x C-j" . dired-jump))
  :init (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  (progn
    (setq dired-omit-verbose nil)        ; Shut up, dired

    (when (eq system-type 'darwin)
      ;; OS X bsdtar is mostly compatible with GNU Tar
      (setq dired-guess-shell-gnutar "tar"))

    ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
    ;; a very peculiar way of registering its lighter explicitly in
    ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
    ;; isn't there yet after dired-omit-mode is loaded.
    (add-function :after (symbol-function 'dired-omit-startup)
                  (lambda () (diminish 'dired-omit-mode " ⓞ"))
                  '((name . dired-omit-mode-diminish)))))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t))

(use-package helm-files                 ; Helm for file finding
  :ensure helm
  :defer t
  :bind (([remap find-file] . helm-find-files)
         ("C-c f r"         . helm-recentf))
  :config (setq helm-recentf-fuzzy-match t
                ;; Use recentf to find recent files
                helm-ff-file-name-history-use-recentf t
                ;; Find library from `require', `declare-function' and friends
                helm-ff-search-library-in-sexp t))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config (progn (dolist (name '(".cask"
                                 ".vagrant"
                                 ".ensime_cache" ".ensime"))
                   ;; Ignore some additional directories
                   (add-to-list 'ignoramus-file-basename-exact-names name))
                 (ignoramus-setup)))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode)
  :config (setq hardhat-mode-lighter " Ⓗ"))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c f b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1))

(use-package recentf                    ; Save recently visited files
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'" ; Package files
                              "/itsalltext/" ; It's all text temp files
                              ;; And all other kinds of boring files
                              #'ignoramus-boring-p)))

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1))

(setq view-read-only t)                 ; View read-only files

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config (progn (setq auto-revert-verbose nil ; Shut up, please!
                       ;; Revert Dired buffers, too
                       global-auto-revert-non-file-buffers t)

                 (when (eq system-type 'darwin)
                   ;; File notifications aren't supported on OS X
                   (setq auto-revert-use-notify nil)))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)

(use-package reveal-in-finder           ; Reveal current buffer in finder
  :ensure t
  :bind (("C-c f f" . reveal-in-finder)))

(use-package lunaryorn-files            ; Personal file tools
  :load-path "lisp/"
  :commands (lunaryorn-recompile-packages)
  :bind (("C-c f D" . lunaryorn-delete-file-and-buffer)
         ("C-c f i" . lunaryorn-open-in-intellij)
         ("C-c f o" . lunaryorn-launch-dwim)
         ("C-c f R" . lunaryorn-rename-file-and-buffer)
         ("C-c f w" . lunaryorn-copy-filename-as-kill)
         ("C-c f u" . lunaryorn-find-user-init-file-other-window)
         ("C-c f ." . lunaryorn-browse-feature-url)))

;;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Navigation and scrolling
(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c s s" . avy-isearch)
         ("C-c j" . avy-goto-word-1)
         ("C-c l" . avy-goto-line)
         ("C-c n b" . avy-pop-mark)
         ("C-c n j" . avy-goto-char-2)
         ("C-c n w" . avy-goto-word-1)))

(use-package ace-link                   ; Fast link jumping
  :ensure t
  :defer t
  :init (progn (with-eval-after-load 'info
                 (bind-key "C-c m l" #'ace-link-info Info-mode-map))

               (with-eval-after-load 'help-mode
                 (defvar help-mode-map)  ; Silence the byte compiler
                 (bind-key "C-c m l" #'ace-link-help help-mode-map))))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish (outline-minor-mode . " Ⓞ"))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

(use-package helm-imenu                 ; Helm frontend for imenu
  :ensure helm
  :bind (("C-c n i" . helm-imenu-in-all-buffers)
         ("C-c n t" . helm-imenu))
  :config (setq helm-imenu-fuzzy-match t
                ;; Don't automatically jump to candidate if only one match,
                ;; because it makes the behaviour of this command unpredictable,
                ;; and prevents me from getting an overview over the buffer if
                ;; point is on a matching symbol.
                helm-imenu-execute-action-at-once-if-one nil))


;;; Basic editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " Ⓕ")

(use-package lunaryorn-simple           ; Personal editing helpers
  :load-path "lisp/"
  :bind (([remap kill-whole-line]        . lunaryorn-smart-kill-whole-line)
         ([remap move-beginning-of-line] . lunaryorn-back-to-indentation-or-beginning-of-line)
         ("C-<backspace>"                . lunaryorn-smart-backward-kill-line)
         ("C-S-j"                        . lunaryorn-smart-open-line)
         ;; Additional utilities
         ("C-c i d"                      . lunaryorn-insert-current-date)
         ("C-c i m"                      . lunaryorn-insert-mit/x11))
  :commands (lunaryorn-auto-fill-comments-mode)
  ;; Auto-fill comments in programming modes
  :init (add-hook 'prog-mode-hook #'lunaryorn-auto-fill-comments-mode))

(use-package indent                     ; Built-in indentation
  :bind (("C-c x i" . indent-region)))

(use-package helm-ring                  ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . " Ⓦ"))

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column         ; Fill column wrapping for Visual Line Mode
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package zop-to-char                ; Better zapping
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package align                      ; Align text in buffers
  :bind (("C-c x a a" . align)))

(use-package lunaryorn-align
  :load-path "lisp/"
  :bind (("C-c x a r" . lunaryorn-align-repeat)
         ("C-c x a m" . lunaryorn-align-repeat-math-oper)
         ("C-c x a ." . lunaryorn-align-repeat-decimal)
         ("C-c x a ," . lunaryorn-align-repeat-comma)
         ("C-c x a ;" . lunaryorn-align-repeat-semicolon)
         ("C-c x a :" . lunaryorn-align-repeat-colon)
         ("C-c x a =" . lunaryorn-align-repeat-equal)
         ("C-c x a &" . lunaryorn-align-repeat-ampersand)
         ("C-c x a |" . lunaryorn-align-repeat-bar)
         ("C-c x a (" . lunaryorn-align-repeat-left-paren)
         ("C-c x a )" . lunaryorn-align-repeat-right-paren)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
         ("C-c o a"     . mc/vertical-align)
         ("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o n"     . mc/mark-next-like-this)
         ("C-c o p"     . mc/mark-previous-like-this)
         ("C-c o r"     . vr/mc-mark)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o C-s"   . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                      face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init (global-undo-tree-mode)
  :diminish (undo-tree-mode . " ⓤ"))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)


;;; Paired delimiters
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :init (progn (smartparens-global-mode)
               (show-smartparens-global-mode)

               (dolist (hook '(inferior-emacs-lisp-mode-hook
                               emacs-lisp-mode-hook))
                 (add-hook hook #'smartparens-strict-mode)))
  :config (progn
            (setq sp-autoskip-closing-pair 'always
                  ;; Don't kill entire symbol on C-k
                  sp-hybrid-kill-entire-symbol nil)

            (use-package lunaryorn-smartparens ; Personal Smartparens extensions
              :load-path "lisp/"
              :config (lunaryorn-smartparens-bind-keys)))
  :diminish (smartparens-mode . " ⓟ"))


;;; Highlights and fontification
(defun lunaryorn-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.

Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun lunaryorn-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(use-package whitespace                 ; Highlight bad whitespace
  :bind (("C-c t w" . whitespace-mode))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'lunaryorn-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)     ; Use `fill-column' for overlong lines
  :diminish (whitespace-mode . " ⓦ"))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c n n" . highlight-symbol-next-in-defun)
   ("C-c n p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init (progn (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
               (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)


;;; Skeletons, completion and expansion

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates.  Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            lunaryorn-try-complete-lisp-symbol-without-namespace))

    (use-package lunaryorn-hippie-exp   ; Custom expansion functions
      :load-path "lisp/"
      :commands (lunaryorn-try-complete-lisp-symbol-without-namespace))))

(use-package yasnippet                  ; Snippets
  :ensure t
  :defer t
  :diminish (yas-minor-mode . " Ⓨ"))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
          company-tooltip-flip-when-above t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t))
  :diminish company-mode)

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-quickhelp-mode)))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-statistics-mode)))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Add backends for math characters
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-emoji              ; Emojis completion like Github/Slack
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-emoji)))

(use-package helm-company               ; Helm frontend for company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Use Company for completion
          (bind-key [remap completion-at-point] #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-active-map)))

(use-package auto-insert                ; Automatic insertion into new files
  :defer t
  :bind (("C-c i a" . auto-insert)))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :bind (("C-c i c" . copyright-update))
  ;; Update copyright when visiting files
  :init (add-hook 'find-file-hook #'copyright-update)
  ;; Use ranges to denote consecutive years
  :config (setq copyright-year-ranges t
                copyright-names-regexp (regexp-quote user-full-name)))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (progn
    (setq ispell-program-name (if (eq system-type 'darwin)
                                  (executable-find "aspell")
                                (executable-find "hunspell"))
          ispell-dictionary "en_GB"     ; Default dictionnary
          ispell-silently-savep t       ; Don't ask when saving the private dict
          ;; Increase the height of the choices window to take our header line
          ;; into account.
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available.  Install Hunspell or ASpell for OS X."))))

(use-package flyspell                   ; On-the-fly spell checking
  :bind (("C-c t s" . flyspell-mode))
  :init (progn (dolist (hook '(text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil)
    ;; Undefine mouse buttons which get in the way
    (define-key flyspell-mouse-map [down-mouse-2] nil)
    (define-key flyspell-mouse-map [mouse-2] nil))
  :diminish (flyspell-mode . " ⓢ"))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c e l" . list-flycheck-errors)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error)
         ("C-c e c" . flycheck-buffer)
         ("C-c e C" . flycheck-clear)
         ("C-c e f" . flycheck-first-error)
         ("C-c e w" . flycheck-copy-errors-as-kill)
         ("C-c t f" . flycheck-mode))
  :init (global-flycheck-mode)
  :config (progn
            (setq flycheck-display-errors-function
                  #'flycheck-display-error-messages-unless-error-list)

            ;; Use italic face for checker name
            (set-face-attribute 'flycheck-error-list-checker-name nil
                                :inherit 'italic))
  :diminish (flycheck-mode . " Ⓢ"))

(use-package lunaryorn-flycheck         ; Personal Flycheck extensions
  :load-path "lisp/"
  :commands (lunaryorn-discard-undesired-html-tidy-error
             lunaryorn-flycheck-mode-line-status
             lunaryorn-use-js-executables-from-node-modules
             lunaryorn-flycheck-set-load-path-for-user-configuration)
  :init (progn
          ;; Don't highlight undesired errors from html tidy
          (add-hook 'flycheck-process-error-functions
                    #'lunaryorn-discard-undesired-html-tidy-error)
          (dolist (hook-fn '(lunaryorn-use-js-executables-from-node-modules
                             lunaryorn-flycheck-set-load-path-for-user-configuration))
            (add-hook 'flycheck-mode-hook hook-fn))

          (with-eval-after-load 'flycheck
            (setq flycheck-mode-line
                  '(:eval (lunaryorn-flycheck-mode-line-status))))))

(use-package helm-flycheck              ; Helm frontend for Flycheck errors
  :ensure t
  :bind (("C-c e h" . helm-flycheck)))


;;; Text editing
(use-package tildify                    ; Insert non-breaking spaces on the fly
  :bind (("C-c x t" . tildify-region))
  :init (dolist (hook '(markdown-mode-hook
                        latex-mode-hook
                        rst-mode-hook))
          (add-hook hook #'tildify-mode))
  ;; Use the right space for LaTeX
  :config (add-hook 'latex-mode-hook
                    (lambda () (setq-local tildify-space-string "~"))))

(use-package typo                       ; Automatically use typographic quotes
  :ensure t
  :init (progn
          (typo-global-mode)

          (dolist (hook '(markdown-mode-hook
                          rst-mode-hook))
            (add-hook hook 'typo-mode)))
  :diminish (typo-mode . " Ⓣ"))


;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-parse-self t              ; Parse documents to provide completion
                                        ; for packages, etc.
          TeX-auto-save t               ; Automatically save style information
          TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
          TeX-electric-math '("\\(" "\\)")
          ;; Don't insert magic quotes right away.
          TeX-quote-after-quote t
          ;; Don't ask for confirmation when cleaning
          TeX-clean-confirm nil
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex)
    (setq-default TeX-master nil        ; Ask for the master file
                  TeX-engine 'luatex    ; Use a modern engine
                  ;; Redundant in 11.88, but keep for older AUCTeX
                  TeX-PDF-mode t)

    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")))

(use-package tex-buf                    ; TeX buffer management
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style                  ; TeX style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
        LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; TeX folding
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; TeX mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word) (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  (progn
    ;; Teach TeX folding about KOMA script sections
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                              (,(rx (0+ space) "\\subsection*{") 3)
                              (,(rx (0+ space) "\\subsubsection*{") 4)
                              (,(rx (0+ space) "\\minisec{") 5))
          ;; No language-specific hyphens please
          LaTeX-babel-hyphen nil)

    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))    ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :init (with-eval-after-load 'latex
          (auctex-latexmk-setup)))

(use-package auctex-skim                ; Skim as viewer for AUCTeX
  :load-path "lisp/"
  :commands (auctex-skim-select)
  :init (with-eval-after-load 'tex
          (auctex-skim-select)))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

(defun lunaryorn-reftex-find-ams-environment-caption (environment)
  "Find the caption of an AMS ENVIRONMENT."
  (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
    ;; Go to the beginning of the label first
    (re-search-backward re)
    (goto-char (match-end 0)))
  (if (not (looking-at (rx (zero-or-more space) "[")))
      (error "Environment %s has no title" environment)
    (let ((beg (match-end 0)))
      ;; Move point onto the title start bracket and move over to the end,
      ;; skipping any other brackets in between, and eventually extract the text
      ;; between the brackets
      (goto-char (1- beg))
      (forward-list)
      (buffer-substring-no-properties beg (1- (point))))))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
          ;; Automatically derive labels, and prompt for confirmation
          reftex-insert-label-flags '(t t)
          reftex-label-alist
          '(
            ;; Additional label definitions for RefTeX.
            ("definition" ?d "def:" "~\\ref{%s}"
             lunaryorn-reftex-find-ams-environment-caption
             ("definition" "def.") -3)
            ("theorem" ?h "thm:" "~\\ref{%s}"
             lunaryorn-reftex-find-ams-environment-caption
             ("theorem" "th.") -3)
            ("example" ?x "ex:" "~\\ref{%s}"
             lunaryorn-reftex-find-ams-environment-caption
             ("example" "ex") -3)
            ;; Algorithms package
            ("algorithm" ?a "alg:" "~\\ref{%s}"
             "\\\\caption[[{]" ("algorithm" "alg") -3)))

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
                   '(biblatex "The biblatex package"
                              ((?\C-m . "\\cite[]{%l}")
                               (?t . "\\textcite{%l}")
                               (?a . "\\autocite[]{%l}")
                               (?p . "\\parencite{%l}")
                               (?f . "\\footcite[][]{%l}")
                               (?F . "\\fullcite[]{%l}")
                               (?x . "[]{%l}")
                               (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)


;;; Other markup languages
(use-package rst                        ; ReStructuredText
  :defer t
  :config
  ;; Indent with 3 spaces after all kinds of literal blocks
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)

  (bind-key "C-=" nil rst-mode-map)
  ;; For similarity with AUCTeX
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  ;; …and with Markdown Mode
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

(use-package markdown-mode              ; Markdown
  :ensure t
  ;; Just no, dear Markdown Mode.  Don't force that bastard Github dialect upon
  ;; me!
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (progn
    ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
    (let ((stylesheet (expand-file-name
                       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq markdown-command
            (mapconcat #'shell-quote-argument
                       `("pandoc" "--toc" "--section-divs"
                         "--css" ,(concat "file://" stylesheet)
                         "--standalone" "-f" "markdown" "-t" "html5")
                       " ")))

    ;; No filling in GFM, because line breaks are significant.
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    ;; Use visual lines instead
    (add-hook 'gfm-mode-hook #'visual-line-mode)
    (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
    (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
    ;; Mode.
    (bind-key "M-q" #'ignore gfm-mode-map)))

(use-package lunaryorn-markdown         ; Personal Markdown extensions
  :load-path "lisp/"
  :commands (lunaryorn-markdown-post-header)
  :init (with-eval-after-load 'markdown-mode
            (bind-key "C-c m h" #'lunaryorn-markdown-post-header
                      markdown-mode-map)))

(use-package jira-markup-mode           ; Jira markup
  :ensure t
  :defer t)

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t
  :config

  (use-package lunaryorn-json           ; Personal JSON tools
    :load-path "lisp/"
    :commands (lunaryorn-json-chef-role)
    :init (bind-key "C-c m r" #'lunaryorn-json-chef-role
                    json-mode-map)))

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package systemd                    ; Mode for systemd unit files
  :ensure t
  :defer t)


;;; Programming utilities
(use-package prog-mode                  ; Prog Mode
  :bind (("C-c t p" . prettify-symbols-mode)))

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c C" . compile)
         ("C-c c r" . recompile))
  :config (progn
            (setq compilation-ask-about-save nil
                  ;; Kill old compilation processes before starting new ones,
                  compilation-always-kill t
                  ;; Automatically scroll
                  compilation-scroll-output 'first-error
                  ;; Skip over warnings and info messages in compilation
                  compilation-skip-threshold 2
                  ;; Don't freeze when process reads from stdin
                  compilation-disable-input t
                  ;; Show three lines of context around the current message
                  compilation-context-lines 3)

            (use-package lunaryorn-compile ; Personal helpers for compilation
              :load-path "lisp/"
              ;; Colorize output of Compilation Mode, see
              ;; http://stackoverflow.com/a/3072831/355252
              :config (add-hook 'compilation-filter-hook
                                #'lunaryorn-colorize-compilation-buffer))))

(use-package helm-make
  :ensure t
  :bind (("C-c c c" . helm-make-projectile)))

(use-package elide-head                 ; Elide lengthy GPL headers
  :bind (("C-c t e" . elide-head))
  :init (add-hook 'prog-mode-hook #'elide-head))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish (eldoc-mode . " ⓓ"))

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-restclient)))


;;; Emacs Lisp
(bind-key "C-c t d" #'toggle-debug-on-error)

(use-package helm-elisp                 ; Helm commands for Emacs Lisp
  :ensure helm
  :bind (("C-c f l" . helm-locate-library)
         ("C-c h a" . helm-apropos)))

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :config (progn (dolist (key '("C-c C-d d" "C-c C-d C-d"))
                   (define-key elisp-slime-nav-mode-map (kbd key) nil))

                 (bind-key "C-c h ."
                           #'elisp-slime-nav-describe-elisp-thing-at-point))
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package pcre2el                    ; Convert regexps to RX and back
  :disabled t
  :ensure t
  :init (rxt-global-mode))

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :defer t
  :init (with-eval-after-load 'elisp-mode
          (bind-key "C-c m m e" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c m m e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c a z" . ielm)))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :config (progn
            (require 'ert)

            (bind-key "C-c m e r" #'eval-region emacs-lisp-mode-map)
            (bind-key "C-c m e b" #'eval-buffer emacs-lisp-mode-map)
            (bind-key "C-c m e e" #'eval-last-sexp emacs-lisp-mode-map)
            (bind-key "C-c m e f" #'eval-defun emacs-lisp-mode-map)))

(use-package lunaryorn-elisp ; Personal tools for Emacs Lisp
              :load-path "lisp/"
              :init (progn
                      (add-hook 'emacs-lisp-mode-hook
                                #'lunaryorn-add-use-package-to-imenu)

                      (with-eval-after-load 'elisp-mode
                        (bind-key "C-c m f" #'lunaryorn-elisp-find-cask-file
                                  emacs-lisp-mode-map))))


;;; Scala

(use-package scala-mode2                ; Scala editing
  :ensure t
  :defer t
  :config (progn (setq scala-indent:default-run-on-strategy
                       scala-indent:operator-strategy)

                 (bind-key "C-c m b s" #'ensime scala-mode-map)))

(use-package flycheck-auto-scalastyle   ; Scalastyle setup
  :load-path "lisp/"
  :defer t
  :commands (flycheck-auto-scalastyle-configure
             flycheck-auto-scalastyle-setup)
  :init (with-eval-after-load 'scala-mode2
          (add-hook 'flycheck-mode-hook #'flycheck-auto-scalastyle-setup)))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :init (with-eval-after-load 'scala-mode2
          (bind-key "C-c m b c" #'sbt-command scala-mode-map)
          (bind-key "C-c m b r" #'sbt-run-previous-command scala-mode-map))
  :config (progn
            (setq sbt:display-command-buffer nil)

            ;; Disable Smartparens Mode in SBT buffers, because it frequently
            ;; hangs while trying to find matching delimiters
            (add-hook 'sbt-mode-hook
                      (lambda ()
                        (when (and (bound-and-true-p smartparens-mode)
                                   (fboundp 'smartparens-mode))
                          (smartparens-mode -1))))))

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :defer t
  :config (progn
            ;; Automatically open new Ensime sessions if needed
            (setq ensime-auto-connect 'always
                  ;; Automatically regenerate ensime configuration
                  ensime-auto-generate-config t)

            ;; Enable Ensime for all Scala buffers.  We don't do this in :init,
            ;; because `ensime-mode' isn't autoloaded, and ensime-mode makes no
            ;; sense before the first session was started anyway
            (add-hook 'scala-mode-hook #'ensime-mode)

            ;; Add binding to shutdown Ensime
            (bind-key "C-c m b s" #'ensime-shutdown ensime-mode-map)
            (bind-key "C-c m b S" #'ensime-shutdown ensime-mode-map)
            (bind-key "C-c m b l" #'ensime-reload ensime-mode-map)

            ;; Free M-n and M-p again
            (bind-key "M-n" nil ensime-mode-map)
            (bind-key "M-p" nil ensime-mode-map)
            ;; Ensime bindings
            (bind-key "C-c m t" #'ensime-inspect-type-at-point ensime-mode-map)
            (bind-key "C-c m T" #'ensime-print-type-at-point ensime-mode-map)
            (bind-key "C-c m r" #'ensime-show-uses-of-symbol-at-point
                      ensime-mode-map)
            (bind-key "C-c m j" #'ensime-edit-definition ensime-mode-map)
            (bind-key "C-c m i" #'ensime-import-type-at-point ensime-mode-map)
            (bind-key "C-c m n" #'ensime-forward-note ensime-mode-map)
            (bind-key "C-c m p" #'ensime-backward-note ensime-mode-map)
            (bind-key "C-c m l" #'ensime-print-errors-at-point ensime-mode-map)
            (bind-key "C-c m z" #'ensime-inf-switch ensime-mode-map))
  :diminish (ensime-mode . " Ⓔ"))

(use-package ensime-sbt                 ; SBT integration for Ensime
  :ensure ensime
  :defer t
  ;; Compile on save.  My projects are small enough :)
  :config (setq ensime-sbt-perform-on-save "test:compile"))

(use-package lunaryorn-scala            ; Personal Scala tools
  :load-path "lisp/"
  :commands (lunaryorn-scala-ensime-mode-line-status
             lunaryorn-scala-pop-to-sbt-frame)
  :defer t
  :config (with-eval-after-load 'scala-mode2
            (bind-key "C-c m s" #'lunaryorn-scala-pop-to-sbt-frame
                      scala-mode-map)))

(use-package flycheck-ensime            ; Ensime-based checker for Flycheck
  :disabled t
  :load-path "lisp/"
  :defer t)


;;; Python
(use-package python                     ; Python editing
  :defer t
  :config
  (progn
    ;; PEP 8 compliant filling rules, 79 chars maximum
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
    (add-hook 'python-mode-hook #'subword-mode)

    (let ((ipython (executable-find "ipython")))
      (if ipython
          (setq python-shell-interpreter ipython)
        (warn "IPython is missing, falling back to default python")))))

(use-package lunaryorn-virtualenv       ; Personal virtualenv tools
  :load-path "lisp/"
  :commands (lunaryorn-virtualenv-init-from-workon-home)
  :init (add-hook 'python-mode-hook #'lunaryorn-virtualenv-init-from-workon-home))

(use-package flycheck-virtualenv        ; Setup Flycheck by virtualenv
  :load-path "lisp/"
  :commands (flycheck-virtualenv-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-anaconda)))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)


;;; Ruby
(use-package inf-ruby                   ; Ruby REPL
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; Easily switch to Inf Ruby from compilation modes to Inf Ruby
  (inf-ruby-switch-setup))

(use-package robe                       ; Ruby backend for Emacs
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-robe)))


;;; Rust
(use-package rust-mode                  ; Rust
  :ensure t
  :defer t)

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)


;;; Haskell

;; This Haskell setup needs:
;;
;; stack install hasktags haskell-docs hoogle hindent
;;
;; Additionally, to be installed from source:
;;
;; - https://github.com/chrisdone/ghci-ng

(use-package haskell-mode               ; Haskell editing
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'haskell-mode-hook #'subword-mode)           ; Subword navigation
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
                                        ; declarations
    ;; Insert module templates into new buffers
    (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)

    ;; Automatically run hasktags
    (setq haskell-tags-on-save t
          haskell-process-type 'stack-ghci ; Use stack for interaction
          ;; Suggest adding/removing imports as by GHC warnings and Hoggle/GHCI
          ;; loaded modules respectively
          haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-use-presentation-mode t ; Don't clutter the echo area
          haskell-process-show-debug-tips nil     ; Disable tips
          haskell-process-log t                   ; Log debugging information
          ;; Suggest imports automatically with Hayoo.  Hayoo is slower because
          ;; it's networked, but it covers all of hackage, which is really an
          ;; advantage.
          haskell-process-suggest-hoogle-imports nil
          haskell-process-suggest-hayoo-imports t)

    (when-let (ghci-ng (executable-find "ghci-ng"))
      ;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
      (setq haskell-process-path-ghci ghci-ng)
      (add-to-list 'haskell-process-args-cabal-repl
                   (concat "--with-ghc=" ghci-ng)))

    (bind-key "C-c m d" #'haskell-describe haskell-mode-map)
    (bind-key "C-c m i" #'haskell-navigate-imports haskell-mode-map)
    (bind-key "C-c m f" #'haskell-cabal-visit-file haskell-mode-map)))

(use-package haskell                    ; Haskell tools
  :ensure haskell-mode
  :defer t
  :init (dolist (hook '(haskell-mode-hook haskell-cabal-mode-hook))
          (add-hook hook #'interactive-haskell-mode))
  :config
  (progn
    (bind-key "C-c m t" #'haskell-mode-show-type-at
              interactive-haskell-mode-map)
    (bind-key "C-c m j" #'haskell-mode-goto-loc
              interactive-haskell-mode-map)
    (bind-key "C-c m r" #'haskell-mode-find-uses
              interactive-haskell-mode-map)))

(use-package haskell-interactive-mode   ; Haskell REPL interaction
  :ensure haskell-mode
  :defer t
  :config (add-hook 'haskell-interactive-mode-hook #'subword-mode))

(use-package haskell-simple-indent      ; Primitive Haskell indentation
  :ensure haskell-mode
  :disabled t
  :defer t
  :init (add-hook 'haskell-mode-hook #'haskell-simple-indent-mode))

(use-package haskell-indentation        ; Intelligent Haskell indentation
  :ensure haskell-mode
  :defer t
  :init (add-hook 'haskell-mode-hook #'haskell-indentation-mode))

(use-package hindent                    ; Automated Haskell indentation
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package flycheck-haskell           ; Setup Flycheck from Cabal projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package helm-hayoo                 ; Helm frontend for Hayoo
  :ensure t
  :defer t
  :init (with-eval-after-load 'haskell-mode
          (bind-key "C-c m h" #'helm-hayoo haskell-mode-map)))

(use-package helm-hoogle                ; Helm frontend for Hoogle
  :ensure t
  :defer t
  :init (with-eval-after-load 'haskell-mode
          (bind-key "C-c m H" #'helm-hoogle haskell-mode-map)))


;;; OCaml
(use-package opam                       ; Initialize Emacs with OPAM env
  :ensure t
  :init (opam-init))

(use-package tuareg                     ; OCaml editing
  :ensure t
  :defer t
  :config
  (progn
    ;; Disable SMIE indentation in Tuareg.  It's just broken currently…
    (setq tuareg-use-smie nil)

    ;; Please, Tuareg, don't kill my imenu
    (define-key tuareg-mode-map [?\C-c ?i] nil)))

(use-package merlin                     ; Powerful Emacs backend for OCaml
  :ensure t
  :defer t
  :init (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config
  ;; Use Merlin from current OPAM env
  (setq merlin-command 'opam
        ;; Disable Merlin's own error checking in favour of Flycheck
        merlin-error-after-save nil))

(use-package flycheck-ocaml             ; Check OCaml code with Merlin
  :ensure t
  :defer t
  :init (with-eval-after-load 'merlin
          (flycheck-ocaml-setup)))


;;; Web languages

(use-package web-mode                   ; Template editing
  :ensure t
  :defer t
  :mode "/templates?/.*\\.\\(php\\|html\\)\\'"
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package js2-mode                   ; Javascript editing
  :ensure t
  :mode "\\.js\\'"
  :config (progn (setq-default js2-basic-offset 2)
                 (setq js2-global-externs '("angular"))

                 (add-hook 'js2-mode-hook
                           #'js2-highlight-unused-variables-mode)))

(use-package css-mode                   ; CSS editing
  :defer t
  :config
  (progn
    ;; Run Prog Mode hooks, because for whatever reason CSS Mode derives from
    ;; `fundamental-mode'.
    (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

    ;; Mark css-indent-offset as safe local variable.  TODO: Report upstream
    (put 'css-indent-offset 'safe-local-variable #'integerp)))

(use-package css-eldoc                  ; Basic Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode                   ; Because sometimes you have to
  :ensure t)


;;; Misc programming languages
(use-package sh-script                  ; Shell scripts
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))

(use-package puppet-mode                ; Puppet manifests
  :ensure t
  :defer t
  :config
  ;; Fontify variables in Puppet comments
  (setq puppet-fontify-variables-in-comments t))

(use-package nxml-mode                  ; XML editing
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t))

(use-package feature-mode               ; Feature files for ecukes/cucumber
  :ensure t
  :defer t
  :config
  (progn
    ;; Add standard hooks for Feature Mode, since it is no derived mode
    (add-hook 'feature-mode-hook #'whitespace-mode)
    (add-hook 'feature-mode-hook #'whitespace-cleanup-mode)
    (add-hook 'feature-mode-hook #'flyspell-mode)))

(use-package cmake-mode                 ; CMake files
  :ensure t
  :defer t)

(use-package thrift                     ; Thrift interface files
  :ensure t
  :defer t
  :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
  :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package swift-mode                 ; Swift sources
  :ensure t
  :defer t
  :config (with-eval-after-load 'flycheck
            (add-to-list 'flycheck-checkers 'swift)))


;;; Proof General & Coq
(defun lunaryorn-have-proofgeneral-p ()
  "Determine whether we have Proof General installed."
  (file-exists-p (locate-user-emacs-file "vendor/ProofGeneral/generic")))

(use-package proof-site                 ; Enable ProofGeneral if present
  ;; Don't :defer this one, since it sets up `load-path' and stuff for
  ;; ProofGeneral
  :load-path "vendor/ProofGeneral/generic"
  :if (lunaryorn-have-proofgeneral-p))

;; Proof General has a rather strange way of creating this variable
(defvar coq-one-command-per-line)
(setq coq-one-command-per-line nil)

(use-package proof-splash               ; ProofGeneral Splash screen
  :if (lunaryorn-have-proofgeneral-p)
  :defer t
  ;; Shut up, ProofGeneral
  :config (setq proof-splash-enable nil))

(use-package proof-useropts             ; ProofGeneral options
  :if (lunaryorn-have-proofgeneral-p)
  :defer t
  :config (setq proof-three-window-mode-policy 'hybrid
                ;; Automatically process the script up to point when inserting a
                ;; terminator.  Really handy in Coq.
                proof-electric-terminator-enable t))

(use-package proof-config               ; ProofGeneral proof configuration
  :if (lunaryorn-have-proofgeneral-p)
  :defer t
  ;; Skip over consecutive comments when processing
  :config (setq proof-script-fly-past-comments t))

(use-package proof-script               ; Proof editing
  :if (lunaryorn-have-proofgeneral-p)
  :defer t
  :config
  (add-hook 'proof-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package isar                       ; Isabelle syntax for PG
  :if (lunaryorn-have-proofgeneral-p)
  :defer t
  :config
  ;; Don't highlight overlong lines in Isar, since Unicode Tokens conceal the
  ;; true line length
  (add-hook 'isar-mode-hook #'lunaryorn-whitespace-style-no-long-lines 'append))

(use-package company-coq                ; Company for Coq and more Coq fanciness
  :if (lunaryorn-have-proofgeneral-p)
  :ensure t
  :defer t
  :init (add-hook 'coq-mode-hook #'company-coq-initialize))


;;; Databases
(use-package sql                        ; SQL editing and REPL
  :bind (("C-c a s" . sql-connect)))


;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init (progn
          ;; Highlight changes to the current file in the fringe
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of Dired
          (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

          ;; Fall back to the display margin, if the fringe is unavailable
          (unless (display-graphic-p)
            (diff-hl-margin-mode))))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull))
  ;; Aggressively commit to WIP refs on any change
  :init (progn (magit-wip-after-save-mode)
               (magit-wip-after-apply-mode)
               (magit-wip-before-change-mode))
  :config (progn
            ;; Shut up, Magit
            (setq magit-revert-buffers 'silent
                  magit-save-repository-buffers 'dontask
                  magit-push-always-verify nil
                  magit-refs-show-commit-count 'all
                  ;; For some reason this doesn't work :(
                  ;; magit-completing-read-function
                  ;; #'helm-completing-read-with-cands-in-buffer
                  )

            ;; Set Magit's repo dirs for `magit-status' from Projectile's known
            ;; projects.  Initialize the `magit-repository-directories'
            ;; immediately after Projectile was loaded, and update it every time
            ;; we switched projects, because the new project might have been
            ;; unknown before
            (defun lunaryorn-magit-set-repo-dirs-from-projectile ()
              "Set `magit-repo-dirs' from known Projectile projects."
              (let ((project-dirs (bound-and-true-p projectile-known-projects)))
                ;; Remove trailing slashes from project directories, because
                ;; Magit adds trailing slashes again, which breaks the
                ;; presentation in the Magit prompt.
                (setq magit-repository-directories
                      (mapcar #'directory-file-name project-dirs))))

            (with-eval-after-load 'projectile
              (lunaryorn-magit-set-repo-dirs-from-projectile))

            (add-hook 'projectile-switch-project-hook
                      #'lunaryorn-magit-set-repo-dirs-from-projectile))
  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package magit-gh-pulls             ; Show Github PRs in Magit
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :defer t
  :config
  ;; Oh, really?  Come on… I know what I'm doing…
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind (("C-c v t" . git-timemachine)))


;;; Search
(use-package "isearch"                  ; Search buffers
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  :init (progn (diminish 'isearch-mode)

               ;; Please, isearch, let me scroll during search
               (setq isearch-allow-scroll t)))

(use-package helm-regex                 ; Helm regex tools
  :ensure helm
  :bind (([remap occur] . helm-occur)
         ("C-c s o"     . helm-occur)
         ("C-c s s"     . helm-multi-occur)))

(use-package grep                       ; Control grep from Emacs
  :defer t
  :config
  (progn
    (when-let (gnu-find (and (eq system-type 'darwin)
                             (executable-find "gfind")))
      (setq find-program gnu-find))

    (when-let (gnu-xargs (and (eq system-type 'darwin)
                              (executable-find "gxargs")))
      (setq xargs-program gnu-xargs))))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package ag                         ; Search code in files/projects
  :ensure t
  :bind (("C-c s d" . ag-dired-regexp)
         ("C-c s D" . ag-dired)
         ("C-c s f" . ag-files)
         ("C-c s k" . ag-kill-other-buffers)
         ("C-c s K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; A little fanciness
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(use-package wgrep                      ; Edit grep/occur/ag results in-place
  :ensure t
  :defer t)

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)

(use-package helm-ag                    ; Helm frontend for Ag
  :ensure t
  ;; :bind (("C-c a a" . helm-do-ag)
  ;;        ("C-c a A" . helm-ag))
  :config (setq helm-ag-fuzzy-match t
                helm-ag-insert-at-point 'symbol
                helm-ag-source-type 'file-line))

;;; Project management with Projectile
(use-package projectile                 ; Project management for Emacs
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

    (setq projectile-completion-system 'helm
          projectile-find-dir-includes-top-level t
          projectile-mode-line '(:propertize
                                 (:eval (concat " " (projectile-project-name)))
                                 face bold))

    (projectile-register-project-type 'haskell-stack '("stack.yml")
                                      "stack build" "stack test")

    (defun lunaryorn-neotree-project-root (&optional directory)
      "Open a NeoTree browser for a project DIRECTORY."
      (interactive)
      (let ((default-directory (or directory default-directory)))
        (if (and (fboundp 'neo-global--window-exists-p)
                 (neo-global--window-exists-p))
            (neotree-hide)
          (neotree-find (projectile-project-root)))))

    (bind-key "t" #'lunaryorn-neotree-project-root
              projectile-command-map)
    (bind-key "T" #'projectile-toggle-between-implementation-and-test
              projectile-command-map))
  :diminish projectile-mode)

(use-package helm-projectile            ; Helm frontend for Projectile
  :ensure t
  :defer t
  :init (with-eval-after-load 'projectile (helm-projectile-on))
  :config (progn (setq projectile-switch-project-action #'helm-projectile)

                 (bind-key "C-t" #'lunaryorn-neotree-project-root
                           helm-projectile-projects-map)

                 (helm-add-action-to-source "Open NeoTree `C-t'"
                                            #'lunaryorn-neotree-project-root
                                            helm-source-projectile-projects 1)))


;;; Processes and commands
(use-package proced                     ; Edit system processes
  ;; Proced isn't available on OS X
  :if (not (eq system-type 'darwin))
  :bind ("C-x p" . proced))


;;; Date and time
(use-package calendar                   ; Built-in calendar
  :bind ("C-c a c" . calendar)
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time                       ; Show current time
  :bind (("C-c a c" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Berlin"    "Berlin")
                                  ("Europe/London"    "London")
                                  ("Europe/Istanbul"  "Istanbul")
                                  ("America/Winnipeg" "Winnipeg (CA)")
                                  ("America/New_York" "New York (USA)")
                                  ("Asia/Tokyo"       "Tokyo (JP)"))))


;;; Terminal emulation and shells
(use-package shell                      ; Dump shell in Emacs
  :bind ("C-c a t" . shell))

(use-package term                       ; Terminal emulator in Emacs
  :bind ("C-c a T" . ansi-term))


;;; Org Mode
(use-package org
  :bind (("C-c a o a" . org-agenda-list)
         ("C-c a o c" . org-capture)
         ("C-c a o f" . org-cycle-agenda-files)
         ("C-c a o s" . org-search-view)
         ("C-c a o t" . org-todo-list))
  :config (progn
            (setq org-directory (expand-file-name "~/Google Drive/Org/")
                  org-default-notes-file (expand-file-name "notes.org" org-directory)
                  org-refile-targets '((org-agenda-files :maxlevel . 2)))
            ;; Disable whitespace highlighting of overlong lines in Org Mode
            (add-hook 'org-mode-hook
                      #'lunaryorn-whitespace-style-no-long-lines)))


;;; Documents
(use-package doc-view
  :defer t
  :config (progn
            ;; Render PDFs at 300dpi
            (setq doc-view-resolution 300)

            ;; Warn if Doc View falls back to Ghostscript for rendering
            (unless (eq doc-view-pdf->png-converter-function
                        'doc-view-pdf->png-converter-mupdf)
              (warn "Doc View is not using mupdf.
Install mudraw with brew install mupdf-tools"))))


;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c a u" . browse-url)))

(use-package bug-reference              ; Turn bug refs into browsable buttons
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'goto-address-prog-mode)
               (add-hook 'text-mode-hook #'goto-address-mode)))

(use-package eww                        ; Emacs' built-in web browser
  :bind (("C-c a w b" . eww-list-bookmarks)
         ("C-c a w w" . eww)
         ("C-c a w u" . eww-browse-url)))

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c a S a" . sx-ask)
         ("C-c a S s" . sx-tab-all-questions)
         ("C-c a S q" . sx-tab-all-questions)
         ("C-c a S f" . sx-tab-all-questions)
         ("C-c a S n" . sx-tab-newest)))

(use-package sx-compose                 ; Write questions/answers for Stack Exchange
  :ensure sx
  :defer t
  :config
  (progn
    ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
    ;; more nicely with the website.
    (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'sx-compose-mode-hook #'visual-line-mode)
    (add-hook 'sx-compose-mode-hook
              #'lunaryorn-whitespace-style-no-long-lines)

    ;; Clean up whitespace before sending questions
    (add-hook 'sx-compose-before-send-hook
              (lambda ()
                (whitespace-cleanup)
                t))

    (bind-key "M-q" #'ignore sx-compose-mode-map)))

(use-package sx-question-mode           ; Show Stack
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package sendmail                   ; Send mails from Emacs
  :defer t
  :config (setq send-mail-function 'smtpmail-send-it))

(use-package message                    ; Compose mails from Emacs
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                ;; Don't keep message buffers around
                message-kill-buffer-on-exit t))

(use-package erc                        ; Powerful IRC client
  :defer t
  :config
  (progn
    ;; Default server and nick
    (setq erc-server "chat.freenode.net"
          erc-port 7000
          erc-nick "lunaryorn"
          erc-nick-uniquifier "_"
          ;; Never open unencrypted ERC connections
          erc-server-connect-function 'erc-open-tls-stream)

    ;; Spell-check ERC buffers
    (add-to-list 'erc-modules 'spelling)
    (erc-update-modules)))

(use-package erc-join                   ; Automatically join channels with ERC
  :defer t
  :config
  ;; Standard channels on Freenode
  (setq erc-autojoin-channels-alist '(("\\.freenode\\.net" . ("#emacs")))))

(use-package erc-track                  ; Track status of ERC in mode line
  :defer t
  :config
  ;; Switch to newest buffer by default, and don't ask before rebinding the keys
  (setq erc-track-switch-direction 'newest
        erc-track-enable-keybindings t))

(use-package rcirc                      ; Simply ERC client
  :defer t
  :config
  (progn
    (setq rcirc-default-full-name (format "%s (http://www.lunaryorn.com)"
                                          user-full-name)
          rcirc-default-nick "lunaryorn"
          rcirc-time-format "%Y-%m-%d %H:%M "
          rcirc-server-alist
          '(("chat.freenode.not" :port 7000 :user-name "lunaryorn"
             :encryption tls :channels ("#emacs" "#haskell" "#hakyll" "#zsh"))))

    (add-hook 'rcirc-mode-hook #'flyspell-mode)

    (rcirc-track-minor-mode)))


;;; Online Help
(use-package find-func                  ; Find function/variable definitions
  :bind (("C-c h F"   . find-function)
         ("C-c h 4 F" . find-function-other-window)
         ("C-c h K"   . find-function-on-key)
         ("C-c h V"   . find-variable)
         ("C-c h 4 V" . find-variable-other-window)))

(use-package info                       ; Info manual viewer
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face.  Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-type-face))

(use-package helm-info                  ; Helm tools for Info
  :ensure helm
  :bind (("C-c h e" . helm-info-emacs)
         ("C-c h i" . helm-info-at-point)))

(use-package helm-man                   ; Browse manpages with Heml
  :ensure helm
  :bind (("C-c h m" . helm-man-woman)))

(use-package helm-descbinds             ; Describe key bindings with Helm
  :ensure t
  :init (helm-descbinds-mode))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish (ansible-doc-mode . " Ⓓ"))

(use-package dash-at-point              ; Jump to Dash docset at point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset))
  :config (add-to-list 'dash-at-point-mode-alist
                       '(swift-mode . "ios,swift")))

(bind-key "C-c h b" #'describe-personal-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
