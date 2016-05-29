;;; init.el --- Emacs configuration of Sebastian Wiesner -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; Requires

(eval-when-compile
  (require 'use-package))

(require 'subr-x)
(require 'time-date)


;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; And disable the site default settings
(setq inhibit-default-init t)

(defun lunaryorn-snapshot-version-p (version)
  "Whether VERSION is an Emacs snapshot version."
  (pcase-let ((`(,_ ,_ ,build) (version-to-list version)))
    ;; Snapshots with build numbers > 90 are pretests which come from proper
    ;; release tarballs and don't need to be rebuild weekly
    (and (>= build 50) (<= build 90))))

(when (lunaryorn-snapshot-version-p emacs-version)
  ;; When on a snapshot version, warn if the build is older than a week to
  ;; ensure that we stay up to date.
  (run-with-idle-timer
   2 nil
   (lambda ()
     (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
       (when (> (time-to-number-of-days time-since-build) 7)
         (lwarn 'emacs :warning "Your Emacs build is more than a week old!"))))))


;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      ;; Use a non-interactive login shell.  A login shell, because my
      ;; environment variables are mostly set in `.zprofile'.
      (setq exec-path-from-shell-arguments '("-l")))

    ;; Import additional environment variables beyond just $PATH
    (dolist (var '("PYTHONPATH"         ; Python modules
                   "INFOPATH"           ; Info directories
                   "JAVA_OPTS"          ; Options for java processes
                   "SBT_OPTS"           ; Options for SBT
                   "RUST_SRC_PATH"      ; Rust sources, for racer
                   "CARGO_HOME"         ; Cargo home, for racer
                   "EMAIL"              ; My personal email
                   ))
      (add-to-list 'exec-path-from-shell-variables var))

    ;; Initialize Emacs' environment from the shell
    (exec-path-from-shell-initialize)

    ;; Tell Emacs about my email address
    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
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

(set-face-attribute 'default nil
                    :family "Source Code Pro" :height 130)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 140 :weight 'regular)

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
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t nil (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(when-let (frame (selected-frame))
  (lunaryorn-configure-fonts frame))
(add-hook 'after-make-frame-functions #'lunaryorn-configure-fonts)

(use-package face-remap                 ; Face remapping
  :bind (("C-c w z" . text-scale-adjust)))


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

;; Insert my logo into scratch <3
(add-hook 'after-init-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (goto-char (point-min))
              (insert-image (create-image (locate-user-emacs-file "logo.png"))
                            "logo")
              (insert "\n"))))

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

(bind-key "C-c t v" #'variable-pitch-mode)

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package beacon                     ; Highlight cursor position in buffer
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode)

(use-package stripe-buffer              ; Add stripes to a buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))


;;; Keys and key bindings

;; Our key bindings are solely in the user space C-c <letter>.  The list of
;; which-key prefixes documents the meaning of specific key prefixes, such as
;; C-c f for file commands.  C-c m is special in that it always holds commands
;; that are only for the current major mode.  The mode-specific which-key
;; prefixes document the individual bindings for major modes under C-c m.
;;
;; We may also bind F5 to F9.  Since we can't document these in code, the
;; following list shows their abstract meanings:
;;
;; * F5: Compile
;; * F6: n/a
;; * F7: n/a
;; * F8: n/a
;; * F9: n/a
;;
;; All of these keys have default global bindings, but major and minor modes may
;; override them if there's a better command for the specific purpose available.
;;
;; Note that the notation for the function keys is <f5>, i.e. the lowercase name
;; surrounded by angle brackets.

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;; Let's go unicode :)
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
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . "|=")
          ;; Drop/shorten package prefixes
          ("\\`lunaryorn-"  . "")
          ("projectile-"    . "proj-")
          ("helm-"          . "h-")
          ("magit-"         . "ma-")))

  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "helm/help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "language/spelling"
    "C-c m" "major mode"
    "C-c o" "cursors"
    "C-c p" "projects"
    "C-c p s" "projects/search"
    "C-c p x" "projects/execute"
    "C-c p 4" "projects/other-window"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames"
    "C-c x" "text")

  ;; Prefixes for major modes
  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    "C-c m" "elisp/personal"
    "C-c m e" "eval")

  (which-key-declare-prefixes-for-mode 'js2-mode
    "C-c m" "js/personal"
    "C-c m r" "refactor")

  (which-key-declare-prefixes-for-mode 'scala-mode
    "C-c C-b" "ensime/build"
    "C-c C-d" "ensime/debug"
    "C-c C-r" "ensime/refactor"
    "C-c C-v" "ensime/misc"
    "C-c m" "scala/personal"
    "C-c m b" "scala/build")

  (which-key-declare-prefixes-for-mode 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-declare-prefixes-for-mode 'rust-mode
    "C-c C-c" "rust/cargo")

  (which-key-declare-prefixes-for-mode 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags")

  :diminish which-key-mode)

(use-package hydra                      ; Bindings that stick
  :ensure t)

(use-package helm-descbinds             ; Describe key bindings with Helm
  :ensure t
  :init (helm-descbinds-mode))


;; Package manager and init file
(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . package-list-packages-no-fetch))
  :config
  (setq paradox-execute-asynchronously nil ; No async update, please
        paradox-spinner-type 'moon      ; Fancy spinner
        ;; Show all possible counts
        paradox-display-download-count t
        paradox-display-star-count t
        ;; Don't star automatically
        paradox-automatically-star nil
        ;; Hide download button, and wiki packages
        paradox-use-homepage-buttons nil ; Can type v instead
        paradox-hide-wiki-packages t))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)


;;; The mode line
(line-number-mode)
(column-number-mode)

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name
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

(use-package spaceline-config           ; A beautiful mode line
  :ensure spaceline
  :config
  (spaceline-helm-mode)                 ; Enable a special Helm mode line

  (spaceline-compile
   'lunaryorn
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     anzu
     '(buffer-id remote-host buffer-encoding-abbrev)
     ((point-position line-column buffer-position selection-info)
      :separator " | ")
     major-mode
     process
     (flycheck-error flycheck-warning flycheck-info)
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active)
     nyan-cat)
   ;; Right segment (the unimportant stuff)
   '((version-control :when active)
     battery))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-lunaryorn)))))

(use-package nyan-mode                  ; NYAN CATS!!!
  :ensure t
  :init (nyan-mode))

(use-package powerline                  ; The work-horse of Spaceline
  :ensure t
  :defer t
  :after spaceline-config
  :config (setq powerline-height (truncate (* 1.0 (frame-char-height)))
                powerline-default-separator 'utf-8))


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
  :bind (
         ;; Replace built-in commands with more powerful Helm variants
         ([remap find-file] . helm-find-files)
         ([remap switch-to-buffer] . helm-mini)
         ([remap execute-extended-command] . helm-M-x)
         ([remap yank-pop]        . helm-show-kill-ring)
         ([remap insert-register] . helm-register)
         ([remap apropos-command] . helm-apropos)
         ([remap occur] . helm-occur)
         ;; Additional helm commands
         ("C-c f l" . helm-locate-library)
         ("C-c f s" . helm-for-files)
         ("C-c f r" . helm-recentf)
         ("C-c h l" . helm-resume)
         ("C-c h m" . helm-man-woman)
         ("C-c i C" . helm-colors)
         ("C-c j t" . helm-imenu))
  :init
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config
  (setq helm-split-window-in-side-p t
        ;; Fuzzy matching
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-imenu-fuzzy-match t
        ;; Use recentf to manage file name history
        helm-ff-file-name-history-use-recentf t
        ;; Find libraries from `require', etc.
        helm-ff-search-library-in-sexp t
        ;; Don't automatically jump to imenu candidate if only one match,
        ;; because it makes the behaviour of this command unpredictable, and
        ;; prevents me from getting an overview over the buffer if point is on a
        ;; matching symbol.
        helm-imenu-execute-action-at-once-if-one nil)

  (when (eq system-type 'darwin)
    ;; Replace locate with spotlight for `helm-for-files'
    (setq helm-for-files-preferred-list
          (append (delq 'helm-source-locate
                        helm-for-files-preferred-list)
                  '(helm-source-mac-spotlight))))
  :diminish helm-mode)

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c i 8" . helm-unicode))


;;; Buffer, Windows and Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b"))
      ;; Size new windows proportionally wrt other windows
      window-combination-resize t)

(setq-default line-spacing 0.2)         ; A bit more spacing between lines

(defun lunaryorn-display-buffer-fullframe (buffer alist)
  "Display BUFFER in fullscreen.

ALIST is a `display-buffer' ALIST.

Return the new window for BUFFER."
  (let ((window (display-buffer-pop-up-window buffer alist)))
    (when window
      (delete-other-windows window))
    window))

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(
        ;; Magit status window in fullscreen
        (,(rx "*magit: ")
         (lunaryorn-display-buffer-fullframe)
         (reusable-frames . nil))
        ;; Give Helm Help a non-side window because Helm as very peculiar ideas
        ;; about how to display its help
        (,(rx bos "*Helm Help" (* nonl) "*" eos)
         (display-buffer-use-some-window
          display-buffer-pop-up-window))
        ;; Nail Helm to the side window
        (,(rx bos "*" (* nonl) "helm" (* nonl) "*" eos)
         (display-buffer-in-side-window)
         (side . bottom)
         (window-height . 0.4)
         (window-width . 0.6))
        ;; Put REPLs and error lists into the bottom side window
        (,(rx bos
              (or "*Help"                 ; Help buffers
                  "*Warnings*"            ; Emacs warnings
                  "*compilation"          ; Compilation buffers
                  "*Flycheck errors*"     ; Flycheck error list
                  "*shell"                ; Shell window
                  "*sbt"                  ; SBT REPL and compilation buffer
                  "*ensime-update*"       ; Server update from Ensime
                  "*SQL"                  ; SQL REPL
                  "*Cargo"                ; Cargo process buffers
                  (and (1+ nonl) " output*") ; AUCTeX command output
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
  :bind (("C-c w F" . toggle-frame-fullscreen))
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
  :demand t                          ; Prevent a mysterious recursive load error
  :bind (("C-c b k" . lunaryorn-kill-this-buffer))
  :config
  (add-hook 'kill-buffer-query-functions
                  'lunaryorn-do-not-kill-important-buffers))

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (setq ibuffer-formats
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
          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :disabled t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

;; Standard window commands
(bind-key "C-c w =" #'balance-windows)
(bind-key "C-c w k" #'delete-window)
(bind-key "C-c w /" #'split-window-right)
(bind-key "C-c w -" #'split-window-below)
(bind-key "C-c w m" #'delete-other-windows)

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

(use-package golden-ratio               ; Automatically resize windows
  :ensure t
  :init
  (defun lunaryorn-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . lunaryorn-toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
                                      windmove-down
                                      windmove-left
                                      windmove-right
                                      ace-window
                                      ace-delete-window
                                      ace-select-window
                                      ace-swap-window
                                      ace-maximize-window)
        ;; Exclude a couple of special modes from golden ratio, namely
        ;; Flycheck's error list, calc
        golden-ratio-exclude-modes '(flycheck-error-list-mode
                                     calc-mode
                                     dired-mode
                                     ediff-mode
                                     )
        ;; Exclude a couple of special buffers from golden ratio, namely Helm,
        ;; WhichKey, NeoTree, etc.
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*" (any "h" "H") "elm*" eos)
          ,(rx bos "*which-key*" eos)
          ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(use-package desktop                    ; Save buffers, windows and frames
  :disabled t
  :init (desktop-save-mode)
  :config
  ;; Save desktops a minute after Emacs was idle.
  (setq desktop-auto-save-timeout 60)

  ;; Don't save Magit and Git related buffers
  (dolist (mode '(magit-mode magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG")))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t r" . writeroom-mode)))

(use-package popup                      ; Popup menus
  ;; We don't ensure this package, because we definitely don't want to have this
  ;; mess, but unfortunately it's a dependency of Ensime :(
  :ensure nil
  :defer t
  :config
  ;; Bring Popup bindings in line with Company bindings, by getting rid of C-n/p
  ;; for navigation and introducing M-n/p
  (define-key popup-menu-keymap "\C-n" nil)
  (define-key popup-menu-keymap [down] nil)
  (define-key popup-menu-keymap "\C-p" nil)
  (define-key popup-menu-keymap [up] nil)
  (define-key popup-menu-keymap (kbd "M-n") #'popup-next)
  (define-key popup-menu-keymap (kbd "M-p") #'popup-previous))


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

(use-package server                     ; The server of `emacsclient'
  :defer t
  :init (server-mode)
  :diminish (server-buffer-clients . " ⓒ"))

(use-package dired                      ; Edit directories
  :defer t
  :config
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
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x                    ; Additional tools for Dired
  :defer nil
  :bind (("C-c f j" . dired-jump)
         ("C-x C-j" . dired-jump))
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
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
                '((name . dired-omit-mode-diminish))))

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

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ; LaTeX
                 ))
    (add-to-list 'ignoramus-file-endings ext))

  (ignoramus-setup))

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
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)

(use-package reveal-in-osx-finder           ; Reveal current buffer in finder
  :ensure t
  :bind (("C-c f f" . reveal-in-osx-finder)))

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

;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)


;;; Navigation and scrolling
(setq scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package page                       ; Page navigation
  :bind (("C-x ]" . lunaryorn-pages/forward-page)
         ("C-x [" . lunaryorn-pages/backward-page))
  :init
  (defhydra lunaryorn-pages ()
    "Pages"
    ("[" backward-page "backward")
    ("]" forward-page "forward")))

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j b" . avy-pop-mark)
         ("C-c j j" . avy-goto-char-2)))

(use-package ace-link                   ; Fast link jumping
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'info
    (bind-key "C-c m l" #'ace-link-info Info-mode-map))

  (with-eval-after-load 'help-mode
    (defvar help-mode-map)              ; Silence the byte compiler
    (bind-key "C-c m l" #'ace-link-help help-mode-map)))

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))


;;; Search
(use-package "isearch"                  ; Search buffers
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  :init
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  (diminish 'isearch-mode)

  ;; Please, isearch, let me scroll during search
  (setq isearch-allow-scroll t))

(use-package helm-swoop                 ; Powerful buffer search for Emacs
  :ensure t
  :after helm
  :bind (("C-c s s" . helm-swoop)
         ("C-c s S" . helm-multi-swoop)
         ("C-c s C-s" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-speed-or-color t     ; Colour over speed 8)
        ;; Split window like Helm does
        helm-swoop-split-window-function #'helm-default-display-buffer))

(use-package grep                       ; Control grep from Emacs
  :defer t
  :config
  (when-let (gnu-find (and (eq system-type 'darwin)
                           (executable-find "gfind")))
    (setq find-program gnu-find))

  (when-let (gnu-xargs (and (eq system-type 'darwin)
                            (executable-find "gxargs")))
    (setq xargs-program gnu-xargs)))

(use-package locate                     ; Search files on the system
  :defer t
  :config
  ;; Use mdfind as locate substitute on OS X, to utilize the Spotlight database
  (when-let (mdfind (and (eq system-type 'darwin) (executable-find "mdfind")))
    (setq locate-command mdfind)))

(use-package helm-ag                    ; Helm frontend for Ag
  :ensure t
  :bind (("C-c s a" . helm-ag)
         ("C-c s A" . helm-do-ag))
  :config
  (setq helm-ag-fuzzy-match t                   ; Fuzzy matching
        helm-ag-insert-at-point 'symbol         ; Default to symbol at point
        helm-ag-edit-save t                     ; save buffers after editing
        ))


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
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " Ⓕ")

(bind-key "C-c x i" #'indent-region)

(use-package simple
  :defer t
  :bind (("M-g n" . lunaryorn-errors/next-error)
         ("M-g p" . lunaryorn-errors/previous-error))
  :init
  (defhydra lunaryorn-errors ()
    "Errors."
    ("n" next-error "next")
    ("p" previous-error "previous")
    ("f" first-error "first")))

(use-package lunaryorn-simple           ; Personal editing helpers
  :load-path "lisp/"
  :bind (([remap kill-whole-line]        . lunaryorn-smart-kill-whole-line)
         ([remap move-beginning-of-line] . lunaryorn-back-to-indentation-or-beginning-of-line)
         ("C-<backspace>"                . lunaryorn-smart-backward-kill-line)
         ("C-S-j"                        . lunaryorn-smart-open-line)
         ("C-<return>"                   . lunaryorn-smart-open-line)
         ;; Additional utilities
         ("C-c i d"                      . lunaryorn-insert-current-date)
         ("C-c i l a"                    . lunaryorn-insert-apache2)
         ("C-c i l m"                    . lunaryorn-insert-mit/x11))
  :commands (lunaryorn-auto-fill-comments-mode)
  ;; Auto-fill comments in programming modes
  :init (add-hook 'prog-mode-hook #'lunaryorn-auto-fill-comments-mode))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package newcomment                 ; Built-in comment features
  :bind (("C-c c d" . comment-dwim)
         ("C-c c l" . comment-line)
         ("C-c c r" . comment-region)))

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
  :bind (("C-c x a a" . align)
         ("C-c x a c" . align-current)))

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
  :bind (("C-c v" . er/expand-region)))

(use-package embrace                    ; Wrap semantic units with pairs
  :ensure t
  :bind (("C-c x e" . lunaryorn-embrace/body))
  :init
  (defhydra lunaryorn-embrace (:hint nil)
    "
Add (_a_), change (_c_) or delete (_d_) a pair.  Quit with _q_.
"
    ("a" embrace-add)
    ("c" embrace-change)
    ("d" embrace-delete)
    ("q" nil)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Additional keybindings
(bind-key [remap just-one-space] #'cycle-spacing)


;;; Internationalisation
(prefer-coding-system 'utf-8)

(use-package mule-cmds                  ; Input methods
  :defer t
  :bind (("C-c t i" . toggle-input-method))
  :config
  (setq default-input-method "german-postfix"))


;;; Paired delimiters
(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :bind (("C-c k" . lunaryorn-smartparens/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :init
  ;; Hydra for Smartparens
  (defhydra lunaryorn-smartparens (:hint nil)
    "
Sexps (quit with _q_)

^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑

^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  ;; TODO: Define hydra for smartparens!
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)

  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)
  :diminish (smartparens-mode . " ⓟ"))

(use-package lunaryorn-smartparens      ; Personal Smartparens extensions
  :disabled t
  :load-path "lisp/"
  :after smartparens
  :config (lunaryorn-smartparens-bind-keys))


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
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
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
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
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
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init
  (dolist (fn '(highlight-symbol-nav-mode highlight-symbol-mode))
    (add-hook 'prog-mode-hook fn))
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  :diminish highlight-symbol-mode)

(use-package hl-todo                    ; Highlight TODOs in buffers
  :ensure t
  :defer t
  :init (global-hl-todo-mode))


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
            lunaryorn-try-complete-lisp-symbol-without-namespace))))

(use-package lunaryorn-hippie-exp       ; Custom expansion functions
  :load-path "lisp/"
  :after hippie-exp
  :commands (lunaryorn-try-complete-lisp-symbol-without-namespace))

(use-package yasnippet                  ; Snippets
  :ensure t
  :defer t
  :diminish (yas-minor-mode . " Ⓨ"))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :diminish company-mode)

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :after company
  :init (company-quickhelp-mode))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :after company
  :init (company-statistics-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-emoji              ; Emojis completion like Github/Slack
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package helm-company               ; Helm frontend for company
  :ensure t
  :defer t
  :after company
  :init ;; Use Company for completion
  (bind-key [remap complete-symbol] #'helm-company company-mode-map)
  (bind-key [remap completion-at-point] #'helm-company company-mode-map)
  (bind-key "C-:" #'helm-company company-mode-map)
  (bind-key "C-:" #'helm-company company-active-map))

(use-package auto-insert                ; Automatic insertion into new files
  :defer t
  :bind (("C-c i a" . auto-insert)))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :bind (("C-c i c" . copyright-update))
  :init
  ;; Update copyright when visiting files
  (defun lunaryorn-copyright-update ()
    (interactive)
    (unless buffer-read-only
      (copyright-update nil 'interactive)
      (unless copyright-update
        ;; Fix years when the copyright information was updated
        (copyright-fix-years))))
  (add-hook 'find-file-hook #'lunaryorn-copyright-update)
  :config
  ;; Use ranges to denote consecutive years
  (setq copyright-year-ranges t
        ;; Limit copyright changes to my own copyright
        copyright-names-regexp (regexp-quote user-full-name)))


;;; Spelling and syntax checking
(use-package ispell                     ; Spell checking
  :defer t
  :config
  (setq ispell-program-name (if (eq system-type 'darwin)
                                (executable-find "aspell")
                              (executable-find "hunspell"))
        ispell-dictionary "en_GB"     ; Default dictionnary
        ispell-silently-savep t       ; Don't ask when saving the private dict
        ;; Increase the height of the choices window to take our header line
        ;; into account.
        ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available.  Install Hunspell or ASpell for OS X.")))

(use-package flyspell                   ; On-the-fly spell checking
  :bind (("C-c t s" . flyspell-mode)
         ("C-c l b" . flyspell-buffer))
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

(use-package helm-flyspell              ; Helm interface to Flyspell
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ([remap flyspell-auto-correct-previous-word] . helm-flyspell-correct)
        ("C-c l c" . helm-flyspell-correct)))

(use-package auto-dictionary            ; Automatically infer dictionary
  :ensure t
  ;; Always change dictionary through adict, because it triggers hooks that let
  ;; us automatically update the "language" for other modes (e.g. Typo Mode) as
  ;; well
  :bind (("C-c l l" . adict-change-dictionary)
         ("C-c l g" . adict-guess-dictionary))
  :init
  (add-hook 'flyspell-mode-hook #'auto-dictionary-mode))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c e" . lunaryorn-flycheck-errors/body)
         ("C-c t f" . flycheck-mode))
  :init
  (defhydra lunaryorn-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message")
    ("h" helm-flycheck "list with helm"))

  (global-flycheck-mode)
  :config
  (setq flycheck-standard-error-navigation nil
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list
        flycheck-scalastylerc "scalastyle_config.xml")
  :diminish (flycheck-mode . " Ⓢ"))

(use-package lunaryorn-flycheck         ; Personal Flycheck extensions
  :load-path "lisp/"
  :commands (lunaryorn-flycheck-find-config-file-in-sbt-project
             lunaryorn-discard-undesired-html-tidy-error
             lunaryorn-use-js-executables-from-node-modules
             lunaryorn-flycheck-set-load-path-for-user-configuration)
  :init
  ;; Don't highlight undesired errors from html tidy
  (add-hook 'flycheck-process-error-functions
            #'lunaryorn-discard-undesired-html-tidy-error)
  (add-hook 'flycheck-locate-config-file-functions
            #'lunaryorn-flycheck-find-config-file-in-sbt-project)
  (dolist (hook-fn '(lunaryorn-use-js-executables-from-node-modules
                     lunaryorn-flycheck-set-load-path-for-user-configuration))
    (add-hook 'flycheck-mode-hook hook-fn)))

(use-package helm-flycheck              ; Helm frontend for Flycheck errors
  :ensure t
  :defer t
  :after flycheck)


;;; Text editing
(use-package table                      ; Edit tables in text files
  :defer t
  :init
  (add-hook 'text-mode-hook #'table-recognize))

(use-package tildify                    ; Insert non-breaking spaces on the fly
  :bind (("C-c x t" . tildify-region))
  :init
  (add-hook 'text-mode-hook #'tildify-mode)
  :config
  ;; Use the right space for LaTeX
  (add-hook 'latex-mode-hook
            (lambda () (setq-local tildify-space-string "~"))))

(use-package typo                       ; Automatically use typographic quotes
  :ensure t
  :bind (("C-c t t" . typo-mode)
         ("C-c l L" . typo-change-language))
  :init
  (typo-global-mode)
  (add-hook 'text-mode-hook #'typo-mode)
  :config
  ;; TODO: Automatically set from ispell dictionary in
  ;; `adict-change-dictionary-hook', to update the typo language whenever the
  ;; spelling language changed
  (setq-default typo-language "English")
  :diminish (typo-mode . " Ⓣ"))


;;; LaTeX with AUCTeX
(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex)

(use-package tex                        ; TeX editing/processing
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save style information
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
  (setq-default TeX-master nil          ; Ask for the master file
                TeX-engine 'luatex      ; Use a modern engine
                ;; Redundant in 11.88, but keep for older AUCTeX
                TeX-PDF-mode t)

  ;; Move to chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

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
  ;; Teach TeX folding about KOMA script sections
  (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
        ;; No language-specific hyphens please
        LaTeX-babel-hyphen nil)

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))    ; Easy math input

(use-package auctex-latexmk             ; latexmk command for AUCTeX
  :ensure t
  :defer t
  :after latex
  :init (auctex-latexmk-setup))

(use-package auctex-skim                ; Skim as viewer for AUCTeX
  :load-path "lisp/"
  :commands (auctex-skim-select)
  :after tex
  :init (auctex-skim-select))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :config
  ;; Run prog mode hooks for bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

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
    (setq reftex-cite-format 'biblatex))
  :diminish reftex-mode)


;;; Markup languages
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
  (bind-key "M-q" #'ignore gfm-mode-map))

(use-package lunaryorn-markdown         ; Personal Markdown extensions
  :load-path "lisp/"
  :bind (:map markdown-mode-map
              ("C-c m h" . lunaryorn-markdown-post-header)
              ("C-c m p" . lunaryorn-markdown-publish-jekyll-draft))
  :after markdown-mode)

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t
  :config
  (add-hook 'json-mode-hook
            ;; Fix JSON mode indentation
            (lambda () (setq-local js-indent-level 4))))

(use-package lunaryorn-json             ; Personal JSON tools
  :load-path "lisp/"
  :after json-mode
  :bind (:map json-mode-map
              ("C-c m r" . lunaryorn-json-chef-role)))

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

(use-package graphviz-dot-mode          ; Graphviz
  :ensure t
  :defer t
  :config
  (setq graphviz-dot-indent-width 4))


;;; Programming utilities
(use-package prog-mode                  ; Prog Mode
  :bind (("C-c t p" . prettify-symbols-mode)))

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c C" . recompile))
  :config
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

  (require 'ansi-color)

  (defun lunaryorn-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer.

Taken from http://stackoverflow.com/a/3072831/355252."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'lunaryorn-colorize-compilation-buffer))

(use-package helm-make                  ; Run makefile targets through Helm
  :ensure t
  :bind (("C-c c c" . helm-make-projectile)
         ;; FIXME: Write a more sophisticated command that checks whether a
         ;; Makefile exists and falls back to an alternative if not.
         ("<f5>" . helm-make-projectile)))

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

(use-package etags                      ; Tag navigation
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (setq tags-revert-without-query t))

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))


;;; Emacs Lisp
(bind-key "C-c t d" #'toggle-debug-on-error)

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :bind (:map elisp-slime-nav-mode-map
              ("C-c h ." . elisp-slive-nav-describe-elisp-thing-at-point))
  :config
  (dolist (key '("C-c C-d d" "C-c C-d C-d"))
    (define-key elisp-slime-nav-mode-map (kbd key) nil))
  :diminish elisp-slime-nav-mode)

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-package           ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :after flycheck
  :init (flycheck-package-setup))

(use-package pcre2el                    ; Convert regexps to RX and back
  :disabled t
  :ensure t
  :init (rxt-global-mode))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c a '" . ielm)))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c m e r" . eval-region)
              ("C-c m e b" . eval-buffer)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e f" . eval-defun)))

(use-package lunaryorn-elisp            ; Personal tools for Emacs Lisp
  :load-path "lisp/"
  :bind (:map emacs-lisp-mode-map ("C-c m f" . lunaryorn-elisp-find-cask-file))
  :after elisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'lunaryorn-add-use-package-to-imenu))

(use-package el-search                  ; pcase-based search for elisp
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m s" . el-search-pattern)
              ("C-c m r" . el-search-query-replace))
  :after elisp-mode)

(use-package cask-mode                  ; A major mode for Cask files
  :ensure t
  :defer t)

(use-package macrostep                  ; Interactively expand macros in code
  :ensure t
  :after elisp-mode
  :bind (:map emacs-lisp-mode-map ("C-c m x" . macrostep-expand)
         :map lisp-interaction-mode-map ("C-c m x" . macrostep-expand)))

(use-package ert                        ; Unit test framework
  ;; Load after Emacs Lisp Mode to support writing ERT tests
  :after elisp-mode)

(use-package buttercup                  ; BDD test framework for Emacs
  :ensure t
  :after elisp-mode)


;;; Scala

(use-package scala-mode                 ; Scala editing
  :ensure t
  :defer t
  :config
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)

  (defun lunaryorn-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (define-key scala-mode-map (kbd "RET")
    #'lunaryorn-newline-and-indent-with-asterisk))

(use-package sbt-mode                   ; Scala build tool
  :ensure t
  :defer t
  :after scala-mode
  :bind (:map scala-mode-map
              ("C-c m b c" . sbt-command)
              ("C-c m b r" . sbt-run-previous-command))
  :config
  ;; Do not pop up SBT buffers automatically
  (setq sbt:display-command-buffer nil)

  (defun lunaryorn-scala-pop-to-sbt (new-frame)
    "Open SBT REPL for this project, optionally in a NEW-FRAME.

Select the SBT REPL for the current project in a new window.  If
the REPL is not yet running, start it.  With prefix arg, select
the REPL in a new frame instead."
    (interactive "P")
    ;; Start SBT when no running, taken from `sbt:command'
    (when (not (comint-check-proc (sbt:buffer-name)))
      (sbt:run-sbt))

    (let ((display-buffer-overriding-action
           (if new-frame '(display-buffer-pop-up-frame) nil)))
      (pop-to-buffer (sbt:buffer-name))))

  (with-eval-after-load 'scala-mode
    (bind-key "C-c m s" #'lunaryorn-scala-pop-to-sbt scala-mode-map))

  ;; Disable Smartparens Mode in SBT buffers, because it frequently
  ;; hangs while trying to find matching delimiters
  (add-hook 'sbt-mode-hook
            (lambda ()
              (when (fboundp 'smartparens-mode)
                (smartparens-mode -1)))))

(use-package ensime                     ; Scala interaction mode
  :ensure t
  :defer t
  :after scala-mode
  :bind (:map ensime-mode-map
              ("C-c m E" . ensime-reload)
              ;; Free M-n and M-p again
              ("M-n" . nil)
              ("M-p" . nil)
              ("<f5>" . ensime-sbt-do-compile)
         :map scala-mode-map ("C-c m e" . ensime))
  :config
  ;; Enable Ensime for all Scala buffers.  We don't do this in :init, because
  ;; `ensime-mode' isn't autoloaded, and ensime-mode makes no sense before the
  ;; first session was started anyway
  (add-hook 'scala-mode-hook #'ensime-mode)

  (defun lunaryorn-auto-start-ensime ()
    "Auto-start Ensime if possible."
    (when (and (buffer-file-name) (not (ensime-connected-p)))
      (let ((ensime-prefer-noninteractive t) ; Don't prompt for ensime path
            (ensime-auto-connect 'always))
        (ensime-auto-connect))))

  (add-hook 'ensime-mode-hook #'lunaryorn-auto-start-ensime)

  ;; Compile on save.  My projects are small enough :)
  (setq ensime-sbt-perform-on-save "test:compile"))

(use-package ensime-expand-region       ; Integrate Ensime into expand-region
  :ensure ensime
  :after ensime)

(use-package play-routes-mode           ; Mode for Play 2 routes files
  :ensure t
  :defer t)

(use-package flycheck-ensime            ; Ensime-based checker for Flycheck
  :disabled t
  :load-path "lisp/"
  :defer t)


;;; Haskell
(use-package haskell-mode               ; Haskell major mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("M-." . haskell-mode-jump-to-def-or-tag)
              ("C-c m i j" . haskell-navigate-imports)
              ("C-c m i s" . haskell-sort-imports)
              ("C-c m i a" . haskell-align-imports)
              ;; Recommended Haskell Mode bindings, see
              ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
            )
  :config
  (setq haskell-tags-on-save t          ; Regenerate TAGS on save
        haskell-process-log t           ; Show log for GHCI process
        ;; Remove unused imports and auto-import modules
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; IMenu support
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package haskell                    ; Interactive Haskell
  :ensure haskell-mode
  :defer t
  :after haskell-mode
  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-file)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c" . haskell-process-cabal)
         :map interactive-haskell-mode-map
         ("C-c m t" . haskell-mode-show-type-at))
  :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-compile            ; Haskell compilation
  :ensure haskell-mode
  :defer t
  ;; :after haskell-mode
  :bind (:map haskell-mode-map
              ("C-c m c" . haskell-compile)
              ("<f5>" . haskell-compile))
  :config
  ;; Build with Stack
  (setq haskell-compile-cabal-build-command "stack build"))

(use-package cabal-mode                 ; Cabal files
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-cabal-mode-map
              ("C-`" . haskell-interactive-bring)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal)))

(use-package hindent                    ; Haskell indentation
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (setq hindent-style "gibiansky"))


;;; Python
(use-package python                     ; Python editing
  :defer t
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)

  (let ((ipython (executable-find "ipython")))
    (if ipython
        (setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python"))))

(use-package lunaryorn-virtualenv       ; Personal virtualenv tools
  :load-path "lisp/"
  :after python
  :commands (lunaryorn-virtualenv-init-from-workon-home)
  :init (add-hook 'python-mode-hook #'lunaryorn-virtualenv-init-from-workon-home))

(use-package flycheck-virtualenv        ; Setup Flycheck by virtualenv
  :load-path "lisp/"
  :after python
  :commands (flycheck-virtualenv-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-virtualenv-setup))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :after python
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)


;;; Rust
(use-package rust-mode                  ; Rust major mode
  :ensure t
  :defer t)

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :defer t
  :after rust-mode
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer                      ; Completion and navigation for Rust
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
  :diminish (racer-mode . "ⓡ"))

(use-package cargo                      ; Control Cargo
  :ensure t
  :bind (:map rust-mode-map ("<f5>" . cargo-process-build))
  :after rust-mode
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)


;;; HTML & Javascript
(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.html\\'" . web-mode)))

(use-package css-mode                   ; CSS
  :defer t
  :config (setq css-indent-offset 2))

(use-package js2-mode                   ; Powerful Javascript mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  ;; Disable parser errors and strict warnings.  We have Flycheck 8)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-highlight-level 3           ; Try to highlight most ECMA built-ins
        ))

(use-package js2-refactor               ; Refactor Javascript
  :ensure t
  :after js2-mode
  :defer t
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c m r"))

(use-package tern                       ; Javascript IDE backend
  :ensure t
  :defer t
  :init (add-hook 'js2-mode-hook #'tern-mode))

(use-package company-tern               ; Auto-completion for javascript
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-tern))


;;; Misc programming languages
(use-package sh-script                  ; Shell scripts
  :defer t
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (setq sh-indentation 2                ; The basic indentation
        sh-basic-offset 2               ; The offset for nested indentation
        ))

(use-package nxml-mode                  ; XML editing
  :defer t
  ;; Complete closing tags, and insert XML declarations into empty files
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t))

(use-package html5-schema               ; HTML5 schemata for NXML
  :ensure t
  :defer t)

(use-package thrift                     ; Thrift interface files
  :ensure t
  :defer t
  :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
  :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package homebrew-mode              ; Homebrew Formulae
  :ensure t
  :defer t)


;;; Databases
(use-package sql                        ; SQL editing and REPL
  :bind (("C-c a s" . sql-connect)
         :map sql-mode-map
         ("C-c m p" . sql-set-product)))

(use-package sqlup-mode                 ; Upcase SQL keywords
  :ensure t
  :after sql
  :bind (:map sql-mode-map
              ("C-c m u" . sqlup-capitalize-keywords-in-region))
  :init (add-hook 'sql-mode-hook #'sqlup-mode))


;;; Version control
(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package what-the-commit            ; Insert random commit messages
  :ensure t
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package diff-hl                    ; Highlight hunks in fringe
  :ensure t
  :defer t
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-file)
         ("C-c g p" . magit-pull))
  :config
  ;; Shut up, Magit
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        ;; Use separate buffers for one-file logs so that we don't need to reset
        ;; the filter everytime for full log view
        magit-log-buffer-file-locked t
        ;; This is creepy, Magit
        magit-revision-show-gravatars nil)

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
  :bind (("C-c g t" . git-timemachine)))

(use-package helm-gitignore             ; Generate gitignore files
  :ensure t
  :defer t
  :bind ("C-c g I" . helm-gitignore))


;;; Github integration
(use-package gh                         ; Github API library
  :defer t
  ;; Change the default profile.  The profile itself is set up via customize,
  ;; and includes auth data, to prevent it from storing tokens in Git config
  :config (setq gh-profile-default-profile "lunaryorn"))

(use-package magit-gh-pulls             ; Show Github PRs in Magit
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package github-clone               ; Clone and fork from Github
  :ensure t
  :bind ("C-c g g c" . github-clone))

(use-package helm-open-github ; Open Github pages for current repo
  ;; FIXME: Triggers a password prompt during load?!
  :disabled t
  :ensure t
  :bind (("C-c g g i" . helm-open-github-from-issues)
         ("C-c g g p" . helm-open-github-from-pull-requests)))

(use-package gist                       ; Create and list Gists
  :ensure t
  :bind (("C-c g g l" . gist-list)
         ("C-c g g b" . gist-region-or-buffer)))

;;; Project management with Projectile
(use-package projectile                 ; Project management for Emacs
  :ensure t
  :init (projectile-global-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'helm
        projectile-find-dir-includes-top-level t)

  (defun lunaryorn-neotree-project-root (&optional directory)
    "Open a NeoTree browser for a project DIRECTORY."
    (interactive)
    (let ((default-directory (or directory default-directory)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (neotree-find (projectile-project-root)))))
  :diminish projectile-mode)

(use-package helm-projectile            ; Helm frontend for Projectile
  :ensure t
  :defer t
  :after projectile
  :bind (("C-c s p" . helm-projectile-ag)
         :map helm-projectile-projects-map
              ("C-t" . lunaryorn-neotree-project-root))
  :init
  (helm-projectile-on)
  :config
  (setq projectile-switch-project-action #'helm-projectile)

  (helm-add-action-to-source "Open NeoTree `C-t'"
                             #'lunaryorn-neotree-project-root
                             helm-source-projectile-projects 1))


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


;;; Documents
(use-package doc-view
  :defer t
  :config
  ;; Render PDFs at 300dpi
  (setq doc-view-resolution 300)

  ;; Warn if Doc View falls back to Ghostscript for rendering
  (unless (eq doc-view-pdf->png-converter-function
              'doc-view-pdf->png-converter-mupdf)
    (warn "Doc View is not using mupdf.
Install mudraw with brew install mupdf-tools")))


;;; Net & Web
(use-package browse-url                 ; Browse URLs
  :bind (("C-c a u" . browse-url)))

(use-package bug-reference              ; Turn bug refs into browsable buttons
  :defer t
  :init
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'text-mode-hook #'bug-reference-mode))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode))
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

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
  ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
  ;; more nicely with the website.
  (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'sx-compose-mode-hook #'visual-line-mode)
  (add-hook 'sx-compose-mode-hook
            #'lunaryorn-whitespace-style-no-long-lines)

  ;; Clean up whitespace before sending questions
  (add-hook 'sx-compose-before-send-hook
            (lambda () (whitespace-cleanup) t))

  (bind-key "M-q" #'ignore sx-compose-mode-map))

(use-package sx-question-mode           ; Show Stack
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))


;;; Fun
(use-package zone                       ; Emacs screen saver
  :bind ("C-c z" . zone))

(use-package zone-nyan                  ; Not exactly useful but <3
  :ensure t
  :after zone
  :config (setq zone-programs (vconcat [zone-nyan] zone-programs)))


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

(use-package niceify-info               ; Prettify Info rendering
  :disabled t
  :ensure t
  :defer t
  :after info
  ;; Adds emphasis to text between * and _, tries to fontify Emacs Lisp code,
  ;; tries to cross-reference symbol names in backticks, tries to fontify
  ;; headers, etc.q
  :init (add-hook 'Info-selection-hook #'niceify-info))

(use-package helm-info
  :ensure helm
  :bind (([remap info] . helm-info-at-point)
         ("C-c h e"    . helm-info-emacs))
  :config
  ;; Also lookup symbols in the Emacs manual
  (add-to-list 'helm-info-default-sources
               'helm-source-info-emacs))

(use-package ansible-doc                ; Documentation lookup for Ansible
  :ensure t
  :defer t
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :diminish (ansible-doc-mode . " Ⓓ"))

(use-package dash-at-point              ; Jump to Dash docset at point
  :ensure t
  :defer t
  :bind (("C-c h d" . dash-at-point)
         ("C-c h D" . dash-at-point-with-docset)))

(bind-key "C-c h b" #'describe-personal-keybindings)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
