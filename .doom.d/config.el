;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Never lose you cursor again!
(beacon-mode 1)

;; I need some colors in my life
(rainbow-mode 1)
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Org mode zone :^) ;;

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Hide emphasis markers (* for bold, / for italic)
(after! org
  (setq org-hide-emphasis-markers t))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; *** My own shit *** ;;

;; Yasnippet. Idk what this does
(require 'yasnippet)
(yas-global-mode 1)

;; Font settings (Big size cuz Im blind)
(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 18))
(setq doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 26))

;; Trucate lines
(setq global-visual-line-mode 0)
(add-hook 'prog-mode-hook (lambda () (toggle-truncate-lines 0)))

;; Set bold and italic font on
(after! doom-themes
  (setq doom-themes-enable-bold t       ;; *word*
        doom-themes-enable-italic t))   ;; /word/

;; Opening a terminal with fish
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000)

;; I prefer tabs over spaces '-'
(setq-default indent-tabs-mode t) ;; nil = spaces, t = tabs

;; Change default tab sizes
(setq default-tab-width 4)

;; Programing stuff :^) ;;

;; Emphasis shit
(custom-set-faces!
  ;;'(font-lock-comment-face :slant italic)     ;; To change comments style
  '(font-lock-keyword-face :slant italic))        ;; To change keywords style
