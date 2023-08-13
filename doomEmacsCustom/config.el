(setq doom-font
      ;; (font-spec :family "FiraCode Nerd Font"
      (font-spec :family "Hack Nerd Font"
                 :size 16
                 :weight 'medium)
      doom-variable-pitch-font
      (font-spec :family "FantasqueSansM Nerd Font"
                 :size 16
                 :weight 'medium))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :weight bold))

(setq-default line-spacing 0.12)

; Some colors: #89dceb #f5c2e7 #f38ba8

(map! :leader
      :desc "Comment or uncomment lines" "l" #'comment-line)

(setq evil-normal-state-cursor '(box "#89dceb")
      evil-insert-state-cursor '(hbar "#89dceb")
      evil-visual-state-cursor '(hollow "#f38ba8"))

(setq display-line-numbers-type 'relative)
(setq display-line-numbers-mode 1)
(global-visual-line-mode t)

(custom-set-faces!
  '(line-number-current-line :foreground "#f5c2e7"
                             :slant italic
                             :weight bold))

(xterm-mouse-mode 1)

(setq org-directory "~/org/")

(add-hook 'org-mode-hook 'org-indent-mode)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq confirm-kill-emacs nil) ; No more asking If I want to quit

; Why did it split ON TOP???
(after! evil
  (setq evil-vsplit-window-right t
        evil-split-window-below t))

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'org-agenda-mode)))
      (rainbow-mode 1))))
(global-rainbow-mode 1)

(setq redisplay-dont-pause t
  scroll-margin 10
  scroll-step 1
  scroll-conservatively 10000
  scroll-perserve-screen-position 1
  mouse-wheel-progressive-speed t ;; (don't) accelerate scrolling
  ;mouse-whell-scroll-amount '(10)
  mouse-whell-follow-mouse 't
)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'macchiato) ; 'latte / 'macchiato / 'latte / 'mocha
