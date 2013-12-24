;; global behavior

;; always stay current
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq make-backup-files nil)
(setq auto-save-default t)

;; show the menu bar, on osx it costs nothing
(menu-bar-mode 1)
(setq use-dialog-box nil)
(tool-bar-mode nil)

(setq idle-update-delay 0.5) ; not sure what this does yet
(setq idle-highlight-idle-time 0.5)

;; enable hax0red packages
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendor/")


;; https://github.com/emacs-jp/replace-colorthemes
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes/"))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; bootstrap packaging
(setq package-user-dir "~/.emacs.d/elpa-test/")
(require 'package)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ;; MELPA proves useless again.
                         ;; ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(defvar my-packages '(
                      dired+
                      clojure-mode
                      clojure-test-mode
                      hackernews
                      nrepl
                      paredit
                      projectile
                      tabbar
                      flx-ido
                      ido-ubiquitous
                      hexrgb
                      ;; cider
                      evil
                      evil-paredit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (not package-archive-contents) (package-refresh-contents))

(require 'starter-kit)
(require 'starter-kit-bindings)
(require 'starter-kit-lisp)
;; (require 'starter-kit-js)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq ac-use-fuzzy t)
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

(require 'linum)
(line-number-mode 1)
;; (column-number-mode 1)  ;; Line numbers on left most column
(global-linum-mode 1)


(require 'sr-speedbar)


(require 'projectile)
(projectile-global-mode)

;; (require 'color-theme)
;; (color-theme-initialize)
;; (require 'color-theme-sanityinc-solarized)
;; (require 'color-theme-sanityinc-tomorrow)
(;; cyclable themes!
setq my-color-themes (list 'sanityinc-solarized-dark
                            'sanityinc-solarized-light
                            'sanityinc-tomorrow-day
                            'sanityinc-tomorrow-night
                            'sanityinc-tomorrow-blue
                            'sanityinc-tomorrow-bright
                            'sanityinc-tomorrow-eighties))

(defun my-theme-set-default () ; Set the first row
  (interactive)
  (setq theme-current my-color-themes)
  (load-theme (car theme-current) t))

(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))

; Set the next theme (fixed by Chris Webber - thanks)
(defun my-theme-cycle ()
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (load-theme (car theme-current) t)
  (message "%S" (car theme-current)))

(setq theme-current my-color-themes)
(setq color-theme-is-global nil) ; Initialization

;; this messes with desktop save, so skip
;; (my-theme-set-default)
;; (global-set-key [f8] 'my-theme-cycle)
(global-set-key [f8] 'customize-themes)


(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)
(setq tabbar-cycle-scope 'tabs)


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-all-like-this-dwim)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-word-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(setq mc/cmds-to-run-for-all
      '(
        evil-append-line
        evil-backward-WORD-begin
        evil-backward-word-begin
        evil-delete-char
        evil-delete-line
        evil-digit-argument-or-evil-beginning-of-line
        evil-emacs-state
        evil-end-of-line
        evil-force-normal-state
        evil-forward-WORD-begin
        evil-forward-WORD-end
        evil-forward-word-begin
        evil-forward-word-end
        evil-insert
        evil-next-line
        evil-normal-state
        evil-previous-line
        evil-forward-char
        evil-backward-char
        kill-region
        evil-delete-backward-char-and-join
        ))


(require 'dired+)
(require 'dired-x)
(require 'direx)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\.pyc$")

;; evil stuff ("M-x evil-mode" to toggle)
(require 'evil)
(require 'evil-paredit)
;; (evil-mode 1)
(setq evil-auto-indent t)
(setq evil-regexp-search t)
(setq evil-want-C-i-jump t)
;; (setq evil-normal-state-cursor '("white" box))
;; (setq evil-insert-state-cursor '("white" bar))
(add-hook 'text-mode-hook 'turn-on-evil-mode)
(add-hook 'prog-mode-hook 'turn-on-evil-mode)
(add-hook 'comint-mode-hook 'turn-on-evil-mode)
(add-hook 'Info-mode-hook 'turn-off-evil-mode)

;; (setq evil-default-cursor t)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
;; evil doesn't always understant indent by default
(add-hook 'python-mode-hook
  (function (lambda ()
              (setq evil-shift-width python-indent)
              (modify-syntax-entry ?_ "w"))))   ; recognize _* as valid identifiers

;; random borrowed snippets
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(require 'buffer-move)

;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


(defun comment-sexp ()
  "Comment out the sexp at point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (paredit-comment-dwim)))

;; some keys
(global-set-key (kbd "M-<tab>") 'auto-complete)

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x j") 'direx:jump-to-directory)
(global-set-key (kbd "C-x C-o") 'direx-project:jump-to-project-root)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-M-;") 'comment-sexp)
(global-set-key (kbd "M-/") 'hippie-expand)
;; already defined by starter-kit-bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-\\") 'indent-for-tab-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "s-}") 'tabbar-forward-group)
(global-set-key (kbd "s-{") 'tabbar-backward-group)
(global-set-key (kbd "s-[") 'tabbar-backward)
(global-set-key (kbd "s-]") 'tabbar-forward)

(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)
(global-set-key [s-return] 'toggle-frame-fullscreen)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; saner scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  mouse-wheel-progressive-speed nil
  scroll-preserve-screen-position 1)


;; because one bad apple (fullscreen without the stupid spaces)
(setq ns-use-native-fullscreen nil)
;; then M-x toggle-frame-fullscreen

(setq pop-up-windows nil)

(defun my-display-buffer-function (buf not-this-window)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or not-this-window
               (not (eq (window-buffer (selected-window)) buf)))
           (> (frame-width) 162))
      (split-window-horizontally))
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer' -- Why, oh, why!
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    (display-buffer buf not-this-window)))

(setq display-buffer-function 'my-display-buffer-function)

;; (desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(desktop-read desktop-dirname)
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (desktop-save desktop-dirname))
(add-hook 'auto-save-hook 'my-desktop-save)
(add-hook 'kill-emacs-hook 'my-desktop-save)


(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


(setq ring-bell-function 'ignore) ; shut up!


;; make Emacs tells us everything it does
(defun command-peek-hook ()
  (message "running %S" this-command))
;; uncomment to start the fun
;; (add-hook 'pre-command-hook 'command-peek-hook)

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

;; trying flx
(require 'flx-ido)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

;; fix hl-line color dynamically:
(require 'hexrgb)
(defun my-fix-hl-line-color ()
  "Adjust hl-line color relative to current color theme"
  (interactive)
  (let*
      ((color (face-attribute 'default :background))
       (value (hexrgb-value color))
       (threshold 0.32)
       (scale 0.9)
       (new-value
        (+ threshold
           (* scale
              (+ (- value threshold)
                 (if (< value threshold) 0.15 (- 0.05)))))))
    (set-face-background
     hl-line-face
     (hexrgb-increment-value
      color
      (- new-value value)))
    (message "%S %S" value new-value)))

(defun my-bind-window-toggle ()
  (interactive)
  (set-window-dedicated-p this-window (not (window-dedicated-p this-window))))

;; (my-fix-hl-line-color)
;; (add-hook 'after-init-hook 'my-fix-hl-line-color)

;; (when (package-installed-p 'color-theme) 
;;   (defadvice color-theme-install-at-point (after fix-hl-line activate)
;;     (my-fix-hl-line-color)))

;; (defadvice load-theme (after fix-hl-line activate)
;;   (my-fix-hl-line-color))


;; indentation settings
(require 'js2-mode)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(electric-indent-mode t)
;; indent, dammit!
(define-key js2-mode-map (kbd "RET") 'newline-and-indent)
;; (define-key python-mode-map (kbd "RET") 'newline-and-indent)
(setq js2-bounce-indent-p nil)

(setq shift-select-mode t)
(delete-selection-mode t)


;; what's going on with tabbar and flycheck?
(defun my-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ;; Flycheck causes a buffer to temporarily become "Process" group, do not want
    ;; ((or (get-buffer-process (current-buffer))
    ;;      ;; Check if the major mode derives from `comint-mode' or
    ;;      ;; `compilation-mode'.
    ;;      (tabbar-buffer-mode-derived-p
    ;;       major-mode '(comint-mode compilation-mode)))
    ;;  "Process"
    ;;  )
    ((member (buffer-name)
             '("*scratch*" "*Messages*"))
     "Common"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(Linum-format "%7i ")
 ;; '(ansi-color-faces-vector
 ;;   [default bold shadow italic underline bold bold-italic bold])
 ;; '(ansi-color-names-vector
 ;;   (vector "#52676f" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "#fcf4dc"))
 ;; '(ansi-term-color-vector
 ;;   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 ;; '(background-color "#042028")
 ;; '(background-mode dark)
 ;; '(cursor-color "#708183")
 '(custom-enabled-themes (quote (assemblage)))
 '(custom-safe-themes
   (quote
    ("f211f8db2328fb031908c9496582e7de2ae8abd5f59a27b4c1218720a7d11803" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "923faef2c7ed017e63f517703c846c6190c31400261e8abdb1be06d5b46ea19a" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "6ecfc451f545459728a4a8b1d44ac4cdcc5d93465536807d0cb0647ef2bb12c4" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "929744da373c859c0f07325bc9c8d5cc30d418468c2ecb3a4f6cb2e3728d4775" "5562060e16ae3188e79d87e9ba69d70a6922448bcc5018205850d10696ed0116" "6394ba6170fd0bc9f24794d555fa84676d2bd5e3cfd50b3e270183223f8a6535" default)))
 '(direx:closed-icon "+ ")
 '(direx:open-icon "- ")
 ;; '(fci-rule-character-color "#202020")
 ;; '(fci-rule-color "#e9e2cb")
 ;; '(foreground-color "#708183")
 ;; '(fringe-mode 4 nil (fringe))
 ;; '(linum-format " %7d ")
 ;; '(main-line-color1 "#1e1e1e")
 ;; '(main-line-color2 "#111111")
 ;; '(main-line-separator-style (quote chamfer))
 ;; '(powerline-color1 "#1e1e1e")
 ;; '(powerline-color2 "#111111")
 ;; '(tabbar-auto-scroll-flag t)
 ;; '(tabbar-mode t nil (tabbar))
 ;; '(vc-annotate-background nil)
 ;; '(vc-annotate-color-map
 ;;   (quote
 ;;    ((20 . "#c60007")
 ;;     (40 . "#bd3612")
 ;;     (60 . "#a57705")
 ;;     (80 . "#728a05")
 ;;     (100 . "#259185")
 ;;     (120 . "#2075c7")
 ;;     (140 . "#c61b6e")
 ;;     (160 . "#5859b7")
 ;;     (180 . "#c60007")
 ;;     (200 . "#bd3612")
 ;;     (220 . "#a57705")
 ;;     (240 . "#728a05")
 ;;     (260 . "#259185")
 ;;     (280 . "#2075c7")
 ;;     (300 . "#c61b6e")
 ;;     (320 . "#5859b7")
 ;;     (340 . "#c60007")
 ;;     (360 . "#bd3612"))))
 ;; '(vc-annotate-very-old-color nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


