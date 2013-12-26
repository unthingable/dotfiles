;; ** PACKAGES
;; https://github.com/emacs-jp/replace-colorthemes
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes/"))
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; bootstrap packaging
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendor/")

(setq package-user-dir "~/.emacs.d/elpa-test/")
(require 'package)

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


;; ag
(setq ag-reuse-window nil)


;; Autocomplete
(setq ac-use-fuzzy t)
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

;; Line numbers
(require 'linum)
(line-number-mode 1)
;; (column-number-mode 1)  ;; Line numbers on left most column
(global-linum-mode 1)


(require 'sr-speedbar)
(setq sr-speedbar-auto-refresh t)


(require 'projectile)
(projectile-global-mode)


(require 'tabbar)
;; turn on the tabbar
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


(require 'helm-config)

(require 'dired+)
(require 'dired-x)
;; (require 'direx)
;; (dired-omit-mode t)
;; (setq dired-omit-files "\\.pyc$")
(define-key direx:direx-mode-map [mouse-1] nil)
(define-key direx:direx-mode-map [mouse-2] nil)

;; Evil stuff ("M-x evil-mode" to toggle)
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
(setq evil-default-cursor t)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
;; evil doesn't always understant indent by default
(add-hook 'python-mode-hook
  (function (lambda ()
              (setq evil-shift-width python-indent)
              (modify-syntax-entry ?_ "w"))))   ; recognize _* as valid identifiers
;; (define-key python-mode-map (kbd "RET") 'evil-ret-and-indent)


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
(global-set-key (kbd "M-S-<tab>") 'ac-fuzzy-complete)

;; Ponder some more.
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(global-set-key (kbd "C-x 4 C-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-x C-o") 'direx-project:jump-to-project-root-other-window)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-M-;") 'comment-sexp)
(global-set-key (kbd "M-/") 'hippie-expand)
;; already defined by starter-kit-bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-\\") 'indent-for-tab-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-R") 'previous-buffer)

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

(global-set-key [f8] 'customize-themes)


(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


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


;; -- Not necessary with the new themes
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


;; (my-fix-hl-line-color)
;; (add-hook 'after-init-hook 'my-fix-hl-line-color)

;; (when (package-installed-p 'color-theme) 
;;   (defadvice color-theme-install-at-point (after fix-hl-line activate)
;;     (my-fix-hl-line-color)))

;; (defadvice load-theme (after fix-hl-line activate)
;;   (my-fix-hl-line-color))


;; experiment
(defun my-bind-window-toggle ()
  (interactive)
  (set-window-dedicated-p this-window (not (window-dedicated-p this-window))))


;; indentation settings
(require 'js2-mode)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(electric-indent-mode t)
;; indent, dammit!
(define-key js2-mode-map (kbd "RET") 'newline-and-indent)
;; (define-key python-mode-map (kbd "RET") 'evil-ret-and-indent)
(setq js2-bounce-indent-p nil)


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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "89f613708c8018d71d97e3da7a1e23c8963b798252f1ac2ab813ad63b7a4b341" "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683" "3dd173744ae0990dd72094caef06c0b9176b3d98f0ee5d822d6a7e487c88d548" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "e83c94a6bfab82536cef63610ec58d08dfddd27752d860763055daf58d028aad" "f211f8db2328fb031908c9496582e7de2ae8abd5f59a27b4c1218720a7d11803" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "923faef2c7ed017e63f517703c846c6190c31400261e8abdb1be06d5b46ea19a" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "6ecfc451f545459728a4a8b1d44ac4cdcc5d93465536807d0cb0647ef2bb12c4" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "929744da373c859c0f07325bc9c8d5cc30d418468c2ecb3a4f6cb2e3728d4775" "5562060e16ae3188e79d87e9ba69d70a6922448bcc5018205850d10696ed0116" "6394ba6170fd0bc9f24794d555fa84676d2bd5e3cfd50b3e270183223f8a6535" default)))
 '(direx:closed-icon "+ ")
 '(direx:open-icon "- ")
 '(fci-rule-color "#383838")
 '(main-line-color1 "#29282E")
 '(main-line-color2 "#292A24")
 '(powerline-color1 "#29282E")
 '(powerline-color2 "#292A24")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#bc8383")
     (40 . "#cc9393")
     (60 . "#dfaf8f")
     (80 . "#d0bf8f")
     (100 . "#e0cf9f")
     (120 . "#f0dfaf")
     (140 . "#5f7f5f")
     (160 . "#7f9f7f")
     (180 . "#8fb28f")
     (200 . "#9fc59f")
     (220 . "#afd8af")
     (240 . "#bfebbf")
     (260 . "#93e0e3")
     (280 . "#6ca0a3")
     (300 . "#7cb8bb")
     (320 . "#8cd0d3")
     (340 . "#94bff3")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; because one bad apple (fullscreen without the stupid spaces)
(setq ns-use-native-fullscreen nil)
;; then M-x toggle-frame-fullscreen


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


;; global behavior

;; ** FILES
;; always stay current
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq make-backup-files nil)
(setq auto-save-default t)

;; ** WINDOWS
;; show the menu bar, on osx it costs nothing
(menu-bar-mode 1)
(setq use-dialog-box nil)
(tool-bar-mode nil)


(setq pop-up-windows nil)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq idle-update-delay 0.5) ; not sure what this does yet
(setq idle-highlight-idle-time 0.5)

;; saner scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  mouse-wheel-progressive-speed nil
  scroll-preserve-screen-position 1)


;; Sane select and delete
(setq shift-select-mode t)
(delete-selection-mode t) ;; does not work with paredit

;; Experimental
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
;; (setq display-buffer-function 'my-display-buffer-function)



(setq ring-bell-function 'ignore) ; shut up!

;; fucking auto-fill
(defun auto-fill-mode (args)
  (message "fuck off"))
