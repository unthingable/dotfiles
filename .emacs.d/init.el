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
(setq ag-reuse-window t)
(setq ag-reuse-buffers t)


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
(require 'direx)
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
                      (define-key python-mode-map (kbd "RET") 'evil-ret-and-indent)
                      (setq evil-shift-width python-indent)
                      (setq ac-use-fuzzy nil)
                      (modify-syntax-entry ?_ "w"))))   ; recognize _* as valid identifiers

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

(global-set-key (kbd "C-x 9")
                (lambda ()
                  (interactive)
                  (delete-other-windows)
                  (split-window-right)
                  (other-window -1)))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-all-like-this-dwim)

;; Ponder some more.
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x 4 j") 'dired-jump-other-window)
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

(global-set-key (kbd "<M-s-down>") 'find-tag)
(global-set-key (kbd "s-d") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "s-G") 'isearch-repeat-backward)

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
    ((eq major-mode 'direx:direx-mode)
     "Direx"
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
     (let ((group 
            (if (and (stringp mode-name)
                     ;; Take care of preserving the match-data because this
                     ;; function is called when updating the header line.
                     (save-match-data (string-match "[^ ]" mode-name)))
                mode-name
              (symbol-name major-mode))))
       (if (projectile-project-p)
           (format "%s:%s" group (projectile-project-name))
         group))
     ))))

(defun my-cached (func)
  "Turn a function into a cache dict."
  (lexical-let ((table (make-hash-table :test 'equal))
                (f func))
    (lambda (key)
      (let ((value (gethash key table)))
        (if value
            value
          (puthash key (funcall f) table))))))

;; evaluate again to clear cache
(setq cached-ppn (my-cached 'my-tabbar-buffer-groups))

(defun my-tabbar-groups-by-project ()
  (funcall cached-ppn (buffer-name)))

(setq tabbar-buffer-groups-function 'my-tabbar-groups-by-project)
;; (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(Linum-format "%7i ")
 ;; '(ansi-color-faces-vector
 ;;   [default bold shadow italic underline bold bold-italic bold])
 ;; '(ansi-color-names-vector
 ;;   (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
 ;; '(ansi-term-color-vector
 ;;   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 ;; '(background-color "#042028")
 ;; '(background-mode dark)
 ;; '(cursor-color "#708183")
 '(custom-enabled-themes (quote (deep-blue)))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "180adb18379d7720859b39124cb6a79b4225d28cef4bfcf4ae2702b199a274c8" "16e7c7811fd8f1bc45d17af9677ea3bd8e028fce2dd4f6fa5e6535dea07067b1" "f7c428459d84a94cd3d4e8d9a7aa3178a13ee0092d91d5e4a4fce4c2b1242934" "617219c11282b84761477059b9339da78ce392c974d9308535ee4ec8c0770bee" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "89f613708c8018d71d97e3da7a1e23c8963b798252f1ac2ab813ad63b7a4b341" "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683" "3dd173744ae0990dd72094caef06c0b9176b3d98f0ee5d822d6a7e487c88d548" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "e83c94a6bfab82536cef63610ec58d08dfddd27752d860763055daf58d028aad" "f211f8db2328fb031908c9496582e7de2ae8abd5f59a27b4c1218720a7d11803" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "923faef2c7ed017e63f517703c846c6190c31400261e8abdb1be06d5b46ea19a" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "6ecfc451f545459728a4a8b1d44ac4cdcc5d93465536807d0cb0647ef2bb12c4" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "929744da373c859c0f07325bc9c8d5cc30d418468c2ecb3a4f6cb2e3728d4775" "5562060e16ae3188e79d87e9ba69d70a6922448bcc5018205850d10696ed0116" "6394ba6170fd0bc9f24794d555fa84676d2bd5e3cfd50b3e270183223f8a6535" default)))
 '(direx:closed-icon "+ ")
 '(direx:open-icon "- ")
 ;; '(fci-rule-character-color "#202020")
 ;; '(fci-rule-color "#efefef")
 '(foreground-color "#708183")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#F2F2F2" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#6DA8D2" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#F2F2F2" . 100))))
 '(linum-format "%3i")
 ;; '(main-line-color1 "#29282E")
 ;; '(main-line-color2 "#292A24")
 ;; '(main-line-separator-style (quote chamfer))
 ;; '(powerline-color1 "#3d3d68")
 ;; '(powerline-color2 "#292945")
 '(projectile-tags-command "/usr/local/bin/ctags -Re %s")
 ;; '(vc-annotate-background nil)
 ;; '(vc-annotate-color-map
 ;;   (quote
 ;;    ((20 . "#c82829")
 ;;     (40 . "#f5871f")
 ;;     (60 . "#eab700")
 ;;     (80 . "#718c00")
 ;;     (100 . "#3e999f")
 ;;     (120 . "#4271ae")
 ;;     (140 . "#8959a8")
 ;;     (160 . "#c82829")
 ;;     (180 . "#f5871f")
 ;;     (200 . "#eab700")
 ;;     (220 . "#718c00")
 ;;     (240 . "#3e999f")
 ;;     (260 . "#4271ae")
 ;;     (280 . "#8959a8")
 ;;     (300 . "#c82829")
 ;;     (320 . "#f5871f")
 ;;     (340 . "#eab700")
 ;;     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
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

;; save more
(add-hook 'focus-out-hook 'save-buffer)

;; global behavior

;; ** FILES
;; always stay current
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
;; (setq make-backup-files nil)
(setq backup-directory-alist `(("." . "~/.saves")))
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
(delete-selection-mode t) ; does not work with paredit


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

(global-rainbow-delimiters-mode t)

;; parens too bleak?
(require 'cl-lib)
(require 'color)
(defun my-saturate-parens ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))


(defun relative-rainbow (start &optional max-hue steps)
  (unless steps (setq steps 7))
  (unless max-hue (setq max-hue 1))
  (let ((start-hue (hexrgb-hue start)))
    (print start-hue)
    (mapcar (lambda (index)
              (let ((offset (- (mod (+ index start-hue) max-hue) start-hue)))
                (print (list index offset))
                (hexrgb-increment-hue start offset)))
            (number-sequence 0 max-hue (/ (float max-hue) steps)))))

(defun my-rainbow-colors ()
  (interactive)
  (relative-rainbow
   (hexrgb-complement (face-attribute 'default :background))
   0.9
   9))
