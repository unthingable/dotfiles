;; fucking auto-fill
;; (defun auto-fill-mode (args)
;;   (message "auto-fill: no wai"))


;; ** PACKAGES
;; https://github.com/emacs-jp/replace-colorthemes
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes/"))
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(defun windmove-default-keybindings () (message "windmove: no wai"))

;; bootstrap packaging
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/vendor/")

(setq package-user-dir "~/.emacs.d/elpa-test/")
(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(defvar my-packages '(
                      auto-complete
;;                      starter-kit
;;                      starter-kit-lisp
;;                      starter-kit-bindings
                      js2-mode
                      buffer-move
                      dired+
                      ;; direx
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
                      evil-paredit
                      multiple-cursors
                      keyfreq
                      rainbow-mode
                      rainbow-delimiters))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (package-refresh-contents)
(when (not package-archive-contents) (package-refresh-contents))

;;(require 'starter-kit)
;;(require 'starter-kit-bindings)
;;(require 'starter-kit-lisp)

;; (require 'starter-kit-js)
(require 'sunrise-commander)
(require 'sunrise-x-tree)

(require 'enaml)
;; (add-to-list 'auto-mode-alist  '("\\.enaml$" . enaml-mode))
;; (add-hook 'enaml-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'electric-indent-functions)
;;                  (list (lambda (arg) 'no-indent)))))
(define-key enaml-mode-map [tab] 'python-indent-shift-right)
(define-key enaml-mode-map [S-tab] 'python-indent-shift-left)

(require 'cider)
(define-key clojure-mode-map [s-return] (lambda ()
                                          (interactive)
                                          (if (region-active-p)
                                              (cider-eval-region (region-beginning) (region-end))
                                            (cider-eval-last-sexp))))

(require 'extempore)
(require 'tidal)
(define-key tidal-mode-map [s-return] (lambda ()
                                          (interactive)
                                          (if (region-active-p)
                                              (progn (previous-line)
                                                (tidal-run-multiple-lines))
                                            (tidal-run-line))))
(define-key tidal-mode-map (kbd "s-.") (lambda ()
                                         (interactive)
                                         (tidal-send-string "hush")))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(require 'asciidoc-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))

(add-to-list 'auto-mode-alist '("\\.xtm\\'" . extempore-mode))
;; (setq auto-mode-alist (rassq-delete-all 'visual-line-mode auto-mode-alist))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'markdown-mode-hook (lambda ()
                                ;; (visual-line-mode t)
                                (turn-off-evil-mode)))
(add-hook 'asciidoc-mode-hook (lambda () (visual-line-mode t)))
(setq line-move-visual nil)

;; ag
(setq ag-reuse-window t)
;; (setq ag-reuse-buffers nil)

;; Autocomplete
(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(setq ac-use-fuzzy t)
(ac-config-default)

;; Line numbers
;; (require 'linum)
;; (line-number-mode 1)
;; (column-number-mode 1)  ;; Line numbers on left most column
;; (global-linum-mode 0)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook  (lambda () (idle-highlight-mode t)))

;; (require 'sr-speedbar)
;; (setq sr-speedbar-auto-refresh t)

(require 'recentf)
(recentf-mode t)

(require 'projectile)
(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-switch-to-buffer)


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

(require 'keyfreq)
(keyfreq-mode t)

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

;; (add-hook 'text-mode-hook 'turn-on-evil-mode)
;; (add-hook 'prog-mode-hook 'turn-on-evil-mode)
;; (add-hook 'comint-mode-hook 'turn-on-evil-mode)
(add-hook 'Info-mode-hook 'turn-off-evil-mode)
(add-hook 'org-mode-hook 'turn-off-evil-mode)
(add-hook 'direx-mode-hook 'turn-off-evil-mode)

(setq evil-default-cursor t)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
;; evil doesn't always understant indent by default
(add-hook 'python-mode-hook
          (function (lambda ()
                      ;; (define-key python-mode-map (kbd "RET") 'evil-ret-and-indent)
                      (setq evil-shift-width python-indent)
                      (setq ac-use-fuzzy nil)
                      (modify-syntax-entry ?_ "w"))))   ; recognize _* as valid identifiers
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

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

;; (defun comment-or-uncomment-region-or-line ()
;;     "Comments or uncomments the region or the current line if there's no active region."
;;     (interactive)
;;     (apply-to-region-or-line 'comment-or-uncomment-region))

;; (defun apply-to-region-or-line (fun)
;;   (let (beg end)
;;     (if (region-active-p)
;;         (setq beg (region-beginning) end (region-end))
;;       (setq beg (line-beginning-position) end (line-end-position)))
;;     (apply fun '(beg end))))


(defun comment-sexp ()
  "Comment out the sexp at point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (paredit-comment-dwim)))

(defun projectile-find-tag-interactive (tagname)
  (interactive (find-tag-interactive "Pfind tag: "))
  (visit-tags-table (projectile-project-root) t)
  ;; (tags-completion-table)
  (find-tag tagname))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key [M-backspace] 'backward-delete-word)

;; some keys
(global-set-key (kbd "M-X") 'smex)
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
;; (global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-x 4 j") 'dired-jump-other-window)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
(global-set-key (kbd "C-x j") 'direx:jump-to-directory)
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
;; buffer sufring
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "s-R") 'previous-buffer)
(global-set-key (kbd "s-T") 'next-buffer)

(global-set-key (kbd "s-}") 'tabbar-forward-group)
(global-set-key (kbd "s-{") 'tabbar-backward-group)
(global-set-key (kbd "s-[") 'tabbar-backward)
(global-set-key (kbd "s-]") 'tabbar-forward)

(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<down>") 'windmove-down)

(global-set-key [s-return] 'eval-region)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "<M-s-down>") 'projectile-find-tag-interactive)
(global-set-key (kbd "s-d") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "s-G") 'isearch-repeat-backward)
(global-set-key (kbd "s-K") 'undo-kill-buffer)

(global-set-key [f8] 'customize-themes)
(global-set-key (kbd "M-s-ƒ") 'toggle-frame-fullscreen)



;; format-time-string is better, this left here for reference
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


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
(setq js2-use-font-lock-faces t)
(setq js-indent-level 2)
(setq js2-basic-offset 2)

(electric-indent-mode t)
;; indent, dammit!
(define-key js2-mode-map (kbd "RET") 'newline-and-indent)
;; (define-key python-mode-map (kbd "RET") 'evil-ret-and-indent)
(setq js2-bounce-indent-p nil)


(setq my-group-by-project nil)
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
           (if my-group-by-project
               (projectile-project-name)
             (format "%s:%s" (projectile-project-root) group))
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

(defun my-toggle-group-by-project ()
  (interactive)
  (setq my-group-by-project (not my-group-by-project))
  (message "Grouping by project alone: %s"
           (if my-group-by-project "enabled" "disabled"))
  (setq cached-ppn (my-cached 'my-tabbar-buffer-groups)))


;; (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ag-arguments
   (quote
    ("--smart-case" "--nogroup" "--ignore" "*migrations" "--ignore" "TAGS" "--column" "--")))
 '(ag-highlight-search t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 '(ansi-term-color-vector
   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"] t)
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (my-deeper-blue)))
 '(custom-safe-themes
   (quote
    ("1a8f59a0b5769efb44cb727a799354e0851e7509d5160676cc01eaf74434990f" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "0ff3aeed353697992d100ddf8a94d065a58ffbde5a40afefa605f211757c8ab0" "1faffcddc50d5dc7d334f2817dd6f159ef1820be3aad303eb7f74006531afdff" "31ba13fd560daff5b05e11d4be7d280213249225e85969ec5bc71532e788ee81" "fc89666d6de5e1d75e6fe4210bd20be560a68982da7f352bd19c1033fb7583ba" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "af4cfe7f2de40f19e0798d46057aae0bccfbc87a85a2d4100339eaf91a1f202a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "180adb18379d7720859b39124cb6a79b4225d28cef4bfcf4ae2702b199a274c8" "16e7c7811fd8f1bc45d17af9677ea3bd8e028fce2dd4f6fa5e6535dea07067b1" "f7c428459d84a94cd3d4e8d9a7aa3178a13ee0092d91d5e4a4fce4c2b1242934" "617219c11282b84761477059b9339da78ce392c974d9308535ee4ec8c0770bee" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "89f613708c8018d71d97e3da7a1e23c8963b798252f1ac2ab813ad63b7a4b341" "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683" "3dd173744ae0990dd72094caef06c0b9176b3d98f0ee5d822d6a7e487c88d548" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "e83c94a6bfab82536cef63610ec58d08dfddd27752d860763055daf58d028aad" "f211f8db2328fb031908c9496582e7de2ae8abd5f59a27b4c1218720a7d11803" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0" "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "923faef2c7ed017e63f517703c846c6190c31400261e8abdb1be06d5b46ea19a" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "6ecfc451f545459728a4a8b1d44ac4cdcc5d93465536807d0cb0647ef2bb12c4" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "929744da373c859c0f07325bc9c8d5cc30d418468c2ecb3a4f6cb2e3728d4775" "5562060e16ae3188e79d87e9ba69d70a6922448bcc5018205850d10696ed0116" "6394ba6170fd0bc9f24794d555fa84676d2bd5e3cfd50b3e270183223f8a6535" default)))
 '(direx:closed-icon "+ ")
 '(direx:open-icon "- ")
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/HEAD/libexec/emacs/24.3.50/x86_64-apple-darwin13.0.0" "/usr/local/bin")))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#202020")
 '(foreground-color "#708183")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(help-window-select t)
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
 '(js2-basic-offset 2)
 '(linum-delay t)
 '(linum-eager nil)
 '(linum-format "%3i")
 '(main-line-color1 "#1e1e1e")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(org-indirect-buffer-display (quote current-window))
 '(powerline-color1 "#1e1e1e")
 '(powerline-color2 "#111111")
 '(projectile-tags-command "/usr/local/bin/ctags -Re %s")
 '(sml/inactive-background-color "gray40")
 '(sml/shorten-modes t)
 '(sml/theme nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c60007")
     (40 . "#bd3612")
     (60 . "#a57705")
     (80 . "#728a05")
     (100 . "#259185")
     (120 . "#2075c7")
     (140 . "#c61b6e")
     (160 . "#5859b7")
     (180 . "#c60007")
     (200 . "#bd3612")
     (220 . "#a57705")
     (240 . "#728a05")
     (260 . "#259185")
     (280 . "#2075c7")
     (300 . "#c61b6e")
     (320 . "#5859b7")
     (340 . "#c60007")
     (360 . "#bd3612"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-7-face ((t (:foreground "firebrick2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "turquoise2")))))


;; because one bad apple (fullscreen without the stupid spaces)
(setq ns-use-native-fullscreen nil)
;; then M-x toggle-frame-fullscreen

;; always split vertically
(setq split-height-threshold nil)
(setq split-width-threshold 200) ; may need to tune this

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(idle-highlight-mode t)

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

;; (global-rainbow-delimiters-mode t)
(rainbow-delimiters-mode t)

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

(defun my-desaturate-parens ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) -30))))


(defun relative-rainbow (start &optional max-hue steps)
  (let* ((start-hue (hexrgb-hue start))
         (steps (or steps 9))
         (max-hue (or max-hue (- float-pi (/ float-pi steps)))))
    (print start-hue)
    (mapcar (lambda (index)
              (let
                  ((offset (- (mod (+ index start-hue)
                                   float-pi)
                              start-hue)))
                  ;; ((offset (mod (+ index start-hue) max-hue)))
                (print (list index offset))
                (hexrgb-increment-hue start offset)))
            (number-sequence 0 max-hue (/ (float max-hue) steps)))))

(defun my-rainbow-colors ()
  (interactive)
  (relative-rainbow
   (hexrgb-complement (face-attribute 'default :background))
   0.9
   9))

;; borrowed from http://www.emacswiki.org/emacs/RecentFiles
;; (not quite working)
(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
	 (buffer-files-list
	  (delq nil (mapcar (lambda (buf)
			      (when (buffer-file-name buf)
				(expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
	     (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

;; (load-file "pelican.el")

;; (require 'org-freemind)

;; **** ORG ****
(require 'org-install)
(require 'org-table)
(require 'org)
(require 'org-archive)
(require 'epresent)
(require 'org-present)
(require 'org-journal)

; Some initial langauges we want org-babel to support
; (org-babel-do-load-languages
;  'org-babel-load-languages
;  '(
;    (sh . t)
;    (python . t)
;    (R . t)
;    (ruby . t)
;    (ditaa . t)
;    (dot . t)
;    (octave . t)
;    (sqlite . t)
;    (perl . t)
;    ))
; (setq org-src-fontify-natively t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "~/Documents/org")
(setq org-default-notes-file "~/Documents/org/refile.org")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (concat org-dir "/diary.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file org-default-notes-file)
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file org-default-notes-file)
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file org-default-notes-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)


(auto-fill-mode -1)
(turn-off-auto-fill)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(remove-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'turn-off-auto-fill)
(add-hook 'prog-mode-hook #'turn-off-auto-fill)

;; (defun auto-fill-mode (args)
;;   (message "auto-fill: no wai"))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))


(setq sml/theme 'light)
(setq sml/shorten-modes 't)
(setq sml/shorten-directory 't)
(require 'smart-mode-line)
(sml/setup)

(setq-default display-buffer-reuse-frames nil)

(setq web-mode-engines-alist '(("php" . "\\.phtml\\'")
                               ("blade" . "\\.blade\\.")
                               ("django" . "\\.html\\'")))

(setq mac-emulate-three-button-mouse t)
(my-fix-hl-line-color)
(exec-path-from-shell-initialize)

(setq user-extempore-directory "/usr/local/Cellar/extempore/0.58/")
(autoload 'extempore-mode "/usr/local/Cellar/extempore/0.58/extras/extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
