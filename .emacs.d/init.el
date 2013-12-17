;; global behavior

;; always stay current
(global-auto-revert-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)

(hl-line-mode 1)
(menu-bar-mode 1)


;; bootstrap packaging
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(
                      starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      dired+
;;                      diredx
                      clojure-mode
                      clojure-test-mode
                      hackernews
                      nrepl
                      paredit
                      ;; cider
                      evil
                      evil-paredit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (not package-archive-contents) (package-refresh-contents))

;; (require 'jedi)
;; (setq jedi:setup-keys t)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; (require 'sr-speedbar)

(setq pe/omit-regex "\.\\(pyc\\)$")
(require 'project-explorer)

(require 'projectile)
(projectile-global-mode)
;; (setq projectile-require-project-root nil)

;; cyclable themes!
(require 'color-theme)
;; (color-theme-initialize)
;; (require 'soothe-theme)
;; (require 'tomorrow-night-theme)
;; (require 'tomorrow-theme)
;; (require 'tomorrow-night-bright-theme)

(require 'color-theme-solarized)
(setq my-color-themes (list 'solarized-dark
                            'solarized-light))

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
(my-theme-set-default)
(global-set-key [f8] 'my-theme-cycle)


(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)

(require 'dired+)
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.[^.]\\|\\.pdf$\\|\\.pyc$")

;; evil stuff ("M-x evil-mode" to toggle)
(require 'evil)
(require 'evil-paredit)
(evil-mode 1)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'python-mode-hook
  (function (lambda ()
          (setq evil-shift-width python-indent))))

;; random borrowed snippets
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


;; some keys
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
;; already defined by starter-kit-bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-\\") 'indent-for-tab-command)

(global-set-key (kbd "C-x C-b") 'ibuffer)


(global-set-key (kbd "s-<up>") 'tabbar-forward-group)
(global-set-key (kbd "s-<down>") 'tabbar-backward-group)
(global-set-key (kbd "s-[") 'tabbar-backward)
(global-set-key (kbd "s-]") 'tabbar-forward)


;; saner scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  mouse-wheel-progressive-speed nil
  scroll-preserve-screen-position 1)

(require 'ido-ubiquitous)

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

;; makes the square visual bell go away (this is also how we know
;; everything has loaded)
(add-to-list 'load-path "~/.emacs.d/")
(load "bell")
