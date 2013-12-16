;; global behavior

(setq initial-frame-alist '((width . 187) (height . 57)))
;; (desktop-save-mode 1)
;; always stay current
(global-auto-revert-mode t)

;; bootstrap packaging
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      clojure-mode
                      clojure-test-mode
                      cider
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
(require 'solarized-theme)
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

(setq make-backup-files nil)


;; some keys
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
;; already defined by starter-kit-bindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-\\") 'indent-for-tab-command)

;; the internet said this helps
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-<up>") 'next-buffer)
(global-set-key (kbd "s-<down>") 'previous-buffer)

;; makes the square visual bell go away
(add-to-list 'load-path "~/.emacs.d/")
(load "bell")


;; saner scrolling
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
