;; happily borrowed from http://jdreaver.com/emacs-org-pelican.html

(setq pelican-md-header (concat
                         "#+OPTIONS: toc:nil tex:verbatim\n"
                         "Title:\n"
                         "Date: %(markdown-time)\n"
                         "Category:\n"
                         "Tags:\n"
                         "Slug:\n"
                         "Author: %n\n"
                         "Summary:\n"
                         "%?"))

(defun capture-pelican-draft-file (dir-path)
  (let ((name (read-string "File Name: ")))
    (expand-file-name (format "%s-%s.org"
                              (format-time-string "%Y-%m-%d")
                              name) dir-path)))

(defun pelican-time ()
  (format-time-string "%Y-%m-%d %R"))

(defun insert-pelican-time ()
  (interactive)
  (insert (pelican-time)))

(setq pelican-md-header (concat
                         "#+OPTIONS: toc:nil tex:verbatim\n"
                         "Title:\n"
                         "Date: %(markdown-time)\n"
                         "Category:\n"
                         "Tags:\n"
                         "Slug:\n"
                         "Author: %n\n"
                         "Summary:\n"
                         "%?"))
