;;;; defuns


;; http://www.emacswiki.org/emacs/UnfillParagraph
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; http://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun xmllint-format-buffer ()
  "Reformat buffer with xmllint."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint --format --encode UTF-8 -" nil t))
