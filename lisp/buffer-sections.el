;; Buffer sections
;; http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html
(defun insert-section ()
  (interactive)
  (insert "§§§"))

(defun forward-section ()
  "Move cursor forward to next occurrence of the SECTION SIGN § character."
  (interactive)
  (when (not (search-forward "§§§" nil t))
    (goto-char (point-max)) ) )

(defun backward-section ()
  "Move cursor backward to previous occurrence of the SECTION SIGN § character."
  (interactive)
  (when (not (search-backward "§§§" nil t))
    (goto-char (point-min)) ) )

(defun search-sections ()
  (interactive)
  (isearch-forward nil 1)
  (isearch-yank-string "§§§")
  (call-interactively 'helm-occur-from-isearch))

(provide 'buffer-sections)
