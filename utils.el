(defun buffer-file-name/no-extension ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(defun camel-case->lisp-case (str)
  (let ((case-fold-search nil))
    (mapconcat (lambda (x) (downcase (car x)))
               (s-match-strings-all (rx (: upper-case (+ lower-case)))
                                    str)
               "-")))


(provide 'utils)

