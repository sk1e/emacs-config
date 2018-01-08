(defun buffer-file-name/no-extension ()
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(defun camel-case->lisp-case (str)
  (let ((case-fold-search nil))
    (mapconcat (lambda (x) (downcase (car x)))
               (s-match-strings-all (rx (: upper-case (+ lower-case)))
                                    str)
               "-")))


(defun project:get-cache ()
  (cl-labels
      ((loop
        (dir &aux (cache-file (format "%s.project/cache.el" dir)))
        (cond
         ((string= dir "/") '())
         ((file-exists-p cache-file)
          (with-temp-buffer
            (insert-file-contents cache-file)
            (car (read-from-string (buffer-string-no-properties (current-buffer))))))
         (t (loop (file-name-directory (directory-file-name dir)))))))
    (loop (file-name-directory (buffer-file-name (current-buffer))))))

(defun project:save-cache (cache)
  (cl-labels
      ((loop
        (dir)
        (cond
         ((string= dir "/") (user-error "could not find .config directory"))
         ((file-directory-p (format "%s.project" dir))
          (with-temp-file (format "%s.project/cache.el" dir)
            (insert (prin1-to-string cache))))
         (t (loop (file-name-directory (directory-file-name dir)))))))
    (loop (file-name-directory (buffer-file-name (current-buffer))))))

(defun web-dev:get-feature-name ()
  (cl-labels
      ((loop
        (dir &aux
             (dir-file (directory-file-name dir))
             (parent-dir (file-name-directory dir-file))
             (parent-file-nondir (file-name-nondirectory (directory-file-name parent-dir))))
        (cond
         ((string= dir "/") (user-error "could not get name of feature"))
         ((string= parent-file-nondir "features") (file-name-nondirectory dir-file))
         (t (loop parent-dir)))))
    (loop (file-name-directory (buffer-file-name (current-buffer))))))


(defun yas:get-action-type (action-name)
  (format "%s:%s" (web-dev:get-feature-prefix) (s-upcase (s-snake-case action-name))))

(defun web-dev:get-feature-prefix ()
  (let* ((cache (project:get-cache))
         (feature-name (web-dev:get-feature-name))
         (prefix (cdr (assoc feature-name cache))))
    (cond
     (prefix prefix)
     (t (let ((new-prefix (read-string (format "Prefix for feature `%s'" feature-name))))
          (project:save-cache (cons (cons feature-name new-prefix) cache)))))))

(defun all-px->rem ()
  (interactive)
  (while (re-search-forward (rx (1+ digit) (? "." (0+ digit)) "px") nil t)
    (cl-destructuring-bind (start-point end-point) (mapcar #'marker-position (match-data))
      (let ((number (string-to-number (buffer-substring start-point (- end-point 2)))))
        (replace-match (format "%.2frem" (/ number 14.0)))))))

(provide 'utils)
