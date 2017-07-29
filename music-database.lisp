(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title  title
	:artist artist
	:rating rating
	:ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (format t "~{~{~a:~8t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t)
       0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: "))
	 (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd)
		     (equal artist (getf cd :artist)))
		 *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal artist (getf cd :artist))))

(defun rating-selector (rating)
  #'(lambda (cd)
      (equal rating (getf cd :rating))))

(defun make-comparison-expr (field value)
  `(equal ,value (getf cd ,field)))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

; (defun where (&key title artist rating (ripped nil ripped-p))
; #'(lambda (cd)
;     (and
;      (if title    (equal title  (getf cd :title))  t)
;      (if artist   (equal artist (getf cd :artist)) t)
;      (if rating   (equal rating (getf cd :rating)) t)
;      (if ripped-p (equal ripped (getf cd :ripped)) t))))

(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row)
	 *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
