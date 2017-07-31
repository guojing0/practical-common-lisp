(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;;; test examples

(deftest test-+ ()
  (check (= (+ 1 2) 2)
	 (= (+ 1 2 3) 6)
	 (= (+ -10 -20) -30)))

(deftest test-* ()
  (check (= (* 4 2) 8)
	 (= (* 5 -3) -15)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
