(defun range (n &optional (start 0) (step 1) &aux res)
  "List of N integers from START with STEP increments"
  (dotimes (i n)
    (push start res)
    (incf start step))
  (nreverse res))

(defun hash-table-list (ht &key (sort-fn nil) &aux res)
  "Convert hashtable HT into list of (key . value) pairs"
  (flet ((pair (key value)
	   (push (cons key value) res)))
    (maphash #'pair ht)
    (if sort-fn
        (sort res (lambda (a b) (funcall sort-fn (car a) (car b))))
        res)))

(defun hash-table-incf (ht key &optional (increment 1))
  (incf (gethash key ht 0) increment))

(defun hash-table-aref-incf (ht key i n &optional (increment 1))
  (when (not (gethash key ht))
    (setf (gethash key ht) (make-array n)))
  (incf (aref (gethash key ht) i) increment))
      
(defun tokenize (str &optional (delimiters #(#\space #\tab #\return #\newline #\,)) &aux res)
  (flet ((delimiter-p (c)
           (position c delimiters)))
    (do* ((p (position-if-not #'delimiter-p str))
          (oldp p p))
         ((null p) )
      (when (setf p (position-if #'delimiter-p str :start p))
        (push (subseq str oldp p) res)
        (setf p (position-if-not #'delimiter-p str :start (1+ p)))))
    (let ((tail (position-if #'delimiter-p str :from-end t)))
      (if tail
          (when (< tail (1- (length str)))
            (push (subseq str (1+ tail)) res))
          (push (copy-seq str) res)))
    (nreverse res)))

    
(defun frequence (seq &key (sort-value-fn nil) (sort-key-fn nil))
  (let ((ht (make-hash-table :test #'equal)))
    (map nil (lambda (x) 
               (incf (gethash x ht 0)))
         seq)
    (if sort-value-fn
        (sort (hash-table-list ht) (lambda (a b) (funcall sort-value-fn (cdr a) (cdr b))))
        (if sort-key-fn
            (sort (hash-table-list ht) (lambda (a b) (funcall sort-key-fn (car a) (car b))))
            (hash-table-list ht)))))

(defun group (seq &key (map-fn nil))
  "Groups sequence SEQ of consed pair by car"
  (let ((ht (make-hash-table :test #'equal)))
    (map nil (lambda (x)
               (if (gethash (car x) ht)
                   (push (cdr x) (gethash (car x) ht))
                   (setf (gethash (car x) ht) (list (cdr x)))))
         seq)
    (let ((res (hash-table-list ht)))
      (if map-fn
          (mapcar (lambda (x) (cons (car x) (funcall map-fn (cdr x)))) res)
          res))))

(defun histogram-ascii (freq-list &key (width 60) (value-fn nil) (bar-char #\o))
  (let* ((values (mapcar (if value-fn
                             (lambda (x) (funcall value-fn (cdr x)))
                             #'cdr)
                         freq-list))
         (keys (mapcar #'car freq-list))
         (max-value (reduce #'max values))
         (key-width (reduce #'max (mapcar (lambda (x) (length (format nil "~A" x))) keys)))
         (value-width (reduce #'max (mapcar (lambda (x) (length (format nil "~A" x))) values)))
         (fmt (format nil "~~~D@A ~~~D@A ~~A~~%" key-width value-width)))
    (dolist (f (mapcar #'cons keys values))
      (format t fmt (car f) (cdr f) 
              (make-string (floor (* (cdr f) width) max-value) :initial-element bar-char)))))
  
(defun split (list len)
  "Splits elements of LIST into sublists of LEN elements"
  (if (zerop (mod (length list) len))
      (do ((p list (cdr p))
           (n 0 (1+ n))
           (res))
          ((null p) (nreverse (mapcar #'nreverse res)))
        (if (zerop (mod n len))
            (push (list (car p)) res)
            (push (car p) (car res))))
      (error "Length of LIST is not multiple of LEN")))

(defun head (seq &optional (n 10))
  "Sequence of first N elements in SEQ"
  (if (>= n (length seq))
      seq
      (subseq seq 0 n)))

(defun tail (seq &optional (n 10))
  "Sequence of last N elmenets in SEQ"
  (if (>= n (length seq))
      seq
      (subseq seq (- (length seq) n))))

(defun enumerate (seq &optional (start 0))
  (map 'list
       (lambda (x) (cons (1- (incf start)) x))
       seq))

(defun v-sum (v)
  (reduce #'+ v))

(defun v-max (v)
  (reduce #'max v))

(defun v-min (v)
  (reduce #'min v))

(defun v-avg (v)
  (coerce (/ (v-sum v) (length v)) 'double-float))

(defun v-median (v)
  (let ((vs (sort v #'<))
        (n (length v)))
    (if (zerop (mod n 2))
        (/ (+ (elt vs (floor n 2)) (elt vs (1- (floor n 2)))) 2)
        (elt vs (floor n 2)))))

(defun uniform-vector (n)
  (make-array n :element-type 'double-float :initial-element (/ 1.0d0 n)))

(defun unit-vector (n)
  (make-array n :element-type 'double-float :initial-element 1.0d0))
       
(defun percentage (v)
  "Map densities 0..1 to percentages 0..100"
  (map (type-of v) (lambda (x) (round (* 100 x))) v))

(defun normalize (v)
  "Normalize vector sum to 1.0"
  (let ((sum (coerce (reduce #'+ v) 'double-float)))
    (map (type-of v) (lambda (x) (/ x sum)) v)))

(defun random-pool (weights)
  "Weighted random element (0..N-1) from list of weights"
  (let ((r (random 1.0d0))
        (sum 0d0)
        (n 0))
    (dolist (w (normalize weights))
      (when (<= r (incf sum w))
          (return-from random-pool n))
      (incf n))))

(defstruct data-bank 
  (schema)         ;; vector of (type convert-fn)
  (columns))

(defun create-data-bank (schema &key (initial-size 1024))
  (let ((db (make-data-bank :schema (coerce schema 'vector)
                            :columns (make-array (length schema)))))
    (dotimes (i (length schema))
      (setf (aref (data-bank-columns db) i)
            (make-array initial-size :element-type (car (aref (data-bank-schema db)  i)) :adjustable t :fill-pointer 0)))
    db))

(defun db-get-row (db pos &optional (cols nil))
    (when (< -1 pos (fill-pointer (aref (data-bank-columns db) 0)))
        (if cols
            (let* ((n (length cols))
                   (res (make-array n :fill-pointer 0)))
              (dolist (c cols)
                (vector-push (aref (aref (data-bank-columns db) c) pos) res))
              res)
            (let* ((n (array-dimension (data-bank-columns db) 0))
                   (res (make-array n)))
              (dotimes (i n)
                (setf (aref res i)
                      (aref (aref (data-bank-columns db) i) pos)))
              res))))

(defun db-convert-row (db row)
  "Convert external ROW into internal format according to schema"
  (let* ((n (array-dimension (data-bank-columns db) 0))
         (m (length row))
         (res (make-array n :initial-element nil)))
    (dotimes (i (min n m))
      (let ((col-spec (aref (data-bank-schema db) i)))
        (when col-spec
          (let ((fn (cadr col-spec)))
            (setf (aref res i)
                  (if fn 
                      (funcall fn (aref row i))
                      (aref row i)))))))
    res))
  
(defun db-push (db row)
  (let ((res (db-convert-row db row))
        (n (array-dimension (data-bank-columns db) 0)))
    (dotimes (i n)
      (vector-push-extend (aref res i)
                          (aref (data-bank-columns db) i)))
    res))

(defun db-size (db)
  (fill-pointer (aref (data-bank-columns db) 0)))

(defun db-column (db col-id)
  (aref (data-bank-columns db) col-id))

(defun db-map (db map-fn &rest cols)
  (declare (inline db-column db-size))
  (dotimes (i (db-size db))
    (apply map-fn (mapcar (lambda (x) (aref (db-column db x) i)) 
                          cols))))

(defun db-select (db &key (cols nil) (where nil) (limit (db-size db)))
  (do ((res nil)
       (max-limit (db-size db))
       (i 0 (1+ i))
       (n 0))
      ((or (>= i max-limit) 
           (>= n limit)) (nreverse res))
    (let ((row (db-get-row db i)))
      (when (or (null where)
                (funcall where row))
        (push (if cols
                  (let ((new-row (make-array (length cols) :fill-pointer 0)))
                    (dolist (c cols new-row)
                      (vector-push 
                       (if (listp c)
                           (funcall (cadr c) (aref row (car c)))
                           (aref row c))
                       new-row)))
                  row)
              res)
        (incf n)))))
      
    
    