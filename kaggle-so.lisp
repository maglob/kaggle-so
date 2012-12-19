(ql:quickload "cl-csv")

(load "tools.lisp")

(deftype i32 () '(signed-byte 32))
(deftype u32 () '(unsigned-byte 32))
(deftype u16 () '(unsigned-byte 16))
(deftype i8 () '(signed-byte 8))

(defconstant +status-open+                0 "open")
(defconstant +status-not-a-real-question+ 1 "not a real question")
(defconstant +status-not-constructive+    2 "not constructive")
(defconstant +status-off-topic+           3 "off topic")
(defconstant +status-too-locallized+      4 "too localized")
(defconstant +status-nill+ -1 "null")
(defconstant +status-count+ 5)
                                                  
(defconstant +epsilon+ 0.001d0)

(defconstant +col-id+                  0)
(defconstant +col-creation-date+       1)
(defconstant +col-owner-id+            2)
(defconstant +col-owner-creation-date+ 3)
(defconstant +col-reputation-at-post+  4)
(defconstant +col-undeleted-answer-count-at-post+ 5)
(defconstant +col-title+               6)
(defconstant +col-body+                7)
(defconstant +col-tag-1+        8)
(defconstant +col-tag-2+        9)
(defconstant +col-tag-3+       10)
(defconstant +col-tag-4+       11)
(defconstant +col-tag-5+       12)
(defconstant +col-closed-date+ 13)
(defconstant +col-status+      14)

(defvar +ht-tags+ (make-hash-table))
(defvar +ht-tag-ids+ (make-hash-table :test 'equal))
(defvar +status-frequency+)
(defvar +data+ nil)

(defparameter +answer-brackets+ #(0 1 2 3 4 5 6 7 8 9 10 100 1000 10000 100000))
(defparameter +reputation-brackets+ #(0 1 2 3 4 5 6 7 8 9 10 100 1000 10000 100000))
(defparameter +age-brackets+ #(-1 0 1 2 3 4 5 6 7 8 10 20 30 50 100 300 1000 2000 100000))

(defstruct (feature
	     (:constructor create-feature (name fn-extract)))
  name
  fn-extract
  (ht-freqs (make-hash-table :test #'equal))
  (active t))

(defun extract-tag (row col)
  (let ((val (aref row col)))
    (when (not (zerop val)) val)))

(defparameter +features+
  (list 
   (create-feature "Undeleted answer count"
		   (lambda (row) 
		     (bracket-id (aref row +col-undeleted-answer-count-at-post+) +answer-brackets+)))
   (create-feature "Reputation at post"
		   (lambda (row) 
		     (bracket-id (aref row +col-reputation-at-post+) +reputation-brackets+)))
   (create-feature "User age at post"
		   (lambda (row) 
                     (bracket-id
                      (floor (- (aref row +col-creation-date+) (aref row +col-owner-creation-date+)) 
                             (* 3600 24))
                      +age-brackets+))) 
   (create-feature "Tags" (lambda (row) 
                            (remove-if #'zerop
                                       (coerce (subseq row +col-tag-1+ (1+ +col-tag-5+)) 'list))))
  (create-feature "Title tokens" (lambda (row)
                                    (tokenize (aref row +col-title+)))) 
                   

))

(defun run-full-slim ()
  (init-all)
  (read-csv-file #P"~/data/train-slim.csv")
  (process-data-bank +data+)
  (predict #P"~/data/public_leaderboard.csv"))

(defun run-10k-slim ()
  (init-all)
  (read-csv-file #P"~/data/train-slim-10k.csv")
  (process-data-bank +data+)
  (predict #P"~/data/train-slim-1k.csv" :logloss-p t))

(defun closed-percentage (status-array)
  (- 1d0 (/ (aref status-array 0) 
            (reduce #'+ status-array))))

(defun collect-status-ht (ht key status)
  (if (gethash key ht)
      (incf (aref (gethash key ht) status))
      (incf (aref (setf (gethash key ht) 
                        (make-array +status-count+)) status))))

(defun stat-close-percentage-per-age (&optional (db +data+) (brackets +age-brackets+))
  (flet ((md (data)
           (bracket-upper-bound data brackets)))
    (histogram-ascii 
     (mapcar (lambda (x) 
               (cons (car x) (closed-percentage (cdr x))))
             (let ((ht (make-hash-table)))
               (db-map db (lambda (create-date reg-date status) 
                            (collect-status-ht ht (md (floor (- create-date reg-date) (* 3600))) status))
                       1 3 14)
               (hash-table-list ht :sort-fn #'<))))))


(defun stat-close-percentage-per-answer-bracket (&optional (db +data+) (brackets +answer-brackets+))
  (flet ((md (data)
           (bracket-upper-bound data brackets)))
    (histogram-ascii 
     (mapcar (lambda (x) 
               (cons (car x) (closed-percentage (cdr x))))
             (let ((ht (make-hash-table)))
               (db-map db (lambda (answer-count status) 
                            (collect-status-ht ht (md answer-count) status))
                       5 14)
               (hash-table-list ht :sort-fn #'<))))))


(defun stat-close-percentage-per-year-month-day (&optional (db +data+))
  (flet ((ymd (date)
           (multiple-value-bind (se mi ho da mo yr)
               (decode-universal-time date)
             (declare (ignore se mi ho))
             (+ (* 10000 yr) (* 100 mo) da))))
    (histogram-ascii 
     (mapcar (lambda (x) 
               (cons (car x) (closed-percentage (cdr x))))
             (let ((ht (make-hash-table)))
               (db-map db (lambda (date status) 
                            (collect-status-ht ht (ymd date) status))
                       1 14)
               (hash-table-list ht :sort-fn #'<))))))

(defun stat-close-percentage-per-day-of-week (&optional (db +data+))
  (flet ((dow (date)
           (multiple-value-bind (se mi ho da mo yr dow)  
               (decode-universal-time date)
             (declare (ignore se mi ho da mo yr))
             dow)))
    (histogram-ascii 
     (mapcar (lambda (x) 
               (cons (car x) (closed-percentage (cdr x))))
             (let ((ht (make-hash-table)))
               (db-map db (lambda (date status) 
                            (collect-status-ht ht (dow date) status))
                       1 14)
               (hash-table-list ht :sort-fn #'<))))))


(defun stat-close-percentage-per-year-month (&optional (db +data+))
  (flet ((year-month (date)
           (multiple-value-bind (se mi ho da mo yr)  
               (decode-universal-time date)
             (declare (ignore se mi ho da))
             (+ (* yr 100) mo))))
    (histogram-ascii 
     (mapcar (lambda (x) 
               (cons (car x) (closed-percentage (cdr x))))
             (let ((ht (make-hash-table)))
               (db-map db (lambda (date status) 
                            (collect-status-ht ht (year-month date) status))
                       1 14)
               (hash-table-list ht :sort-fn #'<))))))

(defun stat-close-percentage-per-reputation-bracket (&optional (db +data+) (brackets +reputation-brackets+))
  (histogram-ascii 
   (enumerate 
    (map 'list (lambda (x) (- 1 (/ (elt x 0) (reduce #'+ x) 1.0))) 
         (let* ((res (map 'vector (lambda (x) (setf x (make-array +status-count+))) 
                          (make-array (1+ (length brackets))))))
           (db-map db (lambda (rank status) 
                        (incf (aref (aref res (bracket-id rank brackets)) status))) 4 14) 
           res)))))

(defun stat-close-percentage-per-hour (&optional (db +data+))
    (histogram-ascii 
     (mapcar (lambda (x) 
               (cons (caaar x) (/ (cdadr x) (cdar x) 1.0))) 
             (split (frequence 
                     (map 'list (lambda (time status) 
                                  (list (floor (mod time (* 3600 24)) 3600) (zerop status))) 
                          (db-column db +col-creation-date+) 
                          (db-column db +col-status+)) 
                     :sort-key-fn (lambda (a b) 
                                    (or (< (car a) (car b)) 
                                        (and (= (car a) (car b)) (cadr a))))) 
                    2))))

(defun stat-new-posts-per-hour (&optional (db +data+))
  (histogram-ascii 
   (frequence (map 'list (lambda (x) 
                           (floor (mod x (* 3600 24)) 3600)) 
                   (db-column db +col-creation-date+)) 
              :sort-key-fn #'<)))

(defun stat-posts-per-user (&optional (db +data+))
  (histogram-ascii 
   (frequence (mapcar #'cdr 
                      (frequence (db-column db +col-owner-id+) 
                                 :sort-value-fn #'>)) :sort-key-fn #'>)))

(defun bracket-id (value brackets)
  (let ((res (position value brackets :test #'<)))
    (if res
        res
        (length brackets))))

(defun bracket-upper-bound (value brackets)
  (find value brackets :test #'<))

(defun tag-id (tag-name)
  "Tag id for given TAG-NAME"
  (if (gethash tag-name +ht-tag-ids+)
      (gethash tag-name +ht-tag-ids+)
      (setf (gethash tag-name +ht-tag-ids+)
            (hash-table-count +ht-tag-ids+))))

(defun tag-name (tag-id)
  "Tag name for given TAG-ID"
  (maphash (lambda (key value)
             (when (equal tag-id value)
               (return-from tag-name key)))
           +ht-tag-ids+))

(defun status-id (status)
  (let ((id (position (string-trim #(#\return) status)
                      '("" "open" "not a real question" "not constructive" "off topic" "too localized") 
                      :test #'equal)))
    (when id
      (1- id))))

(defun date-string-year-month (uni-time)
  "Universal time as string in format YYYY-MM"
  (multiple-value-bind (se mi ho da mo yr)  
      (decode-universal-time uni-time)
    (declare (ignore se mi ho da))
    (format nil "~D-~2,'0D" yr mo)))

(defun parse-date (date)
  "Parse date with format of 'MM/DD/YYYY HH:MI:SS' or 'YYYY-MM-DD' into universal time"
  (flet ((ss-int (a b) 
           (parse-integer (subseq date a b))))
    (ecase (length date)
      ;; Empty string
      (0 0)
      ;; YYYY-MM-DD
      (10 (encode-universal-time 0 0 0
                                 (ss-int 8 10)
                                 (ss-int 5 7)
                                 (ss-int 0 4)))
      ;; MM/DD/YYYY HH:MI:SS
      (19 (encode-universal-time 
           (ss-int 17 19)
           (ss-int 14 16)
           (ss-int 11 13)
           (ss-int 3 5)
           (ss-int 0 2)
           (ss-int 6 10))))))

(defun clamp (x)
  "Clamp X into range +epsilon+ .. (- 1 +epsilon+)"
  (min (- 1 +epsilon+)
       (max +epsilon+ x)))

(defun init-all ()
  "Initialize all"
  (clrhash +ht-tag-ids+)
  (setf (gethash "" +ht-tag-ids+) 0)
  (setf +data+ 
        (create-data-bank
         '((u32 parse-integer)
           (u32 parse-date)
           (u32 parse-integer)
           (u32 parse-date)
           (i32 parse-integer)
           (i32 parse-integer)
           (string)
           (string null)
           (u16 tag-id)
           (u16 tag-id)
           (u16 tag-id)
           (u16 tag-id)
           (u16 tag-id)
           (u32 parse-date)
           ( i8 status-id))))
  (init))

(defun init ()
  "Reset stats"
  (clrhash +ht-tags+)
  (dolist (f +features+)
    (clrhash (feature-ht-freqs f)))
  (setf +status-frequency+ (make-array +status-count+ :element-type 'integer :initial-element 0)))


(defun impact-tag-list (tag-list &optional (default (unit-vector +status-count+)))
  (if tag-list
       (apply #'mulvec (mapcar #'impact-tag tag-list))
      default))

(defun impact-tag (tag &optional (default (unit-vector +status-count+)))
  "Calculate impact of evidence TAG"
  (let ((tag-freq (gethash tag +ht-tags+)))
    (if (and tag-freq (>= (v-sum tag-freq) 10))
        (let ((res (make-array +status-count+ :element-type 'double-float)))
          (dotimes (i +status-count+)
            (setf (aref res i)
                  (coerce (/ (/ (+ (aref tag-freq i) (/ 1 (v-sum tag-freq)))
                                (+ (aref +status-frequency+ i) 1) )
                             (/ (+ (v-sum tag-freq) 0)
                                (+ (v-sum +status-frequency+) 0)))
                          'double-float)))
          res)
        default)))

(defun feature-impact (feature-key ht-feature-freqs &key (min-sum 10) (default (unit-vector +status-count+)))
  "Calculate impact of feature"
  (if (listp feature-key)
      (if (null feature-key)
          default
          (apply #'mulvec (mapcar (lambda (x) (feature-impact x ht-feature-freqs :default default))
                                  feature-key)))
      (let ((freq (gethash feature-key ht-feature-freqs)))
        (if (and freq 
                 (>= (v-sum freq) min-sum))
            (let ((res (make-array +status-count+ :element-type 'double-float)))
              (dotimes (i +status-count+)
                (setf (aref res i)
                      (coerce (/ (/ (+ (aref freq i) (/ 1 (v-sum freq)))
                                    (+ (aref +status-frequency+ i) 1) )
                                 (/ (+ (v-sum freq) 0)
                                    (+ (v-sum +status-frequency+) 0)))
                              'double-float)))
              res)
            default))))

(defun strip (vectors &key (preserve-order nil))
  (when (and vectors (car vectors))
    (when preserve-order
      (setf vectors (reverse vectors)))
    (let* ((n (array-dimension (car vectors) 0))
           (res (make-array n :element-type 'list :initial-element nil)))
      (dotimes (i n)
        (dolist (v vectors)
          (push (aref v i)
                (aref res i))))
      res)))

(defun mulvec (a &rest rest)
  (let ((res (make-array (length a) :element-type 'double-float :initial-contents a)))
    (dolist (v rest)
      (dotimes (i (length a))
        (setf (aref res i)
              (* (aref res i) (aref v i)))))
    res))

(defun combine (a b)
  "Combine probability densities A and B"
  (map 'vector (lambda (x y) (/ (* x y) 
                                (+ (* x y) 
                                   (* (- 1.0d0 x) (- 1.0d0 y)))))
       a b))


(defun process-data-bank (db)
  "Process rows from DATA-BASE db"
  (init)
  (dotimes (i (db-size db))
    (process-row (db-get-row db i)))
  +status-frequency+)

(defun process-row (row &aux (status (aref row +col-status+)))
  "Process single row (of DATA-BANK)"
  ;; Global tag frequency
  (incf (aref +status-frequency+ status))
  ;; Tag frequency per tags
;  (dolist (tag (coerce (subseq row +col-tag-1+ (1+ +col-tag-5+)) 'list))
;    (when (> tag 0) 
;      (hash-table-aref-incf +ht-tags+ tag status +status-count+)))
  (dolist (f +features+)
    (let ((val (funcall (feature-fn-extract f) row)))
      (if (listp val)
          (dolist (v val)
              (hash-table-aref-incf (feature-ht-freqs f)
                                    v
                                    status 
                                    +status-count+))
            (hash-table-aref-incf (feature-ht-freqs f)
                                  val
                                  status 
                                  +status-count+)))))

(defun train (path)
  (init)
  (with-open-file (stream path :direction :input)
    (let ((read-char-fn (create-buffered-char-reader stream))
          (n 0))
      (do ((row (read-csv-row read-char-fn) (read-csv-row read-char-fn)))
          ((null row) )
        (when (> n 0)
          (process-row (db-convert-row +data+ row)))
        (incf n)
        (when (zerop (mod n 10000))
          (format t ".")
          (finish-output)))
      (format t "~%")
      (finish-output)))
  +status-frequency+)

(defun predict-row (row)
  (let ((default-density (normalize +status-frequency+))
        (unit-density (unit-vector +status-count+))
        (v-res))
    (setf v-res unit-density)
;    (setf v-res (impact-tag-list 
;                 (coerce (subseq row +col-tag-1+ (1+ +col-tag-5+)) 'list)
;                 unit-density))
    (dolist (f +features+)
      (let ((val (funcall (feature-fn-extract f) row)))
        (setf v-res (mulvec v-res (feature-impact val
                                                  (feature-ht-freqs f))))))
    (values (normalize (map 'vector #'clamp  
                            (normalize
                             (mulvec default-density v-res))))
            (mulvec default-density v-res))))

(defun top-word (feature words prio-open)
  (let ((ht (feature-ht-freqs feature)))
    (caar
     (sort (mapcar (lambda (x)
                     (let ((a (gethash x ht)))
                       (cons x (if a
                                   (abs (- prio-open (/ (aref a 0) (v-sum a))))
                                   0))))
                   words) 
           (lambda (a b) (> (cdr a) (cdr b)))))))

(defun probs-row-2 (row)
  (let ((res (make-array +status-count+ :element-type 'double-float))
        (pc 1))
    (dotimes (k +status-count+)
      (setf (aref res k)
            (/ (* (/ (aref +status-frequency+ k)
                     (v-sum +status-frequency+))
                  (let ((p1 1d0))
                    (dolist (f +features+)
                      (when (feature-active f)
                        (let ((freqs (gethash (funcall (feature-fn-extract f) row)
                                              (feature-ht-freqs f))))
                          (setf p1 (* p1 (/ (aref freqs k)
                                            (aref +status-frequency+ k)))))))
                    p1))
               pc)))
    res))

(defun get-pc (freqs)
  (let ((res 0d0))
    (dotimes (i +status-count+)
      (incf res (* (/ (aref +status-frequency+ i)
                      (v-sum +status-frequency+))                   
                   (/ (aref freqs i)
                      (v-sum freqs)))))
    res))

(defun update-probs (probs freqs)
  (if (and freqs (> (v-sum freqs) 1000))
      (let ((res (make-array +status-count+ :element-type 'double-float)))
        (dotimes (i +status-count+)
          (setf (aref res i) 
                (/ (* (aref probs i)
                      (/ (1+ (aref freqs i))
                         (1+ (v-sum freqs))))
                   (/ (aref +status-frequency+ i)
                      (v-sum +status-frequency+)))))
        (normalize res))
                                        ;(map 'vector #'clamp res)))
      probs))
  

(defun probs-row (row &key (debug nil))
  (let ((res (make-array +status-count+ :element-type 'double-float :initial-contents (normalize +status-frequency+)))
        (prio-open (aref (normalize +status-frequency+) 0)))
    (dolist (f +features+)
      (when debug
        (print (list "feature" (feature-name f) res)))
      (when (feature-active f)
        (let ((key (funcall (feature-fn-extract f) row)))
          (if (listp key)
              (dolist (k (if (eq f (nth 4 +features+)) 
                             (list (top-word f key prio-open))
                             key))
                (let ((freqs (gethash k (feature-ht-freqs f))))
                  (when debug
                    (print (list "freqs" k freqs)))
                  (setf res (update-probs res freqs))))
              (let ((freqs (gethash key (feature-ht-freqs f))))
                (when debug
                  (print (list "freqs" key freqs)))
                (setf res (update-probs res freqs)))))))
    (values (normalize (map 'vector #'clamp res)) res)))

(defun prod-open (row)
  (let ((probs (make-array +status-count+ :element-type 'double-float)))
    (dotimes (i +status-count+)
      (let ((pcd 1.0d0))
        (dolist (f +features+)
          (let* ((v (funcall (feature-fn-extract f) row))
                 (a (gethash v (feature-ht-freqs f))))
            (when a
              (if (listp a)
                  (print (list "list" a))
                  (setf pcd (* pcd 
                               (/ (aref a i)
                                  (v-sum a))))))))
        (setf (aref probs i)
              (/ pcd (expt (/ (aref +status-frequency+ i)
                              (v-sum +status-frequency+))
                           (1- (length +features+)))))))
    (values (normalize probs) probs)))
              
                             

                 
(defun predict (file &key (logloss-p nil))
  (with-open-file (out #P"~/data/result.csv" 
                       :direction :output 
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (format out "id,not a real question,not constructive,off topic,open,too localized~%")
    (with-open-file (stream file :direction :input)
      (let ((read-char-fn (create-buffered-char-reader stream))
            (nrow 0)
            (n 0)
            (logloss 0))
        (do ((row  (read-csv-row read-char-fn) (read-csv-row read-char-fn)))
            ((null row) )
          (when (> nrow 0)
            (setf row (db-convert-row +data+ row))
;            (let ((res (predict-row row)))
            (let ((res (probs-row row)))
;            (let ((res (probs-row-2 row)))
              (when logloss-p
                (incf n)
                (incf logloss (log (aref res (aref row +col-status+)))))
              (format out "~A,~F,~F,~F,~F,~F~%" 
                      (aref row +col-id+)
                      (aref res +status-not-a-real-question+) 
                      (aref res +status-not-constructive+) 
                      (aref res +status-off-topic+) 
                      (aref res +status-open+) 
                      (aref res +status-too-locallized+))))
          (incf nrow))
        (when logloss-p
          (- (/ logloss n)))))))

(defun predict-data-bank (db &key (logloss-p nil))
  (with-open-file (out #P"~/data/result.csv" 
                       :direction :output 
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (format out "id,not a real question,not constructive,off topic,open,too localized~%")
      (let ((nrow 0)
            (n 0)
            (logloss 0))
        (dotimes (i (db-size db))
          (let ((row (db-get-row db i)))
            ;(when (> nrow 0)
             ; (setf row (db-convert-row +data+ row))
                                        ;            (let ((res (predict-row row)))
              (let ((res (probs-row row)))
                (when logloss-p
                  (incf n)
                  (incf logloss (log (aref res (aref row +col-status+)))))
                (format out "~A,~F,~F,~F,~F,~F~%" 
                        (aref row +col-id+)
                        (aref res +status-not-a-real-question+) 
                        (aref res +status-not-constructive+) 
                        (aref res +status-off-topic+) 
                        (aref res +status-open+) 
                        (aref res +status-too-locallized+))))
            (incf nrow))
          (when logloss-p
            (- (/ logloss n))))))
  
(defun split-csv-file (file-in file-out-a file-out-b weight-a &key (slim nil))
  "Split csv-file FILE-IN in two parts FILE-OUT-A and FILE-OUT-B"
  (with-open-file (out-a file-out-a :direction :output 
                         :if-exists :rename-and-delete
                         :if-does-not-exist :create)
    (with-open-file (out-b file-out-b :direction :output 
                           :if-exists :rename-and-delete
                           :if-does-not-exist :create)
      (format out-a "PostId,PostCreationDate,OwnerUserId,OwnerCreationDate,ReputationAtPostCreation,OwnerUndeletedAnswerCountAtPostTime,Title,BodyMarkdown,Tag1,Tag2,Tag3,Tag4,Tag5,PostClosedDate,OpenStatus~%")
      (format out-b "PostId,PostCreationDate,OwnerUserId,OwnerCreationDate,ReputationAtPostCreation,OwnerUndeletedAnswerCountAtPostTime,Title,BodyMarkdown,Tag1,Tag2,Tag3,Tag4,Tag5,PostClosedDate,OpenStatus~%")
      (cl-csv:do-csv (row file-in :skip-first-p t)
        (cl-csv:write-csv-row (if slim
                                  (append (subseq row 0 6) '("" "") (subseq row 8 15))
                                  row)
                              :stream (if (< (random 1.0d0) weight-a)
                                          out-a
                                          out-b))))))

(defun read-csv-file (path &key (skip-first-p t) (data-bank +data+))
  "Reads CSV file into DATA-BANK"
  (with-open-file (stream path :direction :input)
    (let ((read-char-fn (create-buffered-char-reader stream))
          (n 0))
      (when skip-first-p
        (read-csv-row read-char-fn))
      (do ((row (read-csv-row read-char-fn) (read-csv-row read-char-fn)))
          ((null row) )
        (db-push data-bank row)
        (when (zerop (mod (incf n) 10000))
          (format t ".")
          (finish-output)))
      (format t "~%")
      (db-size data-bank))))

(defun create-buffered-char-reader (stream &optional (size 4096))
  (declare (optimize (speed 3))
           (type fixnum size))
  (let ((buffer (make-array size :element-type 'base-char))
        (nread 0)
        (pos 0))
    (declare (type fixnum nread pos))
    (lambda (&key (peek nil)) 
      (when (= pos nread)
        (setf nread (read-sequence buffer stream :end size))
        (setf pos 0))
      (when (< pos nread)
        (aref buffer (1- (if peek (1+ pos) (incf pos))))))))


(defun read-csv-row (read-char-fn)
  (declare (optimize (speed 1))
           (inline vector-push-extend))
  (do ((fields (make-array 8 :element-type 'vector :adjustable t :fill-pointer 0))
       (token (make-array 32 :element-type 'base-char :adjustable t :fill-pointer 0))
       (in-quote nil)
       (c (funcall read-char-fn) (funcall read-char-fn)))
      ((null c) nil)
    (if in-quote
        (if (char= c #\")
            (if (char= (funcall read-char-fn :peek t) #\")
                (vector-push-extend (funcall read-char-fn) token)
                (setf in-quote (not in-quote)))
            (vector-push-extend c token))
        (case c
          (#\Newline
           (let ((col #|(string-trim #(#\return)|# (copy-seq token)))
             (when (> (length col) 0)
               (vector-push-extend col fields))
           (return-from read-csv-row
             (if (zerop (length fields)) 
                 nil
                 fields))))
          (#\,
           (vector-push-extend (copy-seq token) fields)
           (setf (fill-pointer token) 0))
          (#\"
           (setf in-quote (not in-quote)))
          (otherwise
           (vector-push-extend c token))))))
