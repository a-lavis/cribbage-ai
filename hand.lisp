(defun make-hand (c1 c2 c3)
  (list c1 c2 c3))

(defun hand-value (cut hand)
  (+ (fifteen-score cut hand)
     (tuple-score cut hand)
     (run-score cut hand)
     (flush-score cut hand)))A

(defun power-set (listy)
  (if (null listy)
    listy
    (let ((rest-pset (power-set (rest listy))))
      (append (map (lambda (subby)
                     (cons (first listy) subby))
                   rest-pset)
              rest-pset))))

(defun fifteen-score (cut hand)
  (labels ((count-fifteens (pset)
                           (cond ((null pset) 0)
                                 ((= (apply #'+ hand) 15)
                                  (+ 1 (count-fifteens (rest pset))))
                                 (else (count-fifteens (rest pset))))))
    (count-fifteens (power-set (cons cut hand)))))

(defun tuple-score (cut hand)
  (labels ((count-tuples (ordered counter current)
                         (cond ((null ordered) 0)
                               ((= (first ordered) current)
                                (

  (count-tuples (sort (cons cut hand) #'<)))

