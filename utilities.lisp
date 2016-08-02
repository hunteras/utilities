;;;; utilities.lisp

(in-package #:utilities)

;;; "utilities" goes here. Hacks and glory await!

(defun last1 (lst)
  "Return the last element in a list."
  (car (last lst)))

(defun single (lst)
  "Test whether something is a list of one element. "
  (and (listp lst) (null (cdr lst))))

(defun append1 (lst obj)
  "Attach a new element to the end of a list."
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Attach a new element to the end of a list, destructively."
  (nconc lst (list obj)))

(defun mklist (obj)
  "Make sure obj is a list."
  (if (listp obj) obj (list obj)))
    
(defun longer (x y)
  "Compare two sequence and return true only if the first is longer."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "Returns a list of whatever non-nil values are returned by the function as it is applied to the elements of the list. "
  (labels ((rec (l acc)
             (cond ((null l) acc)
                   ((funcall fn (car l)) (rec (cdr l) (append1 acc (car l))))
                   (t (rec (cdr l) acc)))))
    (rec lst nil)))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (last1 fns))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))
;; macros
(defmacro mac (expr)
  `(pprint (macroexpand-1 ,expr)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
     ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (x) `(eql ,insym ,x))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a) args)))

