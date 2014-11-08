(defmacro ¬ (&rest quoted-data)
  `(quote ,quoted-data))

(defmacro ¬ (&rest quoted-data)
  (labels ((process-forms (form quote)
	     (cond ((null form) nil)
		   ((atom form) (if quote 
				    `(quote ,form)
				    form))
		   ((and (listp form) (eq '¬ (first form)))
		    (append (when (not quote) '(list))
			     (loop :for f :in (rest form)
				   :collect (process-forms
					     f (not quote)))))
		   ((listp form) 
		    (append (when quote '(list))
			    (loop :for f :in form
				  :collect (process-forms
					    f quote))))
		   (t (error "how?")))))
    (process-forms quoted-data t)))

(¬ 1 2 3)

(¬ 1 (2 4) 3 (¬ + 2 3))

