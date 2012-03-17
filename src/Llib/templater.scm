;;;; Copyright(c) 2010, 2011, 2012 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of Templater.
;;;;
;;;;     Templater is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     Templater is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with Templater.  If not, see
;;;;     <http://www.gnu.org/licenses/>.
 
(module templater
   (export (make-templater #!key (load-path ""))
           (templater-load-path-add! templater path)
	   (templater-load-paths templater)
	   (templater-apply-template templater template attribs)
	   (templater-get-template templater template-name)
	   (templater-flush-templates! templater))
   (static
    (class %templater
       template-map
       load-paths)))

 (define (make-templater #!key (load-path ""))
    (let ((res
	   (instantiate::%templater (template-map (create-hashtable))
				    (load-paths '()))))
       (when (not (string=? "" load-path))
	  (templater-load-path-add! res load-path))
       res))


(define (templater-flush-templates! templater)
   (with-access::%templater templater (template-map)
	 (set! template-map (create-hashtable))))

(define (template-map-put! templater name template)
   (with-access::%templater templater (template-map)
	 (hashtable-put! template-map name template)))

(define (template-map-get templater name)
   (with-access::%templater templater (template-map)
	 (hashtable-get template-map name)))


(define +template-ext+ "tmplt")

(define (templater-load-path-add! templater path)
   (with-access::%templater templater (load-paths)
	 (set! load-paths (cons path load-paths))))

(define (templater-load-paths templater)
   (let ((t::%templater templater))
      (-> t load-paths)))

(define (templater-get-template templater template-name)
   (let ((res (template-map-get templater template-name)))
      (if res
	  res
	  (let* ((file-name (string-append template-name
					  "." +template-ext+))
		 (file (find-file/path file-name (templater-load-paths templater))))
	     (if file
		 (let ((template 
			  (with-input-from-file file
			     (lambda ()
				(templater-template-read
				   (current-input-port))))))
		    (template-map-put! templater template-name template)
		    template)
		 (error "templater-get-template" "template not found"
		    template-name))))))
		       
	     
	     
;;;; attribute handling

(define-struct %attribute-collection
   lst)

(define (make-attribute-collection)
   (%attribute-collection '()))

(define (attribute-collection-add! coll attrib-src)
   (%attribute-collection-lst-set! coll
        (cons attrib-src (%attribute-collection-lst coll))))

(define (%get-attribute-collection-attribute obj name)
   (let loop ((lst (%attribute-collection-lst obj)))
      (if (pair? lst)
	  (let ((res (templater-get-attribute (car lst) name)))
	     (if (equal? res "")
		 (loop (cdr lst))
		 res))
	  "")))
		     
(define (%get-hashtable-attribute obj name)
   (let ((res (hashtable-get obj name)))
      (if res
	  res
	  "")))

(define (%get-alist-attribute obj name)
   (let ((res (assoc name obj)))
      (if res
	  (cdr res)
	  "")))

(define-generic (templater-get-attribute obj name)
   (cond ((hashtable? obj)
	  (%get-hashtable-attribute obj name))
	 ((pair? obj)
	  (%get-alist-attribute obj name))
	 ((%attribute-collection? obj)
	  (%get-attribute-collection-attribute obj name))
	 (else
	  (error "get-attribute" "unsupported attribute source" obj))))

(define-method (templater-get-attribute obj::object name)
   (let* ((klass (object-class obj))
	  (field-name (string->symbol name))
	  (field (find-class-field klass field-name)))
      (if field
	  ((class-field-accessor field) obj)
	  "")))

(define-inline (fold proc seed lst)
   (if (pair? lst)
       (fold proc (proc seed (car lst)) (cdr lst))
       seed))

(define (templater-list-attribute->string attribute separator)
   (fold (lambda (s x)
	    (string-append s (templater-attribute->string x separator) separator))
	 ""
	attribute))

(define-inline (vector-fold proc seed vector)
   (define (%vector-fold proc seed vector index)
      (if (= index (vector-length vector))
	  seed
	  (%vector-fold proc
			(proc seed (vector-ref vector index))
			vector (+ index 1))))
   (%vector-fold proc seed vector 0))

(define (templater-vector-attribute->string attribute separator)
   (vector-fold (lambda (s x)
		   (string-append s (templater-attribute->string x separator) separator))
		""
		attribute))

(define-generic (templater-attribute->string attribute separator)
   (cond ((list? attribute)
	  (templater-list-attribute->string attribute
					    separator))
	 ((vector? attribute)
	  (templater-vector-attribute->string attribute
					      separator))
	 (else
	  (format "~a" attribute))))



;;;; attribute tests

#;(begin
(define-class dog
   name
   legs)


(let ((d (instantiate::dog (name "rufus") (legs 3))))
   (get-attribute d "legs"))

(let ((h (create-hashtable)))
   (hashtable-put! h "legs" 4)
   (hashtable-put! h "name" "deloris")
   (get-attribute h "name"))

(let ((a '(("legs". 3)
	   ("name" . "frank"))))
   (get-attribute a "legs")))


;;;; template parsing
(define +templater-base-tokenizer+
   (regular-grammar ()
      (#\$
       'dollar)
      ("\\$"
	 (cons 'string "$"))
      ((+ (out #\$ ))
       (cons 'string (the-string)))
      (else
       (let ((char (the-failure)))
	  (if (eof-object? char)
	      char
	      (error "+string-template-tokenizer+"
		     "Illegal character" char))))))

(define +templater-expression-tokenizer+
   (regular-grammar ()
      (#\(
	 'open-paren)
      (#\)
	 'close-paren)
      (#\!
       'exclamation)
      (#\;
       'semi-colon)
      (#\:
       'colon)
      (#\=
       'equal)
      (#\"
       (cons
	'string-literal
	(let loop ((char (read-char (the-port)))
		   (res ""))
	   (if (char=? char #\")
	       res
	       (loop (read-char (the-port))
		     (string-append res (string char)))))))
      ("separator"
       'separator)
      ("if"
       'tif)
      ("else"
       'telse)
      ("elseif"
       'telseif)
      ("endif"
       'tendif)   
      ((: alpha (* (in alnum #\- #\?)))
       (cons 'attribute (the-string)))
      (space
       (ignore))
      (else
       (let ((char (the-failure)))
	  (if (eof-object? char)
	      char
	      (error "+string-template-expression-tokenizer+"
		     "Illegal char" char))))))   
      

(define +templater-tokenizer+
   (let ((string-port #f))
      (letrec ((tokenize
		(lambda (input-port)
		   (if string-port
		       (let ((res (read/rp
				   +templater-expression-tokenizer+
				   string-port)))
			  (if (not (eof-object? res))
			      res
			      (begin
				 (set! string-port #f)
				 (tokenize input-port))))
		       (let ((res (read/rp +templater-base-tokenizer+
					   input-port)))
			  (if (eq? res 'dollar)
			      (let ((string (read/rp 
					     +templater-base-tokenizer+
					     input-port)))
				 (set! string-port (open-input-string
					      (cdr string)))
				 (let ((res (read/rp
					     +templater-base-tokenizer+
					     input-port)))
				    (when (not (eq? res 'dollar))
				       (error "+templater-tokenizer+"
					      "expected $"
					      res))
				    (tokenize input-port)))
			      res))))))
	 tokenize)))



(define +templater-parser+
   (lalr-grammar (string
		  open-paren
		  close-paren
		  exclamation
		  tif
		  telseif
		  telse
		  tendif
		  attribute
		  semi-colon
		  equal
		  separator
		  colon
		  string-literal
		  )
	(template
	 ((strorexprs)
	  strorexprs))
	(strorexprs
	 (() '())
	 ((strorexpr strorexprs)
	  (cons strorexpr strorexprs)))

	(strorexpr
	 ((string)
	  string)
	 ((expr)
	  expr))

	(expr
	 ((attribute)
	  (lambda (templater attribs)
	     (templater-attribute->string (templater-get-attribute
					   attribs attribute)
					  "")))
	 
	 ((attribute@a semi-colon separator equal attribute@b)
	  (lambda (templater attribs)
	     (templater-attribute->string (templater-get-attribute
					   attribs a)
					  (templater-attribute->string
					   (templater-get-attribute
					    attribs b) ""))))
	 
	 ((attribute@a semi-colon separator equal string-literal)
	  (lambda (templater attribs)
	     (templater-attribute->string (templater-get-attribute
					   attribs a)
					  string-literal)))
	 ((attribute@a colon attribute@b)
	  (lambda (templater attribs)
	     (let* ((attrib-a (templater-get-attribute attribs a))
		    (template-name (templater-get-attribute attribs b))
		    (template (templater-get-template templater template-name)))
		    (fold (lambda (s it)
			     (let ((coll (make-attribute-collection)))
				(attribute-collection-add! coll attribs)
				(attribute-collection-add!
				 coll `(("it" . ,it)))
				(string-append
				 s
				 (templater-apply-template templater template coll))))
			  ""
			  (if (pair? attrib-a)
			      attrib-a
			      (list attrib-a))))))

	 ((attribute@a colon string-literal)
	  (lambda (templater attribs)
	     (let* ((attrib-a (templater-get-attribute attribs a))
		    (template-name string-literal)
		    (template (templater-get-template templater
						      template-name)))
		    (fold (lambda (s it)
			     (let ((coll (make-attribute-collection)))
				(attribute-collection-add! coll attribs)
				(attribute-collection-add!
				 coll `(("it" . ,it)))
				(string-append
				 s
				 (templater-apply-template templater
							   template coll))))
			  ""
			  (if (pair? attrib-a)
			      attrib-a
			      (list attrib-a))))))
		    
	
		
		       
		
		
	     
								   
	 ((conditional)
	  conditional))

	(conditional
	 ((beginif boolexpr close-paren
		   template elseifblocks elseblock tendif)
	  (let ((tests (append (list (lambda (templater attribs)
					   (cons (boolexpr attribs)
						   template)))
				  elseifblocks
				  elseblock)))
	     
	     (lambda (templater attribs)
		;(print tests)
		(let loop ((t tests))
		   (if (pair? t)
		       (let ((res ((car t) attribs)))
			  (if (car res)
			      (cdr res)
			      (loop (cdr t))))
		       ""))))))
	
	(beginif
	 ((tif open-paren)))

	(elseifblocks
	 (() '())
	 ((elseifblock elseifblocks)
	  (cons elseifblock elseifblocks)))

	(elseifblock
	  ((telseif open-paren boolexpr close-paren template)
	   (lambda (templater attribs)
	      (cons (boolexpr attribs)
		      template))))

	(elseblock
	 (() '())
	 ((telse template)
	  (list (lambda (templater attribs)
		   (cons #t
			   template)))))

		  
	  

	(boolexpr
	 ((attribute)
	  (lambda (templater attribs)
	     (let ((v (templater-get-attribute attribs attribute)))
		(and (not (equal? v ""))
		     v))))
	 ((exclamation attribute)
	   (lambda (templater attribs)
	     (let ((v (templater-get-attribute attribs attribute)))
		(not (and (not (string=? v ""))
			  v))))))))
    		 


(define (templater-template-read input)
   (read/lalrp +templater-parser+ +templater-tokenizer+ input))

;;;; template application
(define (templater-apply-template templater template attribs)
   (define (template-apply templater template attribs)
      (if (pair? template)
	  (let ((curr (car template)))
	     (cond ((string? curr)
		    (display curr))
		   ((procedure? curr)
		    (let ((v (curr templater attribs)))
		       (if (string? v)
			   (display v)
			   (template-apply templater
					   (curr templater attribs)
					   attribs))))
		   ((pair? curr)
		    (template-apply templater curr attribs)))
	     (template-apply templater (cdr template) attribs))
	  #unspecified))
   (with-output-to-string
      (lambda ()
	 (template-apply templater template attribs))))

		   