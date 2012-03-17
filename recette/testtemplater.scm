;;;; Copyright(c) 2010 Joseph Donaldson(donaldsonjw@yahoo.com) 
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
(module testtemplater
   (library templater)
   (main main))


;;;; testing infrastructure copied from the recette for Bigloo's pthread library

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let ((provided (with-handler
		      (lambda (e)
			 (error-notify e)
			 (vector res))
		      (prgm))))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (begin
	     (set! *success* (+fx 1 *success*))
	     (print "ok."))
	  (begin
	     (set! *failure* (cons name *failure*))
	     (print "error.")
	     (print "   ==> provided: [" provided
		    "]\n       expected: ["
		    (if (procedure? res) (res 'result) res)
		    "]")))))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))


;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (for-each (lambda (pvn)
		   (apply test pvn))
		(if (null? tests)
		    (reverse *tests*)
		    (reverse (filter (lambda (t) (memq (car t) tests))
				     *tests*))))
      ;; if we reach that point, we are done
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))


(define +templater+ (make-templater :load-path "./templates"))

;;;; cond-expand	   
(define-test cond-expand 
   (cond-expand	   
      (templater #t) 
      (else #f))	   
   :result #t)


;;;; simple attribute replacement
(define-test simple-attribute
   (let ((template (templater-get-template +templater+ "simpleattribute")))
      (templater-apply-template +templater+ template '(("person" . "sherry"))))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "hello sherry"
		  (string=? v "hello sherry"))))

;;;; simple attribute replacement 2
(define-test simple-attribute2
   
   (let ((attrib-src (create-hashtable))
	 (template (templater-get-template +templater+ "simpleattribute")))
      (hashtable-put! attrib-src "person" "sherry")
      (templater-apply-template +templater+ template attrib-src))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "hello sherry"
		  (string=? v "hello sherry"))))


(define-class coll
   person)
;;;; simple attribute replacement 3
(define-test simple-attribute3
   (let ((attrib-src (instantiate::coll (person "sherry")))
	 (template (templater-get-template +templater+ "simpleattribute")))
      (templater-apply-template +templater+ template attrib-src))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "hello sherry"
		  (string=? v "hello sherry"))))

;;;; aggregate attribute with separator
(define-test aggregate-attribute-with-separator
   (let ((attrib-src '(("people" . ("sherry" "jason" "donald"))))
	 (template (templater-get-template +templater+ "aggregate")))
      (templater-apply-template +templater+ template attrib-src))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "hello sherry=>jason=>donald=>"
		  (string=? v "hello sherry=>jason=>donald=>"))))

;;;; aggregate attribute with separator2
(define-test aggregate-attribute-with-separator2
   (let ((attrib-src '(("people" . ("sherry" "jason" "donald"))
		       ("sep" . "=>")))
	 (template (templater-get-template +templater+ "aggregate2")))
      (templater-apply-template +templater+ template attrib-src))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "hello sherry=>jason=>donald=>"
		  (string=? v "hello sherry=>jason=>donald=>"))))


;;;; apply template to attribute
(define-test apply-template-to-attribute
   (let ((attrib-src '(("animals" . ("dog" "horse" "pig"))))
	 (template (templater-get-template +templater+ "maptemplate")))
      (templater-apply-template +templater+ template attrib-src))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "cool dog cool horse cool pig "
		  (string=? "cool dog cool horse cool pig " v))))

;;;; aplply template to attribute 2
(define-test apply-template-to-attribute2
   (let ((attrib-src '(("animals" . ("dog" "horse" "pig"))
		       ("temp" . "map")))
	 (template (templater-get-template +templater+ "maptemplate2")))
      (templater-apply-template +templater+ template attrib-src))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "cool dog cool horse cool pig "
		  (string=? "cool dog cool horse cool pig " v))))