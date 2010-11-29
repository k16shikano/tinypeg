;; toy parser combinator for strings

;; Copyright (c) 2010, Keiichirou SHIKANO <k16.shikano@gmail.com>
;; All rights reserved.
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice,this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;   * Neither the name of the Keiichirou SHIKANO nor the names of its
;;     contributors may be used to endorse or promote products derived
;;     from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
;; OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-condition-type <parser-error> <error> #f)

(define (make-parser pred)
  (lambda (ts)
    (cond 
     ((null? ts)
      (error <parser-error> "no character" ts))
     ((pred (car ts))
      (values (list (car ts)) (cdr ts)))
     (else
      (error <parser-error> "doesn't match at" ts)))))

(define-syntax parser-or
  (syntax-rules (error)
    ((_) (values '() ts))
    ((_ p)
     (lambda (ts) (p ts)))
    ((_ p1 p2 ...)
     (lambda (ts)
       (guard (e
	       ((<parser-error> e) 
		((parser-or p2 ...) ts)))
	      (p1 ts))))))

(define-syntax parser-cont
  (syntax-rules ()
    ((_) (lambda (ts) (values '() ts)))
    ((_ p)
     (lambda (ts) (p ts)))
    ((_ p1 p2 ...)
     (lambda (ts)
       (receive (match-p1 rest-p1)
           (p1 ts)
	 (receive (match-p2 rest-p2)
	     ((parser-cont p2 ...) rest-p1)
	   (values `(,@match-p1 ,@match-p2) rest-p2)))))))

(define-syntax parser-do
  (syntax-rules (<- return in)
    ((_ return <r>)
     (lambda (ts) (values <r> ts)))
    ((_ return <r> in <x1> <- p1)
     (lambda (ts)
       (receive (match-p1 rest-p1)
           (p1 ts)
	 (let ((<x1> match-p1))
	   (values <r> rest-p1)))))
    ((_ return <r> in <x1> <- p1 <x2> <- p2 ...)
     (lambda (ts)
       (receive (match-p1 rest-p1)
           (p1 ts)
	 (let ((<x1> match-p1))
	   (receive (match-p2 rest-p2)
		    ((parser-do return <r> in <x2> <- p2 ... ) 
		     rest-p1)
		    (let ((<x2> match-p2))
		      (values match-p2 rest-p2)))))))))

(define anychar (make-parser char?))

(define (char-parser char)
  (make-parser (pa$ char=? char)))

(define (char-set-parser charset)
  (make-parser (pa$ char-set-contains? charset)))

(define (skip p)
  (lambda (ts)
    (guard (e
	    ((<parser-error> e) 
	     (values '() ts)))
	   (receive (m r)
	       (p ts)
	     (values '() r)))))

(define (parser-many p)
  (lambda (ts)
    (let R ((match '())
	    (rest  ts))
      (guard (e
	      ((<parser-error> e) 
	       (values match rest)))
	     (receive (m r)
	         (p rest)
	       (R (append match m) r))))))

(define (parser-many1 p)
  (parser-cont p (parser-many p)))

(define space1
  (char-set-parser #[\s]))

(define nill
  (lambda (ts)
    (values '() ts)))

(define extra-space1
  (parser-or space1 nill))

(define extra-space
  (skip space1))




;;; Example 1

(define (run parser str)
  (receive (matched rest)
      (parser (string->list str))
    (values (list->string matched) 
	    (list->string rest))))

(define parser-group
  (parser-or
   (parser-cont
    (char-parser #\{)
    (parser-or 
     (parser-many1 (char-set-parser #[^{}]))
     parser-group)
    (char-parser #\})
    parser-group)
   nill))

(run parser-group "{{abc}{123}}rest...")


;;; Example 2

(define count-nest
  (parser-or
   (parser-do return (max (+ n 1) m)
	      in op <- (char-parser #\{)
	         n  <- count-nest
		 cp <- (char-parser #\})
		 m  <- count-nest)
   (lambda (ts) (values 0 ts))))

(count-nest (string->list "{{}}{{{}}}"))

