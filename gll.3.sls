(library (gll (3))
  (export define-parser
          succeed
          string
          bind
          seq
          alt
          red)
  (import (match)
          (only (scheme) set-car! set-cdr! force delay)
          (rename (rnrs)
                  (string rnrs-string)))

  (define (stream-cons a b) (cons a (delay b)))
  (define (stream-car stream) (car stream))
  (define (stream-cdr stream) (force (cdr stream)))
  (define (stream-append s1 s2)
    (if (null? s1) 
        s2 
        (stream-cons (stream-car s1) 
                     (stream-append (stream-cdr s1) s2))))
  (define (sequence->stream sequence)
    (if (null? sequence)
        '()
        (stream-cons (car sequence) (sequence->stream (cdr sequence)))))

  (define (trampoline%)
    (define stack (list))
    (define table (list))

    (define (has-next?)
      (not (null? stack)))

    (define (step)
      (when (has-next?)
        (let* ((next (car stack))
               (fn (car next))
               (args (cdr next)))
          (set! stack (cdr stack))
          (apply fn args))))

    (define (push-stack fn . args)
      (let ((call (cons fn args)))
        (set! stack (cons call stack))))

    (define (push fn str cont)
      (define entry-continuations car)
      (define entry-results cdr)
      (define (push-continuation! entry cont)
        (set-car! entry (cons cont (entry-continuations entry))))
      (define (push-result! entry result)
        (set-cdr! entry (cons result (entry-results entry))))
      (define (result-subsumed? entry result)
        (member result (entry-results entry)))
      (define (make-entry)
        (cons '() '()))
      (define (empty? entry)
        (and (null? (car entry)) (null? (cdr entry))))
      (define (table-ref fn str)
        (define fn-entry (assoc fn table))
        (if fn-entry
            (begin
              (let* ((fn (car fn-entry))
                     (memo (cdr fn-entry))
                     (str-entry (assoc str memo)))
                (if str-entry
                    (cdr str-entry)
                    (let ((entry (make-entry)))
                      (set-cdr! fn-entry (cons (cons str entry) memo))
                      entry))))
            (begin
              (let* ((entry (make-entry))
                     (memo (list (cons str entry))))
                (set! table (cons (cons fn memo) table))
                entry))))
      (let ((entry (table-ref fn str)))
        (if (empty? entry)
            (begin
              (push-continuation! entry cont)
              ;; push the parser on the stack
              (push-stack fn str self
                          (lambda (result)
                            (unless (result-subsumed? entry result)
                              (push-result! entry result)
                              (let loop ((cont (entry-continuations entry)))
                                (unless (null? cont)
                                  ((car cont) result)
                                  (loop (cdr cont))))))))
            (begin
              (push-continuation! entry cont)
              (let loop ((result (entry-results entry)))
                (unless (null? result)
                  (cont (car result))
                  (loop (cdr result))))))))

    (define (run)
      (do () ((not (has-next?)))
        (step)))

    (define self
      (case-lambda
        ((msg) (cond ((eq? msg 'has-next?) (has-next?))
                     ((eq? msg 'step) (step))
                     ((eq? msg 'run) (run))
		     (else
		      (error 'trampoline% "Invalid message." self msg))))
        ((msg param1
	      param2) (cond ((eq? msg 'push-stack)  (push-stack param1 param2))
			    (else
			     (error 'trampoline% "Invalid message." self msg param1 param2))))
        ((msg param1
	      param2
	      param3) (cond ((eq? msg 'push) (push param1 param2 param3))
			    (else
			     (error 'trampoline% "Invalid message." self msg param1 param2 param3))))))
    self)

  (define-syntax delay-parser
    (syntax-rules ()
      ((delay-parser parser)
       (lambda args
         (apply parser args)))))

  (define-syntax define-parser
    (syntax-rules ()
      ((define-parser parser body)
       (define parser
         (make-parser
          (delay-parser body))))))

  (define-syntax make-stream
    (syntax-rules ()
      ((make-stream body ...)
       (stream-cdr
        (stream-cons '() (begin body ...))))))

  (define (make-parser parser)
    (lambda (str . args)
      (define tramp (if (>= (length args) 1) (car args) #f))
      (define cont (if (>= (length args) 2) (cadr args) #f))
      (if (and tramp cont)
          (parser str tramp cont)
          (run-parser parser str))))

  (define (run-parser parser str)
    (let ((tramp (trampoline%))
          (results '()))
      (define (compute)
        (when (tramp 'has-next?)
          (do () ((not (and (null? results)
                            (tramp 'has-next?))))
            (tramp 'step)))
        (stream))
      (define (stream)
        (let ((result (sequence->stream results)))
          (set! results (list))
          (if (tramp 'has-next?)
              (stream-append result (make-stream (compute)))
              result)))
      (make-stream
       (parser str tramp
               (lambda (result)
                 (match result
                   [(success ,val ,rest)
                    (when (string=? rest "")
                      (set! results (cons result results)))]
                   [(failure ,str) `(failure ,str)])))
       (compute))))

  (define (memo fn)
    (let ((alist (list)))
      (lambda args
        (define result (assoc args alist))
        (if result (cdr result)
            (begin
              (let* ((result (apply fn args))
                     (entry (cons args result)))
                (set! alist (cons entry alist))
                result))))))

  (define succeed
    (memo
     (lambda (val)
       (lambda (str tramp cont)
         (cont `(success ,val ,str))))))

  (define string
    (memo
     (lambda (match)
       (lambda (str tramp cont)
         (let* ((len (min (string-length str) (string-length match)))
                (head (substring str 0 len))
                (tail (substring str len (string-length str))))
           (if (string=? head match)
               (cont `(success ,head ,tail))
               (cont `(failure ,tail))))))))

  (define (bind p fn)
    (lambda (str tramp cont)
      (p str tramp
         (lambda (result)
           (match result
             [(success ,val ,rest) ((fn val) rest tramp cont)]
             [(failure ,str) (cont `(failure ,str))])))))

  (define seq
    (memo
     (lambda parsers
       (define (seq2 a b)
         (bind a (lambda (x)
                   (bind b (lambda (y)
                             (succeed (append (list x)
                                              (if (or (pair? y)
                                                      (null? y))
                                                  y
                                                  (list y)))))))))
       (fold-right seq2 (succeed '()) parsers))))

  (define alt
    (memo
     (lambda parsers
       (lambda (str tramp cont)
         (let loop ((fn parsers))
           (unless (null? fn)
             (tramp 'push (car fn) str cont)
             (loop (cdr fn))))))))

  (define red
    (memo
     (lambda (p fn)
       (bind p (lambda (val)
                 (match val
                   [(,val ...) (succeed (apply fn val))]
                   [,_ (succeed (fn val))])))))))

;; (define-parser expr
;;   (alt (red (seq expr (string "+") term)
;;             (lambda (x _ y) (+ x y)))
;;        (red (seq expr (string "-") term)
;;             (lambda (x _ y) (- x y)))
;;        term))

;; (define-parser term
;;   (alt (red (seq term (string "*") factor)
;;             (lambda (x _ y) (* x y)))
;;        (red (seq term (string "/") factor)
;;             (lambda (x _ y) (/ x y)))
;;        factor))

;; (define-parser factor
;;   (alt (red (seq (string "(") expr (string ")"))
;;             (lambda (_ x __) x))
;;        num))

;; (define-parser digit
;;   (alt (string "0")
;;   (alt (string "1")
;;   (alt (string "2")
;;   (alt (string "3")
;;   (alt (string "4")
;;   (alt (string "5")
;;   (alt (string "6")
;;   (alt (string "7")
;;   (alt (string "8")
;;        (string "9")))))))))))

;; (define-parser num
;;   (red digit string->number))

;; (expr "1*2+3*4")
;; (expr "9-(5+2)")
