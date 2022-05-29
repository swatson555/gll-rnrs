(library (gll (2))
  (export define-parser
          succeed
          string
          bind
          seq
          alt)
  (import (match)
          (only (scheme) set-car! set-cdr!)
          (rename (rnrs)
                  (string rnrs-string)))

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

  (define (make-parser parser)
    (lambda (str . args)
      (define cont (if (>= (length args) 1) (car args) #f))
      (if cont
          (parser str cont)
          (run-parser parser str))))

  (define (run-parser parser str)
    (let ((results '()))
      (parser str (lambda (result)
                    (match result
                      [(success ,val ,rest)
                       (when (string=? rest "")
                         (set! results (cons result results)))]
                      [(failure ,str) `(failure ,str)])))
      results))

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

  (define (memo-cps fn)
    (let ((table (list)))
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
      (define (table-ref str)
        (define entry (assoc str table))
        (if entry (cdr entry)
            (begin
              (let ((entry (make-entry)))
                (set! table (cons (cons str entry) table))
                entry))))
      (lambda (str cont)
        (let ((entry (table-ref str)))
          (if (empty? entry)
              (begin
                ;; first time memoized procedure has been called with str
                (push-continuation! entry cont)
                (fn str (lambda (result)
                          (unless (result-subsumed? entry result)
                            (push-result! entry result)
                            (let loop ((cont (entry-continuations entry)))
                              (unless (null? cont)
                                ((car cont) result)
                                (loop (cdr cont))))))))
              (begin
                ;; memoized procedure has been called with str before
                (push-continuation! entry cont)
                (let loop ((result (entry-results entry)))
                  (unless (null? result)
                    (cont (car result))
                    (loop (cdr result))))))))))

  (define succeed
    (memo
     (lambda (val)
       (memo-cps
        (lambda (str cont)
          (cont `(success ,val ,str)))))))

  (define string
    (memo
     (lambda (match)
       (memo-cps
        (lambda (str cont)
          (let* ((len (min (string-length str) (string-length match)))
                 (head (substring str 0 len))
                 (tail (substring str len (string-length str))))
            (if (string=? head match)
                (cont `(success ,head ,tail))
                (cont `(failure ,tail)))))))))

  (define (bind p fn)
    (lambda (str cont)
      (p str (lambda (result)
               (match result
                 [(success ,val ,rest) ((fn val) rest cont)]
                 [(failure ,str) (cont `(failure ,str))])))))

  (define seq
    (memo
     (lambda (a b)
       (memo-cps
        (bind a (lambda (x)
                  (bind b (lambda (y)
                            (succeed (list x y))))))))))

  (define alt
    (memo
     (lambda (a b)
       (memo-cps
        (lambda (str cont)
          (a str cont)
          (b str cont)))))))

;; (define-parser s
;;   (alt (seq s (string "a"))
;;        (string "a")))

;; (s "aaa")
