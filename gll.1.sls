(library (gll (1))
  (export define-parser
          succeed
          string
          bind
          seq
          alt)
  ;;; Notes! uses match in a simple way. really
  ;;; all that's here are tagged lists...
  (import (match)
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
         (delay-parser body)))))

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
       (memo
        (lambda (str)
          `(success ,val ,str))))))

  (define string
    (memo
     (lambda (match)
       (memo
        (lambda (str)
          (let* ((len (min (string-length str) (string-length match)))
                 (head (substring str 0 len))
                 (tail (substring str len (string-length str))))
            (if (equal? head match)
                `(success ,head ,tail)
                `(failure ,str))))))))

  (define (bind p fn)
    (lambda (str)
      (match (p str)
        [(success ,val ,rest) ((fn val) rest)]
        [(failure ,str) `(failure ,str)])))

  (define seq
    (memo
     (lambda (a b)
       (memo
        (bind a (lambda (x)
                  (bind b (lambda (y)
                            (succeed (list x y))))))))))

  (define alt
    (memo
     (lambda (a b)
       (memo
        (lambda (str)
          (let ((result (a str)))
            (match result
              [(success ,val ,rest) result]
              [(failure ,str) (b str)]))))))))

;; (define-parser article
;;   (alt (string "the ")
;;        (string "a ")))

;; (define-parser noun
;;   (alt (string "student ")
;;        (string "professor ")))

;; (define-parser verb
;;   (alt (string "studies ")
;;        (string "lectures ")))

;; (define-parser noun-phrase
;;   (seq article noun))

;; (define-parser verb-phrase
;;   (seq verb noun-phrase))

;; (define-parser sentence
;;   (seq noun-phrase verb-phrase))

;; (sentence "the professor lectures the student ")
;; (sentence "not a sentence ")
