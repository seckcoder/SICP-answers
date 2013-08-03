;;; 47-semaphore-using-test-and-set.scm

(define (make-semaphore n)
  (let ((cell (list #f)))
    (define (acquire)
      (if (test-and-set! cell)
        (acquire)
        (begin
          (cond ((> n 0)
                 (set! n (- n 1))
                 (clear! cell)
                 #t)
                (else
                  (clear! cell)
                  (acquire))))))
    (define (release)
      (if (test-and-set! cell)
        (begin
          (clear! cell)
          (release))
        (begin
          (set! n (+ n 1))
          (clear! cell))))
    (define (self msg)
      (cond ((eq? msg 'acquire) (acquire))
            ((eq? msg 'release) (release))
            (else
              (error 'semaphore-self "UNKNOWN MESSAGE" msg))))
    self))

; a not practical implementation of test-and-set!
(define (test-and-set! cell)
  (if (car cell)
    #t
    (begin (set-car! cell #t)
           #f)))

(define (clear! cell)
  (set-car! cell #f))

#|
; 根据注释 174
; 以下是一个可以在采用时间片模型的单处理器的 MIT Scheme 里实际运行的 test-and-set!

(define (test-and-set! n)
  (without-interrupts
    (lambda ()
      (if (= n 0)
        #t
        (begin (set! n (- n 1))
               #f)))))

|#
