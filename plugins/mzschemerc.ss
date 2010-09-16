;;; Syntax debugging
;;; -test or (-test)     returns current syntax object as an s-expression
;;; (-test foo)          set current
;;; (-test :this)        show current
;;; (-test :expand)      expand current (possibly in a context)
;;; (-test :expand-once) expand one step
;;; (-test :expand*)     expand one step repeatedly
;;; (-test :pp)          pprint current
(define-syntax -test
  (let ((v #f)
        (->datum (lambda (x) (if (syntax? x) (syntax-object->datum x)
                            x))))
    (lambda (stx)
      (syntax-case stx ()
        ((_ m)
         (let ((msg #'m))
           (let loop ((new (case (->datum msg)
                             ((:this :pp) v)
                             ((:expand) (expand v))
                             ((:expand-once :expand*) (expand-once v))
                             (else msg))))
             ((dynamic-require '(lib "pretty.ss") 'pretty-print)
                  (->datum new))
             (let ((old v))
               (set! v new)
               (when (eq? (->datum msg) ':expand*)
                 (if (equal? (->datum new) (->datum old))
                     (printf "--> ...\n")
                     (loop (expand-once v))))))
           #'(void)))
        (_ #`'#,v)))))


;;; Trace functions
(require (lib "trace.ss"))

;; trace all functions in the current namespace
(define (trace-all)
  (eval `(begin (require (prefix mz: (lib "trace.ss")))
                (mz:trace ,@(all-function-symbols)))))

;; untrace all functions in the current namespace
(define (untrace-all)
  (eval `(begin (require (prefix mz: (lib "trace.ss")))
                (mz:untrace ,@(all-function-symbols)))))

;; all-function-symbols: -> (listof symbol)
;; get a list of symbols representing all global functions
(define (all-function-symbols)
  (let ([mz:filter (dynamic-require '(lib "list.ss") 'filter)])
    (mz:filter (lambda (s)
                 (and (procedure? (namespace-variable-value s #f
                                                            (lambda () #f)))
                      (not (regexp-match "mz:" (symbol->string s)))
                      (not (regexp-match "trace-table" (symbol->string s)))))
               (namespace-mapped-symbols))))
