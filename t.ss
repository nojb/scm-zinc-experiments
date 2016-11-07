(define (compile-quote e env sz cont)
  (if (integer? e)
      `((const ,e) . ,cont)
      (error "Not implemented")))

(define (compile-begin e env sz cont)
  (if (null? e) cont
      (compile (car e) env sz (compile-begin (cdr e) env sz cont))))

(define (add-var id sz env)
  (cons (list id sz) env))

(define (find-var id env)
  (cadr (assoc id env)))

(define (add-pop n cont)
  `((pop ,n) . ,cont))

(define (compile-let id e1 e2 env sz cont)
  (compile e1 env sz
           `(push .
                  ,(compile e2 (add-var id (+ sz 1) env) (+ sz 1) (add-pop 1 cont)))))

(define (compile-variable id env sz cont)
  (let ((pos (find-var id env)))
    (cons `(acc ,pos) cont)))

(define (compile-primitive p args env sz cont)
  (case p
    ((identity)   (compile (car args) env sz cont))
    ((ignore)     (compile (car args) env sz `((const ()) . ,cont)))
    ((addint)     (compile (car args) env sz `(push . ,(compile (cadr args) env (+ sz 1) `(addint . ,cont)))))))

(define max-stack-used 0)

(define (compile e env sz cont)
  (if (> sz max-stack-used) (set! max-stack-used sz))
  (if (atom? e)
      (cond ((symbol? e) (compile-variable e env sz cont))
            (else       (compile-quote e env sz cont)))
      (case (car e)
        ((quote)  (compile-quote e env sz cont))
        ((if)     (compile-if (cadr e) (caddr e) (cadddr e) env sz cont))
        ((begin)  (compile-begin (cdr e) env sz cont))
        ((set!)   (compile-set! (cadr e) (caddr e) env sz cont))
        ((lambda) (compile-lambda (cadr e) (cddr e) env sz cont))
        ((let)    (compile-let (cadr e) (caddr e) (cadddr e) env sz cont))
        ((prim)   (compile-primitive (cadr e) (cddr e) env sz cont))
        (else     (compile-application (car e) (cdr e) env sz cont)))))
