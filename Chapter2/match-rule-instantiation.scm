(define dsimp
  (simplifier deriv-rules))
(define (atom? x) (not (list? x)))

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)

        ;;这里主要是处理诸如+与*这种在pattern及exp中是完全一致的符号
        ;;（因此此时dict不需要更新，因为+仍然映射到+）
        ((atom? pat) (if (atom? exp)
                         (if (eq? pat exp)
                             dict
                             'failed)
                         'failed))

        ;;一般情况
        ;;注意extend-dict函数同时也起着判断pat在dict中是否已经存在
        ;;一个对应exp的作用，如果一个pat被match到两个不同的dict，那
        ;;也产生一个'failed
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))

        
        ((atom? exp) 'failed)

        ;;本程序的核心部分
        ;;注意pat和exp在此处都是列表,因此match实现了同时遍历pat和exp
        (else (match (cdr pat)
                     (cdr exp)
                     (match (car pat)
                            (car exp)
                            dict)))))


(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)

          ;;如果s在skeleton中是注明需要被eval的，那么首先用eval-exp
          ;;取出s中需要求值的表达式，用dict对其中可以替换的量进行替
          ;;换，然后再对结果求值
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))

          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (mapcar (lambda (v)
                 (lookup v dict))
               (cdr form)))))

;; garbage-in-garbage-out.
;; take in some rule and generate a procedure which
;; simplify expression according to the rules.
(define (simplifier the-rules)

  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (map simplify-exp exp)
                   exp)))
  
  (define (try-rules exp)
    ;; scan接收一组rules并返回一个exp
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern (car rules))
                             exp
                             (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                 (instantiate (skeleton (car rules))
                              dict))))))
    (scan the-rules))
  
  simplify-exp)


(define (empty-dictionary) '())

(define (extend-dict pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v) (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))
