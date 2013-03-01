(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;; ex 2.44 ========================================
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;; =================================================

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity painter)
  painter)

;; ex 2.45 ======================================
(define (split cut1 cut2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split cut1 cut2) painter (- n 1))))
          (cut1 painter (cut2 painter painter))))))
;;  ======================================

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; ex 2.46  ======================================
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
                (xcor-vect vect2))
             (+ (ycor-vect vect1)
                (ycor-vect vect2))))
(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1)
                (xcor-vect vect2))
             (- (ycor-vect vect1)
                (ycor-vect vect2))))
(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))


;; ex 2.47   ======================================
;; type 1 of make-frame and corresponding functions
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;; type 2 of make-frame and corresponding functions 
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cddr frame))

;;   ======================================

(define (for-each f l)
  (if (null? l)
      t
      (begin (f (car l))
             (for-each f (cdr l)))))

(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       ;; attention! the procedure 'draw-line' hasn't been defined
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; ex2.48   ======================================
(define (make-segment vect1 vect2)
  (cons vect1 vect2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;; ex2.49   ======================================
(define (draw-edges frame)
  ((segment->painter
    (list (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 0 0) (make-vect 1 0))
          (make-segment (make-vect 0 1) (make-vect 1 1))
          (make-segment (make-vect 1 0) (make-vect 1 1))))
   frame))
(define (draw-diagonals frame)
  ((segment->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 1 0) (make-vect 0 1))))
   frame))
(define (draw-diamond frame)
  ((segment->painter
    (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
          (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
          (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
          (make-segment (make-vect 0.5 1) (make-vect 1 0.5))))
   frame))
(define (wave frame)
  )

;;    ======================================

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; ex 2.50     ======================================
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; ex 2.51     ======================================
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 1.0)))))))
(define (below-another-version painter1 painter2)
  (rotate90
   (beside (rotate270 painter1)
           (rotate270 painter2))))
