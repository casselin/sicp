#lang sicp
(#%require sicp-pict)

#|
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
|#

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

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

#|
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
|#

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; Exercise 2.45
(define (split f g)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f g) painter (- n 1))))
          (f painter (g smaller smaller))))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))

;; Exercise 2.46
(define (make-vect xcor ycor)
  (cons xcor ycor))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame2 frame)
  (car frame))
(define (edge1-frame2 frame)
  (cadr frame))
(define (edge2-frame2 frame)
  (cddr frame))

;; Exercise 2.48
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;; Exercise 2.49
; Using segment and vect of the sicp-pict library
; a
(define outline
  (let ((bl (vect 0.0 0.0))
        (br (vect 1.0 0.0))
        (tl (vect 0.0 1.0))
        (tr (vect 1.0 1.0)))
    (segments->painter
     (list (segment bl br)
           (segment tl bl)
           (segment tl tr)
           (segment br tr)))))

; b
(define cross
  (let ((bl (vect 0.0 0.0))
        (br (vect 1.0 0.0))
        (tl (vect 0.0 1.0))
        (tr (vect 1.0 1.0)))
  (segments->painter
   (list (segment bl tr)
         (segment tl br)))))

; c
(define diamond
  (let ((b (vect 0.5 0.0))
        (r (vect 1.0 0.5))
        (t (vect 0.5 1.0))
        (l (vect 0.0 0.5)))
  (segments->painter
   (list (segment b r)
         (segment r t)
         (segment t l)
         (segment l b)))))

; d
(define (make-segments-list vectors)
  (let ((remove-last (reverse (cdr (reverse vectors))))
        (remove-first (cdr vectors)))
    (map segment remove-last remove-first)))

(define wave
  (let ((upper-left (make-segments-list
                     (list (vect 0.0 0.85)
                           (vect 0.15 0.6)
                           (vect 0.3 0.65)
                           (vect 0.4 0.65)
                           (vect 0.35 0.85)
                           (vect 0.4 1.0))))
        (upper-right (make-segments-list
                      (list (vect 0.6 1.0)
                            (vect 0.65 0.85)
                            (vect 0.6 0.65)
                            (vect 0.75 0.65)
                            (vect 1.0 0.35))))
        (bottom-left (make-segments-list
                      (list (vect 0.25 0.0)
                            (vect 0.35 0.5)
                            (vect 0.3 0.6)
                            (vect 0.15 0.4)
                            (vect 0.0 0.65))))
        (bottom-middle (make-segments-list
                        (list (vect 0.6 0.0)
                              (vect 0.5 0.3)
                              (vect 0.4 0.0))))
        (bottom-right (make-segments-list
                       (list (vect 1.0 0.15)
                             (vect 0.6 0.45)
                             (vect 0.75 0.0)))))
    (segments->painter (append
                        upper-left
                        upper-right
                        bottom-left
                        bottom-middle
                        bottom-right))))

;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (vect 1.0 0.0)
                     (vect 0.0 0.0)
                     (vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (vect 1.0 1.0)
                     (vect 0.0 1.0)
                     (vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (vect 0.0 1.0)
                     (vect 0.0 0.0)
                     (vect 1.0 1.0)))

;; Exercise 2.51
; Analogous to beside
(define (below1 painter1 painter2)
  (let ((split-point (vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (vect 0.0 0.0)
                              (vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (vect 1.0 0.5)
                              (vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

; As a rotation of beside
(define (below2 painter1 painter2)
  (let ((paint-bottom (rotate270 painter1))
        (paint-top (rotate270 painter2)))
    (rotate90 (beside paint-bottom paint-top))))

;; Exercise 2.52