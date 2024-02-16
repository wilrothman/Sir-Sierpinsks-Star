;;; AUTHOR: William Rothman
;;; November 2022 - November 2022 
;;;
;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Sir Sierpinski's Star
;;;
;;; Description:
;;;   Sir Sierpinski's star, 💫
;;;   A 3D engine, you are! 💻
;;;   One day you'll go far. 😌
;;; Tokens: 3022
;;; okpy submission: https://okpy.org/cal/cs61a/fa22/proj04contest/submissions/oAYDpL
;;; imagur: https://i.imgur.com/JOWBZ5h.png

(define size 200)
(define depth 4) ; Max is 5, depth >= 6 result in a recursion depth error

(speed 10)
(define background-color (rgb 0 0 0))
(define r 1)
(define g 1)
(define b 0)
(define b-modifier 1)
(define pen-color (rgb r g b))

(define red (rgb 1 0 0))
(define orange (rgb 1 (/ 2 3) 0))
(define yellow (rgb 1 1 0))
(define green (rgb 0 1 0))
(define cyan (rgb 0 1 1))
(define blue (rgb 0 0 1))
(define magenta (rgb 1 0 1))
(define dark-purple (rgb 0.5 0 1))
(define white (rgb 1 1 1))
(define black (rgb 0 0 0))

(define screen-width (screen_width))
(define screen-height (screen_height))

(define depth-constant 
  (cond ((= depth 1) 3) 
        ((= depth 2) 12)
        ((= depth 3) 39)
        ((= depth 4) 120)
        ((= depth 5) 363)
        ((= depth 6) 1092)
  )
) ; 3, 12, 39, 120, 363, ...
; https://www.meritanswers.com/questions/find-the-next-number-3-12-39-120-363/26827-1-1

;;; Tools, taken directly from cs61a.org
; For

(define (map fn vals) 
  (if (null? vals) 
      () 
      (cons (fn (car vals)) 
            (map fn (cdr vals)))))

(define-macro (for sym vals exprs)
  `(map (lambda (,sym) ,@exprs) ,vals))





;;; ABSTRACTION BARRIER: Primitive Functions
;; Data structre for a coordinate

(define (coordinate x y z)
  (list x y z)
)

(define (neg n)
  (- 0 n)
)

(define (pos n)
  n
)

(define (deg->rad theta)
  (* theta (/ pi 180))
)

(define (rad->deg theta)
  (* theta (/ 180 pi))
)

(define (set-color r g b)
  (color (rgb r g b))
)


;;; ABSTRACTION BARRIER: Secondary Functions
;; Gets the node at the given index
;; get-node: nodes num --> coordinate
(define (get-node nodes i)
  (define (helper i nodes)
    (if (= i 0)
        (car nodes)
        (helper (- i 1) (cdr nodes)))
  )
  (helper i nodes)
)

(define (set-node nodes i new-coor)
  (if (= i 0)
      (cons new-coor (cdr nodes))
      (cons (car nodes) (set-node (cdr nodes) (- i 1) new-coor))
  )
)


;; Data structe for a line
;; line: coor coor --> line 
(define (line p1 p2)
  (list p1 p2)
)

(define (x coor)
  (car coor)
)

(define (y coor)
  (car (cdr coor))
)

(define (z coor)
  (car (cdr (cdr coor)))
)

(define (midpoint coor1 coor2)
  (let ((x1 (x coor1))
        (x2 (x coor2))
        (y1 (y coor1))
        (y2 (y coor2))
        (z1 (z coor1))
        (z2 (z coor2)))
    (coordinate
      (/ (+ x1 x2) 2)
      (/ (+ y1 y2) 2)
      (/ (+ z1 z2) 2)
    )
  )
)

;; Gets the first coordinate in a line
;; first: line --> coor
(define (first line)
  (car line)
)

;; Gets the second coordinate in a line
;; second: line --> coor
(define (second line)
  (car (cdr line))
)



;;; ABSTRACTION BARRIER

;; Draws a line from P1 to P2
;; draw-line: coor coor --> void
(define (draw-line p1 p2)
  (penup)
  (goto (x p1) (y p1))
  (pendown)
  (goto (x p2) (y p2))
  (penup)
)

(define (draw-point point)
  (penup)
  (goto (x point) (y point))
  (pendown)
  (circle 5)
  (penup)
)



; (define b (if (= b 0) 1 0))
(define (draw3D edges nodes)
  (define (helper edges nodes n b)
    (if (null? edges)
      nil
      (begin 
        (draw-line (get-node nodes (car (car edges))) (get-node nodes (car (cdr (car edges)))))
        (helper (cdr edges) nodes (+ n 1) b)
      )
    )
  )
  (helper edges nodes 0 b)
)

(define (fill-background)
  (let ((x0 (neg (/ screen-width 2)))
        (x1 (pos (/ screen-width 2)))
        (y0 (neg (/ screen-height 2)))
        (y1 (pos (/ screen-height 2))))
    (color background-color)
    (begin_fill)
    (draw-line (coordinate x0 y0 0) (coordinate x0 y1 0))
    (draw-line (coordinate x0 y1 0) (coordinate x1 y1 0))
    (draw-line (coordinate x1 y1 0) (coordinate x1 y0 0))
    (draw-line (coordinate x1 y0 0) (coordinate x0 y0 0))
    (end_fill)
  )
)

;;; ABSTRACTION BARRIER: rotation
;; Applies mathematical rules for rotating the cube by theta degrees. Returns a whole new nodes parameter
;; num --> nodes
(define (rotate-x nodes theta)
  (define (helper nodes i)
    (let ((y (y (get-node nodes i)))
          (z (z (get-node nodes i)))
          (sin-theta (sin (deg->rad theta)))
          (cos-theta (cos (deg->rad theta))))

      (define nodes (set-node nodes i (coordinate 
        (x (get-node nodes i))
        (- (* y cos-theta) (* z sin-theta))
        (+ (* z cos-theta) (* y sin-theta))
      )))

      (if (= i (- (length nodes) 1))
        nodes
        (helper nodes (+ i 1))
      )
    )
  )
  (helper nodes 0)
)

(define (rotate-y nodes theta)
  (define (helper nodes i)
    (let ((x (x (get-node nodes i)))
          (z (z (get-node nodes i)))
          (sin-theta (sin (deg->rad theta)))
          (cos-theta (cos (deg->rad theta))))

      (define nodes (set-node nodes i (coordinate 
        (+ (* x cos-theta) (* z sin-theta))
        (y (get-node nodes i))
        (- (* z cos-theta) (* x sin-theta))
      )))

      (if (= i (- (length nodes) 1))
        nodes
        (helper nodes (+ i 1))
      )
    )
  )
  (helper nodes 0)
)

(define (rotate-z nodes theta)
  (define (helper nodes i)
    (let ((x (x (get-node nodes i)))
          (y (y (get-node nodes i)))
          (sin-theta (sin (deg->rad theta)))
          (cos-theta (cos (deg->rad theta))))

      (define nodes (set-node nodes i (coordinate 
        (- (* x cos-theta) (* y sin-theta))
        (+ (* y cos-theta) (* x sin-theta))
        (z (get-node nodes i)))
      ))

      (if (= i (- (length nodes) 1))
        nodes
        (helper nodes (+ i 1))
      )
    )
  )
  (helper nodes 0)
)




;; ABSTRACTION BARRIER: Definitions
;;; BASIC VARIABLES
(define angle 0)
(define pi 3.14159)
(define s 100)


(define (new-sierpinski a b c depth)
  (if (<= depth 0)
    nil
    (begin
      (define ab (midpoint a b))
      (define bc (midpoint b c))
      (define ac (midpoint a c))
      
      (define this-triangle (list a b c))
      (define triangle-a (new-sierpinski a ab ac (- depth 1)))
      (define triangle-b (new-sierpinski ab b bc (- depth 1)))
      (define triangle-c (new-sierpinski ac bc c (- depth 1)))
      (append this-triangle triangle-a triangle-b triangle-c)
    )
  )
)

(define (calc-sir-nodes dimensions depth)
  (let ((first (car dimensions))
        (second (car (cdr dimensions)))
        (third (car (cdr (cdr dimensions)))))
    (new-sierpinski first second third depth)
  )
)

(define (calc-sir-edges depth depth-constant)
  (if (= depth depth-constant)
    nil
    (let ((first depth)
          (second (+ depth 1))
          (third (+ depth 2)))
      (define this (list (list first second) (list second third) (list first third)))
      (define next (calc-sir-edges (+ depth 3) depth-constant))
      (append this next)
    )
  )
)


;;; BASIC SETTINGS

;; TESTS
(define sir-nodes (calc-sir-nodes 
  (list 
    (coordinate 0 size 0)
    (coordinate (neg (/ (* size (sqrt 3)) 2)) (neg (/ size 2)) 0)
    (coordinate (/ (* size (sqrt 3)) 2) (neg (/ size 2)) 0) 
  )
  depth)
)

(define sir-edges (calc-sir-edges 0 depth-constant))





;;; ABSTRACTION BARRIER
(define (draw)
  (bgcolor background-color)
  (fill-background)

  ;; Starry night
  (pixelsize 5)

  ;; The background stars were randomly generated using the python function, which I wrote:
  ;; >>> from random import *
  ;; >>> def r(lower, upper, n):
  ;; ...    for i in range(n):
  ;; ...       x, y = randrange(lower, upper), randrange(lower, upper)
  ;; ...       print(f"(pixel {x} {y} white)")
  ;; >>> r(-75, 75, 201)
  (pixel -16 -2 white)
  (pixel 43 -46 white)
  (pixel -67 -56 white)
  (pixel 68 7 white)
  (pixel 69 -47 white)
  (pixel -38 22 white)
  (pixel -64 14 white)
  (pixel -69 17 white)
  (pixel -28 -2 white)
  (pixel 61 12 white)
  (pixel 47 50 white)
  (pixel -67 -42 white)
  (pixel -70 59 white)
  (pixel 28 53 white)
  (pixel 6 18 white)
  (pixel 29 70 white)
  (pixel 3 -8 white)
  (pixel -42 23 white)
  (pixel -22 19 white)
  (pixel -17 46 white)
  (pixel 12 -67 white)
  (pixel 71 -1 white)
  (pixel 66 30 white)
  (pixel -32 -30 white)
  (pixel -71 13 white)
  (pixel -74 50 white)
  (pixel -63 -11 white)
  (pixel 8 46 white)
  (pixel 32 65 white)
  (pixel -48 -13 white)
  (pixel -58 -29 white)
  (pixel 49 56 white)
  (pixel 67 64 white)
  (pixel 11 54 white)
  (pixel -27 -52 white)
  (pixel 69 -73 white)
  (pixel 39 -2 white)
  (pixel -72 69 white)
  (pixel -12 -60 white)
  (pixel -7 58 white)
  (pixel 69 61 white)
  (pixel -22 37 white)
  (pixel -37 70 white)
  (pixel -73 46 white)
  (pixel 71 -74 white)
  (pixel 26 14 white)
  (pixel -60 58 white)
  (pixel -74 -4 white)
  (pixel 30 50 white)
  (pixel 64 -12 white)
  (pixel 52 -44 white)
  (pixel -62 67 white)
  (pixel -17 -21 white)
  (pixel -75 26 white)
  (pixel 13 73 white)
  (pixel 3 -2 white)
  (pixel 29 -31 white)
  (pixel 32 23 white)
  (pixel -30 1 white)
  (pixel 65 -63 white)
  (pixel 1 -39 white)
  (pixel -40 0 white)
  (pixel -75 -3 white)
  (pixel -73 59 white)
  (pixel -7 37 white)
  (pixel 73 -31 white)
  (pixel 21 -70 white)
  (pixel 16 -6 white)
  (pixel -28 -60 white)
  (pixel 23 -2 white)
  (pixel 33 -27 white)
  (pixel -63 16 white)
  (pixel 17 28 white)
  (pixel 70 27 white)
  (pixel -33 12 white)
  (pixel 44 51 white)
  (pixel 63 -44 white)
  (pixel -36 -42 white)
  (pixel -71 -33 white)
  (pixel -70 -1 white)
  (pixel 27 -70 white)
  (pixel -2 -49 white)
  (pixel -17 -8 white)
  (pixel 54 8 white)
  (pixel 50 53 white)
  (pixel -62 -62 white)
  (pixel -53 -31 white)
  (pixel 53 5 white)
  (pixel -36 -60 white)
  (pixel 55 54 white)
  (pixel 4 46 white)
  (pixel -8 66 white)
  (pixel 60 42 white)
  (pixel 13 -23 white)
  (pixel -62 -42 white)
  (pixel -10 31 white)
  (pixel -7 -48 white)
  (pixel -24 -27 white)
  (pixel -34 60 white)
  (pixel -49 70 white)
  (pixel -50 51 white)
  (pixel -46 9 white)
  (pixel -50 19 white)
  (pixel 73 -36 white)
  (pixel 32 46 white)
  (pixel 74 -9 white)
  (pixel -48 5 white)
  (pixel -11 -55 white)
  (pixel -72 67 white)
  (pixel -32 -74 white)
  (pixel -12 58 white)
  (pixel -8 59 white)
  (pixel -61 -67 white)
  (pixel -43 -37 white)
  (pixel 56 63 white)
  (pixel 43 -57 white)
  (pixel -44 -69 white)
  (pixel -32 -29 white)
  (pixel -73 -7 white)
  (pixel -30 -37 white)
  (pixel 32 64 white)
  (pixel 2 65 white)
  (pixel -50 49 white)
  (pixel 37 -45 white)
  (pixel -71 -46 white)
  (pixel -1 -28 white)
  (pixel -74 -36 white)
  (pixel -7 -21 white)
  (pixel 24 11 white)
  (pixel -42 -6 white)
  (pixel -72 15 white)
  (pixel 26 31 white)
  (pixel -28 -64 white)
  (pixel -51 -69 white)
  (pixel 17 -6 white)
  (pixel 33 -69 white)
  (pixel 9 -4 white)
  (pixel -68 15 white)
  (pixel -10 65 white)
  (pixel -56 7 white)
  (pixel 16 -72 white)
  (pixel -60 61 white)
  (pixel -51 -66 white)
  (pixel -11 -27 white)
  (pixel 57 60 white)
  (pixel 41 -31 white)
  (pixel 5 -64 white)
  (pixel -61 -31 white)
  (pixel -26 1 white)
  (pixel 42 45 white)
  (pixel 57 60 white)
  (pixel 72 40 white)
  (pixel -60 10 white)
  (pixel -75 -33 white)
  (pixel 23 -68 white)
  (pixel 63 -39 white)
  (pixel 41 -52 white)
  (pixel 61 37 white)
  (pixel -29 -23 white)
  (pixel 22 -16 white)
  (pixel 13 52 white)
  (pixel -60 39 white)
  (pixel 12 -12 white)
  (pixel 64 -46 white)
  (pixel 1 7 white)
  (pixel 3 -37 white)
  (pixel -52 -10 white)
  (pixel -13 34 white)
  (pixel -19 48 white)
  (pixel -45 -64 white)
  (pixel 40 -40 white)
  (pixel 46 -48 white)
  (pixel 61 73 white)
  (pixel 3 -27 white)
  (pixel 7 43 white)
  (pixel -45 -14 white)
  (pixel -5 -27 white)
  (pixel -8 70 white)
  (pixel 41 16 white)
  (pixel 18 -32 white)
  (pixel 29 39 white)
  (pixel 72 -62 white)
  (pixel -4 68 white)
  (pixel -24 -40 white)
  (pixel -60 7 white)
  (pixel 61 1 white)
  (pixel -43 -46 white)
  (pixel -65 -26 white)
  (pixel 74 68 white)
  (pixel -16 -40 white)
  (pixel -41 51 white)
  (pixel -62 -49 white)
  (pixel -44 -37 white)
  (pixel 50 70 white)
  (pixel -45 58 white)
  (pixel -47 13 white)
  (pixel 70 -3 white)
  (pixel -71 -17 white)
  (pixel 18 -68 white)
  (pixel -60 -37 white)
  (pixel 10 27 white)
  ;; /Starry night

  (color magenta)
  ; 1
  (define nodes (rotate-x sir-nodes 35))
  (draw3D sir-edges nodes)

  (color red)
  ; 2
  (define nodes (rotate-z nodes 170))
  (draw3D sir-edges nodes)

  (color orange)
  ; 3
  (define nodes (rotate-x sir-nodes 125))
  (draw3D sir-edges nodes)

  (color yellow)
  ; 4
  (define nodes (rotate-z nodes 170))
  (draw3D sir-edges nodes)

  (color green)
  (define nodes (rotate-y nodes 35))
  (draw3D sir-edges nodes)

  (color cyan)
  ; 6
  (define nodes (rotate-y nodes 170))
  (draw3D sir-edges nodes)

  (color blue)
  ; 7
  (define nodes (rotate-z nodes 170))
  (draw3D sir-edges nodes)

  (color dark-purple)
  ; 8
  (define nodes (rotate-y nodes 170))
  (draw3D sir-edges nodes)

  ; Starry night

)
; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
