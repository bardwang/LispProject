;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")) #f)))
;; Final Project
;; Nicholas Lauretano
;; Xun Wang

(require 2htdp/universe)
(require htdp/image)

;;;;;;;;;;;;;;;;;;;
;;;;; FROGGER ;;;;;
;;;;;;;;;;;;;;;;;;;

#|
*****Game Instructions*****

**To Start: 

Type the following into the interactions window:

(run start)

**Rules: 

Attempt to reach the other side of the busy road and fill all five
of the open areas.  If a vehicle hits you or you try to reach any
area besides the alcoves on the other side, you will lose a life.
If the timer reaches zero, you lose one life.  If you have no lives
left, you lose the game.

**Instructions:

Use the arrow keys to move

|#


;;;;;; CONSTANTS ;;;;;;

(define WIDTH 500)  ;; Width of the scene
(define HEIGHT 500) ;; Height of the scene
(define EMPTY (empty-scene 500 500)) ;; The empty scene
(define RADIUS 10) ;; The radius of the player
(define DIMENSION 50) ;; The main dimension of vehicles
(define PLAYER (circle RADIUS "solid" "green")) ;; The moveable player 
(define INCREMENT 40) ;; The amount a player moves
(define STARTPOSN (make-posn 250 475))
(define CAR (rectangle DIMENSION (/ DIMENSION 2) 
                       "solid" "red")) ;; Cars
(define TRUCK (rectangle (* 2 DIMENSION) (/ DIMENSION 2)
                         "solid" "blue")) ;; Trucks
(define TRAIN (rectangle (* 5 DIMENSION) (/ DIMENSION 2)
                         "solid" "black")) ;; Trains
(define SAFE (rectangle 20 20 "solid" "white")) ;; The safe zones
(define SAFEY 50) ;; The Y location of the safe zones
(define VELOCITY 5) ;; The velocity of the vehicles
(define TIMESTART 1000) ;; The start time of the timer

;;Construction of the scene
(define RECTANGLE1 (rectangle 498 38 "solid" "gray"))
(define RECTANGLE2 (rectangle 498 58 "solid" "gray"))
(define SCENE
  (place-image 
   RECTANGLE2 250 30
   (place-image
    RECTANGLE1 250 480
    EMPTY)))

;; End of Game messages
(define GAMEOVER 
  (place-image (text "GAME OVER" 25 "red")
               (/ WIDTH 4) (/ HEIGHT 2)
               SCENE))
(define YOUWIN
  (place-image (text "YOU WIN!" 25 "orange")
               (/ WIDTH 4) (/ HEIGHT 2)
               SCENE))



;;;;;; DATA DEFINITIONS ;;;;;;

;; A Player is a Posn

;; A Car is a Posn

;; A Truck is a Posn

;; A Train is a Posn

;; A Vehicle is one of:
;; -- Car
;; -- Truck
;; -- Train

;; A SafeZone is a Posn

;; A Life is a Natural

;; A Timer is a Natural

;; A World is (make-world Player [Listof Car] [Listof Truck] [Listof Train]
;; [Listof SafeZone] Life Timer)
(define-struct world (player car truck train safe life time))

;; A GameStatus is one of:
;; -- Image
;; -- World



;;;;;; EXAMPLE WORLD ;;;;;;;

(define ex-world 
  (make-world 
   (make-posn 200 470)
   (list (make-posn 300 400)
         (make-posn 100 250))
   (list (make-posn 400 150)
         (make-posn 200 300))
   (list (make-posn 200 100)
         (make-posn 400 150))
   (list (make-posn 50 50)
         (make-posn 210 50))
   3
   400))



;;;;;; CODE ;;;;;;

;; render-world : World -> Scene
;; To render the world in a scene
(define (render-world w)
  (cond
    [(empty? (world-safe w)) YOUWIN]
    [(= 0 (world-life w)) GAMEOVER]
    [else
     (place-image
      PLAYER
      (posn-x (world-player w)) (posn-y (world-player w))
      (render-vehicle 
       (world-car w) CAR 
       (render-vehicle
        (world-truck w) TRUCK
        (render-vehicle 
         (world-train w) TRAIN
         (render-vehicle
          (world-safe w) SAFE
          (place-image
           (text 
            (string-append 
             "Lives: " (number->string (world-life w))) 
            10 "black")
           10 10
           (place-image
            (text 
             (string-append
              "Time: " (number->string (world-time w))) 
              10 "black")
            (- WIDTH 70) 10
            SCENE)))))))]))

(check-expect (render-world ex-world) 
              (place-image
               PLAYER
               200 470
               (render-vehicle 
                (list (make-posn 300 400) (make-posn 100 250)) CAR 
                (render-vehicle
                 (list (make-posn 400 150) (make-posn 200 300)) TRUCK
                 (render-vehicle 
                  (list (make-posn 200 100) (make-posn 400 150)) TRAIN
                  (render-vehicle
                   (list (make-posn 50 50) (make-posn 210 50)) SAFE
                   (place-image
                    (text "Lives: 3" 10 "black")
                    10 10
                    (place-image
                     (text "Time: 400" 10 "black")
                     (- WIDTH 70) 10
                     SCENE))))))))
(check-expect (render-world
               (make-world
                STARTPOSN empty empty empty empty 3 1000))
               YOUWIN)
(check-expect (render-world
               (make-world
                STARTPOSN empty empty empty (list (make-posn 400 500)) 0 1000))
               GAMEOVER)

;; render-vehicle : [Listof Vehicle] Image Scene -> Scene
;; To render a vehicle at a posn
(define (render-vehicle v img scene)
  (local [(define (render posn sce) 
            (place-image img (posn-x posn) (posn-y posn) sce))]
    (foldr render scene v)))

(check-expect (render-vehicle empty CAR SCENE) SCENE)
(check-expect (render-vehicle
               (list (make-posn 300 400)
                     (make-posn 100 200))
               TRUCK SCENE)
              (place-image 
               TRUCK 300 400 
               (place-image
                TRUCK 100 200 SCENE)))

;; handle-key : World KeyEvent -> World
;; To update the world on a key event
(define (handle-key w ke)
  (cond
    [(string=? ke "up")    (key-helper "vert" w (- 0 INCREMENT))]
    [(string=? ke "down")  (if (= (posn-y STARTPOSN) 
                                  (posn-y (world-player w)))
                               w (key-helper "vert" w INCREMENT))]
    [(string=? ke "right") (if (>= 480 (posn-x (world-player w)))
                               (key-helper "horz" w (/ INCREMENT 2))
                               w)]
    [(string=? ke "left")  (if (<= 20 (posn-x (world-player w)))
                               (key-helper "horz" w (/ (- 0 INCREMENT) 2))
                               w)]
    [else w]))

;; key-helper : String World Natural -> World
;; To update the player's position
(define (key-helper str w nat)
  (cond
    [(string=? str "vert")
     (make-world
      (make-posn (posn-x (world-player w))
                 (+ nat (posn-y (world-player w))))
      (world-car w)
      (world-truck w)
      (world-train w)
      (world-safe w)
      (world-life w)
      (world-time w))]
    [(string=? str "horz")
     (make-world
      (make-posn (+ nat (posn-x (world-player w)))
                 (posn-y (world-player w)))
      (world-car w)
      (world-truck w)
      (world-train w)
      (world-safe w)
      (world-life w)
      (world-time w))]))

(check-expect (handle-key ex-world "up")
              (make-world 
   (make-posn 200 430)
   (list (make-posn 300 400) (make-posn 100 250))
   (list (make-posn 400 150) (make-posn 200 300))
   (list (make-posn 200 100) (make-posn 400 150))
   (list (make-posn 50 50) (make-posn 210 50))
   3
   400))
(check-expect (handle-key ex-world "down")
              (make-world 
   (make-posn 200 510)
   (list (make-posn 300 400) (make-posn 100 250))
   (list (make-posn 400 150) (make-posn 200 300))
   (list (make-posn 200 100) (make-posn 400 150))
   (list (make-posn 50 50) (make-posn 210 50))
   3
   400))
(check-expect (handle-key ex-world "right")
              (make-world 
   (make-posn 220 470)
   (list (make-posn 300 400) (make-posn 100 250))
   (list (make-posn 400 150) (make-posn 200 300))
   (list (make-posn 200 100) (make-posn 400 150))
   (list (make-posn 50 50) (make-posn 210 50))
   3
   400))
(check-expect (handle-key ex-world "left")
              (make-world 
   (make-posn 180 470)
   (list (make-posn 300 400) (make-posn 100 250))
   (list (make-posn 400 150) (make-posn 200 300))
   (list (make-posn 200 100) (make-posn 400 150))
   (list (make-posn 50 50) (make-posn 210 50))
   3
   400))
(check-expect (handle-key 
               (make-world (make-posn 10 100) 
                           empty empty empty empty 3 100) "left")
              (make-world (make-posn 10 100) empty empty empty empty 3 100))
(check-expect (handle-key 
               (make-world (make-posn 490 100) 
                           empty empty empty empty 3 100) "right")
              (make-world (make-posn 490 100) empty empty empty empty 3 100))
(check-expect (handle-key 
               (make-world STARTPOSN empty empty empty empty 3 100) "down")
              (make-world STARTPOSN empty empty empty empty 3 100))
(check-expect (handle-key ex-world "t") ex-world)

;; update-world : World -> World
;; To update the world on clock ticks
(define (update-world w)
  (cond
    [(or (collision? w)
         (miss-safe? (world-player w) (world-safe w))
         (= (world-time w) 0))
     (make-world
      STARTPOSN
      (world-car w)
      (world-truck w)
      (world-train w)
      (world-safe w)
      (- (world-life w) 1)
      TIMESTART)]
    [(safe? (world-player w) (world-safe w))
     (make-world
      STARTPOSN
      (world-car w)
      (world-truck w)
      (world-train w)
      (remove-safe (world-player w) (world-safe w) )
      (world-life w)
      TIMESTART)]
    [else (make-world
      (world-player w)
      (update-vehicles (world-car w) (* 1.5 VELOCITY))
      (update-vehicles (world-truck w) (- 0 VELOCITY))
      (update-vehicles (world-train w) (* 3 VELOCITY))
      (world-safe w)
      (world-life w)
      (sub1 (world-time w)))]))

(check-expect (update-world 
               (make-world 
                STARTPOSN 
                (list (make-posn 300 400) (make-posn 100 250))
                (list (make-posn 400 150) (make-posn 200 300))
                (list (make-posn 200 100) (make-posn 400 150))          
                (list (make-posn 50 50) (make-posn 210 50))
                3
                0))
              (make-world 
                STARTPOSN 
                (list (make-posn 300 400) (make-posn 100 250))
                (list (make-posn 400 150) (make-posn 200 300))
                (list (make-posn 200 100) (make-posn 400 150))          
                (list (make-posn 50 50) (make-posn 210 50))
                2
                TIMESTART))
(check-expect (update-world 
               (make-world 
                (make-posn 50 50) 
                (list (make-posn 300 400) (make-posn 100 250))
                (list (make-posn 400 150) (make-posn 200 300))
                (list (make-posn 200 100) (make-posn 400 150))          
                (list (make-posn 50 50) (make-posn 210 50))
                3
                400))
              (make-world 
                STARTPOSN
                (list (make-posn 300 400) (make-posn 100 250))
                (list (make-posn 400 150) (make-posn 200 300))
                (list (make-posn 200 100) (make-posn 400 150))          
                (list (make-posn 210 50))
                3
                TIMESTART))
(check-expect (update-world ex-world)
              (make-world 
               (make-posn 200 470)
               (list (make-posn 307.5 400) (make-posn 107.5 250))
               (list (make-posn 395 150) (make-posn 195 300))
               (list (make-posn 215 100) (make-posn 415 150))
               (list (make-posn 50 50) (make-posn 210 50))
               3
               399))

;; collision? : World -> Boolean
;; Is there a collision
(define (collision? w)
  (or (collision-helper (world-player w) (world-car w) CAR)
      (collision-helper (world-player w) (world-truck w) TRUCK)
      (collision-helper (world-player w) (world-train w) TRAIN)))

;; collision-helper : Posn [ListOf X] Image -> Boolean
;; To help determines if there is a collision
(define (collision-helper posn lox img)
  (local [(define (hit? item)
            (and (< (abs (- (posn-y posn) (posn-y item))) (image-height img))
                 (< (abs (- (posn-x posn) (posn-x item))) 
                    (+ RADIUS (* 0.5 (image-width img))))))]
  (ormap hit? lox)))

(check-expect (collision?
               (make-world
                (make-posn 200 300)
                (list (make-posn 205 300))
                empty
                empty
                empty
                3 400))
              true)
(check-expect (collision?
               (make-world
                (make-posn 200 300)
                empty
                (list (make-posn 205 300))
                empty
                empty
                3 400))
              true)
(check-expect (collision?
               (make-world
                (make-posn 200 300)
                empty
                empty
                (list (make-posn 205 300))
                empty
                3 400))
              true)
(check-expect (collision? ex-world) false)

;; safe? : Posn [Listof Safe] -> Boolean
;; Is the player in a safe area
(define (safe? posn los)
  (collision-helper posn los SAFE))

(check-expect (safe? (make-posn 250 50) (list (make-posn 250 50)))
              true)
(check-expect (safe? (make-posn 250 50) (list (make-posn 100 50)))
              false)

;; remove-safe : Posn [Listof Safe] -> [Listof Safe]
;; To remove the safe area if it has become occupied
(define (remove-safe posn los)
  (cond
    [(empty? los) empty]
    [else
     (if (and (< (abs (- (posn-y posn) (posn-y (first los)))) 
                 (image-height SAFE))
              (< (abs (- (posn-x posn) (posn-x (first los)))) 
                 (image-width SAFE)))
         (remove-safe posn (rest los))
         (cons (first los) (remove-safe posn (rest los))))]))

(check-expect (remove-safe (make-posn 250 50) (list (make-posn 250 50)))
              empty)
(check-expect (remove-safe (make-posn 50 250) (list (make-posn 250 50)))
              (list (make-posn 250 50)))

;; miss-safe? : Posn [listof Safe] -> Boolean
;; Did the player miss the safe area
(define (miss-safe? posn los)
  (and (>= (+ 10 SAFEY) (posn-y posn))
       (not (safe? posn los))))

(check-expect (miss-safe? (make-posn 250 50) (list (make-posn 250 50)))
              false)
(check-expect (miss-safe? (make-posn 250 100) (list (make-posn 250 50)))
              false)
(check-expect (miss-safe? (make-posn 200 50) (list (make-posn 250 50)))
              true) 


;; update-vehicles : [Listof vehicles] Natural -> [Listof vehicles]
;; To update the vehicles' positions
(define (update-vehicles lov vel)
  (cond
    [(empty? lov) empty]
    [(< vel 0)
     (if (<= (posn-x (first lov)) -200)
         (cons (make-posn (+ WIDTH 200) (posn-y (first lov))) 
               (update-vehicles (rest lov) vel))
         (cons (make-posn (+ vel (posn-x (first lov))) (posn-y (first lov)))
               (update-vehicles (rest lov) vel)))]
    [(> vel 0)
     (if (>= (posn-x (first lov)) (+ WIDTH 200))
         (cons (make-posn -200 (posn-y (first lov))) 
               (update-vehicles (rest lov) vel))
         (cons (make-posn (+ vel (posn-x (first lov))) (posn-y (first lov)))
               (update-vehicles (rest lov) vel)))]))

(check-expect (update-vehicles empty 6) empty)
(check-expect (update-vehicles 
               (list (make-posn 800 20) (make-posn 150 40)) 6)
              (list (make-posn -200 20) (make-posn 156 40)))
(check-expect (update-vehicles 
               (list (make-posn -300 20) (make-posn 150 40)) -6)
              (list (make-posn 700 20) (make-posn 144 40)))

;; game-over? : World -> Boolean
;; Is the game over?
(define (game-over? w)
  (or (empty? (world-safe w))
      (= 0 (world-life w))))

(check-expect (game-over? ex-world) false)
(check-expect (game-over?
               (make-world
                (make-posn 200 475)
                empty
                empty
                empty
                empty
                2
                200))
              true)
(check-expect (game-over?
               (make-world
                (make-posn 200 475)
                empty
                empty
                empty
                (list (make-posn 40 50))
                0
                200))
              true)
  
;; run : World -> Scene
;; To start the game
(define (run w)
  (big-bang w
            (on-draw render-world)
            (on-key handle-key)
            (on-tick update-world)
            (stop-when game-over?)))



;;;;;; THE STARTING WORLD ;;;;;;

(define start
  (make-world
   STARTPOSN
   (list (make-posn 100 (- (posn-y STARTPOSN) 40))
         (make-posn 350 (- (posn-y STARTPOSN) 40))
         (make-posn 600 (- (posn-y STARTPOSN) 40))
         (make-posn -150 (- (posn-y STARTPOSN) 40))
         (make-posn 50 (- (posn-y STARTPOSN) 120))
         (make-posn 300 (- (posn-y STARTPOSN) 120))
         (make-posn 550 (- (posn-y STARTPOSN) 120))
         (make-posn -100 (- (posn-y STARTPOSN) 120))
         (make-posn 0 (- (posn-y STARTPOSN) 360))
         (make-posn 250 (- (posn-y STARTPOSN) 360))
         (make-posn 500 (- (posn-y STARTPOSN) 360))
         (make-posn -100 (- (posn-y STARTPOSN) 360)))
   (list (make-posn 100 (- (posn-y STARTPOSN) 80))
         (make-posn 400 (- (posn-y STARTPOSN) 80))
         (make-posn -200 (- (posn-y STARTPOSN) 80))
         (make-posn 200 (- (posn-y STARTPOSN) 160))
         (make-posn 500 (- (posn-y STARTPOSN) 160))
         (make-posn -100 (- (posn-y STARTPOSN) 160))
         (make-posn 600 (- (posn-y STARTPOSN) 320))
         (make-posn 0 (- (posn-y STARTPOSN) 320))
         (make-posn 300 (- (posn-y STARTPOSN) 320))
         (make-posn 350 (- (posn-y STARTPOSN) 400))
         (make-posn 650 (- (posn-y STARTPOSN) 400))
         (make-posn 150 (- (posn-y STARTPOSN) 400)))  
   (list (make-posn 100 (- (posn-y STARTPOSN) 200))
         (make-posn 600 (- (posn-y STARTPOSN) 280)))
   (list (make-posn 50 SAFEY)
         (make-posn 150 SAFEY)
         (make-posn 250 SAFEY)
         (make-posn 350 SAFEY)
         (make-posn 450 SAFEY))
   3
   TIMESTART))
  
