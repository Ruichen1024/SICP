;; this is the code for problem set -- Lunar Lander

(#%require (only racket/base random))

(define (show-ship-state ship-state)
  (write-line 
    (list 'height (height ship-state)
          'velocity (velocity ship-state)
          'fuel (fuel ship-state))))

(define (landed? ship-state)
  (<= (height ship-state) 0))

(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
       (write-line final-velocity)
       (cond ((>= final-velocity safe-velocity)
               (write-line "good landing")
               'game-over)
             (else
               (write-line "you crashed!")
               'game-over))))

(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))



(define (initial-ship-state)
  (make-ship-state 50       ; 50 km high
                   0      ; not moving (0 km/sec)
                   20))     ; 20 kg of fuel left

(define dt 1)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define (player-input) 
  (char->integer (prompt-for-command-char " action: "))) 

(define burn-key 32)   ;space key

; You'll learn about the stuff below here in Chapter 2.
; For now, think of make-ship-state, height, velocity, and fuel
; as primitive procedures built in to Scheme.

(define (make-ship-state height velocity fuel)
  (list 'HEIGHT   height
        'VELOCITY velocity
        'FUEL     fuel))

(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))

(define (second l) (cadr l))
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))

; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...

; for input and output

(define (write-line x)
  (display x)
  (newline))

(define (get-one-key)
  (let ((x (read-char)))
    (if (eq? x #\newline)
        x
        (empty-buffer x))))

(define (empty-buffer x)
  (if (eq? (read-char) #\newline)
      x
      (empty-buffer x)))

(define (prompt-for-command-char prompt)
  (display prompt)
  (get-one-key)) 

; for random number generation

(#%require (only racket/base random))

; a ridiculous addendum  (you'll need this for the exercises)

(define (1+ x) (+ 1 x))

;My answer:
;1
(define (update-1 ship-state fuel-burn-rate)
  (define fuel-rate
    (if (< (fuel ship-state) fuel-burn-rate)
        (/ (fuel ship-state) dt)
        fuel-burn-rate
        ))
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) 
   (+ (velocity ship-state)
      (* (- (* engine-strength fuel-rate) gravity)
         dt))                                           
   (- (fuel ship-state) (* fuel-rate dt))))       

(define (full-burn ship-state) 1)
(define (no-burn ship-state) 0)

(define (ask-user ship-state) (get-burn-rate))

;2

(define (lander-loop ship-state strategy )
  (show-ship-state ship-state)
  (if (landed? ship-state)
      (end-game ship-state)
      (lander-loop (update ship-state (strategy ship-state)) strategy)))

(define (play strategy)
  (lander-loop (initial-ship-state) strategy))



;3
(define (random-choice f g)
  (lambda (ship-state)
    (if (= (random 2) 0)
        (f ship-state)
        (g ship-state))))
;4
(define (height-choice f g height)
    (if (>= height 30)
        (f ship-state)
        (g ship-state)))

;5
(define (random-choice strategy-1 strategy-2)
  (choice strategy-1
          strategy-2
          (lambda (ship-state) (= (random 2) 0))))

(define (choice strategy1 strategy2 boolean-func)
    (lambda (ship-state)
      (if (boolean-func ship-state)
          (strategy1 ship-state)
          (strategy2 ship-state))))

(define (height-choice strategy1 strategy2 standard)
    (choice strategy1
            strategy2
            (lambda (ship-state) (>= (height ship-state) standard))))

;6
(define (compound-choice ship-state)
    (if (> (height ship-state) 40)
        (no-burn ship-state)
        (random-choice full-burn no-burn)))

;7
;On PDF

;8
(define (square x)
  (* x x))

(define (constant-acc ship-state)
    (if (= (velocity ship-state) 0)
        0
        (+ gravity (/ (/ (square (velocity ship-state)) 
                       (* 2 (height ship-state))) dt))))
;9
;The fuel of height 20 is indeed less than height of 30
;by testing (play (height-choice no-burn constant-acc 30)) and (play (height-choice no-burn constant-acc 20))

;10
(define (update ship-state fuel-burn-rate)
  (define fuel-rate
    (cond ((< 1 fuel-burn-rate)
           1)
          ((< (/ fuel-burn-rate dt) fuel-burn-rate)
           (/ fuel-burn-rate dt))
          (fuel-burn-rate)))
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt))
   (+ (velocity ship-state)
      (* (- (* engine-strength fuel-rate) gravity)
         dt))                                         
   (- (fuel ship-state) (* fuel-rate dt)))) 

;11
(define optimal-constant-acc
  (lambda (ship-state)
    (if (< (constant-acc ship-state) 0.9)
        (no-burn ship-state)
        (constant-acc ship-state))))



