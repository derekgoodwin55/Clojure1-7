(ns circles.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;;; Circles Game Basic Setup
;;; CSCI 2041 Homework #7
;;; Derek Goodwin
;;; 3862569 goodw171@umn.edu
;;; Spring 2016

;;; Constants
(def speed 5)                          ;maximum speed circles move

;---------------------------------------------------------------------
; Setup
;---------------------------------------------------------------------

(defn make-circle 
  "Creates a circle with a random color and set speed and heading."
   [x y]
  (let [angle (rand q/TWO-PI)          ;random angle
        cur-speed (+ (rand speed) 1)]  ;random speed up to our constant
       {:x x                           ;set this circle's x
    	:y y                           ;set this circle's y
        :size (+ 10 (rand 15))         ;set random diameter 
    	:color (rand 255)              ;make this colorful      
    	:speed cur-speed               ;set this circle's speed
    	:heading angle}                ;set this circle's heading
    ))                                 ;returns circle

(defn setup 
  "Set up a sketch and return initial state."
  []
  (q/frame-rate 30)                    ;frequency update and draw functions
  (q/color-mode :hsb)                  ;how we represent colors
  (let [size (q/width)
        n 20
        bg 250]
       (q/background bg)               ;nice light grey color for the bg
       ;; need to make n circles of random sizes
       ;; here we make 20 circles in a list
       {:circles (list (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))) 
        :running? true                 ;so we can pause and unpause in update
        :n n                           ;how many circles
        :size size                     ;how big is the sketch
        :bg bg                         ;we might want to change this later
        :rectangle-x 250
        :rectangle-y 250
        :score 0
        }))
    
;---------------------------------------------------------------------
; Update functions
;---------------------------------------------------------------------

(defn bounce-back [c size]
  (let [angle (mod (:heading c) q/TWO-PI)]
    
  (cond
  (or (and (>= (+ (:x c) (/ (:size c) 2.0)) 500) (or (< angle (* 0.5 q/PI)) (> angle (* 1.5 q/PI))))
      (and (<= (- (:x c) (/ (:size c) 2.0)) 0) (> angle (* 0.5 q/PI)) (< angle (* 1.5 q/PI))))
    (- q/PI angle)
  (or (and (>= (+ (:y c) (/ (:size c) 2.0)) 500) (or (< angle (* 1 q/PI)) (> angle (* 2 q/PI))))
      (and (<= (- (:y c) (/ (:size c) 2.0)) 0) (> angle (* 1 q/PI)) (< angle (* 2 q/PI))))
    (- (* 2 q/PI) angle)
  :else
 angle)))

(defn move-circle [c state]
  "Moves a circle according to its speed and heading"
  (merge c {:x (+ (:x c) (* (q/cos (:heading c)) (:speed c)))
            :y (+ (:y c) (* (q/sin (:heading c)) (:speed c)))
            :heading (bounce-back c (:size state))}
         ))

(defn update-circles 
  "Moves each circle and returns updated vector of circles."
  [circles state]
  (map (fn [c] (move-circle c state)) circles))

(defn update-state 
  "Updates sketch state. If it is paused, then the state is returned unmodified."
  [state]
  (if (:running? state)
      ;add some movement and update functions so the next line moves circles
      (assoc state :circles (update-circles (:circles state) state))
      state))

;---------------------------------------------------------------------
; Draw functions
;---------------------------------------------------------------------

(defn draw-circle 
  "Draws an individual circle with correct color, location, and size."
  [c] 
  (q/fill (:color c) 255 255)
  (q/ellipse (:x c) (:y c) (:size c) (:size c)))

(defn draw-state 
  "Draws the sketch state."
  [state]
  (q/background (:bg state))                    ;update the background
  (q/stroke 1)                                  ;how wide should the lines be
  (dorun (map draw-circle (:circles state)))    ;map is lazy
  (q/fill 0)
  (q/text "Score:" 5 15)
  (q/text-num 0 43 15)
  (q/rect (:rectangle-x state) (:rectangle-y state) 10 10 )
  )

;---------------------------------------------------------------------
; User interaction functions
;---------------------------------------------------------------------

(defn mouse-clicked 
  "Changes background color to different shades of grey."
  [state event]
  (update-in state [:bg] (fn [n] (rand-int 255))))

(defn key-pressed 
  "Process key event.  p will pause/unpause everything."
  [state event]
  (condp = (:key event)
    :p (update-in state [:running?] not)
    :up (update-in state [:rectangle-y] (fn [x] (- x 10)))
    :down (update-in state [:rectangle-y] (fn [x] (+ x 10)))
    :left (update-in state [:rectangle-x] (fn [x] (- x 10)))
    :right (update-in state [:rectangle-x] (fn [x] (+ x 10)))
    state))

(q/defsketch circles
    :host "host"
    :size [500 500]                ;we need a square canvas
    :setup setup                   ;getting things started, setting initial state
    :update update-state           ;the function to update the state
    :draw draw-state               ;the necessary draw function
    :mouse-clicked mouse-clicked   ;this is our mouse click event
    :key-pressed key-pressed       ;this is our keyboard input event
    :middleware [m/fun-mode])      ;this gives us the ability to have state
