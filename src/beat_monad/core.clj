(ns beat-monad.core
  (:use overtone.core)
  (:use clojure.algo.monads))

(defn metro-beat [m]
  (fn [beats & insts]
    (fn [beat-num]
      (fn [c]
        (let [ next-beat (+ beat-num beats)]
          (doseq [inst insts]
            (at (m beat-num) (inst)))
          (apply-at (m next-beat) c next-beat []))))))

(defn leading-beat
  "Same as metro-beat, but will schedule events lead-ms milliseconds ahead of the metronome beat"
  [m lead-ms]
  (fn [beats & insts]
    (fn [beat-num]
       (fn [c]
         (let [next-beat (+ beat-num beats)
               ahead-of  (fn [time] (- time lead-ms))]
           (doseq [inst insts]
             (at (ahead-of (m beat-num)) (inst)))
           (apply-at (ahead-of (m next-beat)) c next-beat []))))))

(defn chain-beats [beats]
  (with-monad cont-m
    (m-chain beats)))

(def metro (metronome 400))

(defn loop-metro [metro & mbeats]
  (let [start-beat (metro)]
    (doseq
        [mbeat mbeats]
      ((fn restart [beat-num]
         ((mbeat beat-num) restart))
       start-beat))))

;;; example:

(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 0.7 src env)))

(definst beep [freq 1100]
  (let [noise  (white-noise)
        noise-env (env-gen (perc 0.001 0.01))
        son (+ (* 1   (sin-osc freq))
               (* 0.4 (sin-osc (* 2 freq)))
               (* 0.4 (sin-osc (* 3 freq)))
               (* 0.4 (sin-osc (* 4 freq)))
               (* 1.2 (sin-osc (* 5 freq)))
               (* 1.5 (sin-osc (* 6 freq)))
               (* 1.2 (sin-osc (* 7 freq))))
        env (env-gen (perc 0.001 3) :action FREE)
        discord (+ (* 0.001 (sin-osc (* 3.7 freq)))
                   (* 0.001 (sin-osc (* 5.2 freq)))
                   )
        discord-env (env-gen (perc 0.001 3) :action FREE)]
    
    (+ 0.3 (* noise noise-env) (* son env) #_(* discord discord-env))))

(definst theremin [freq 220]
  (let [freq (lpf freq 2)
        ;; or use the mouse coordinates:
        ;;note (round (mouse-x 30 90 LIN 0.2) 3)
        
        ;;freq (* 440.0 (pow 2.0 (/ (- note 69.0) 12.0 )))
        ;;vol  (mouse-y 0 10 LIN 0.2)
        vol 3
        son (saw freq)
        son (lpf son (* 3 freq))]
    (* vol son)))

(def theme [:v :vi :iv :v :iii :iv :ii :i])

(def melody [:v :i :iii :i])

(def mode (atom [:minor :a3]))

(def base-beat (metro-beat metro))

(def drums
  (let [beep #(beep 330)
        weak-hat #(hat 0.5)]
    (chain-beats
     (map #(apply base-beat 1 %)
          [[     hat beep]
           [weak-hat]
           [weak-hat]
           [     hat]
           [weak-hat beep]
           [weak-hat]
           [     hat]
           [weak-hat]]))))

(defn next-freq [num]
  (midi->hz (nth (degrees->pitches theme (first @mode) (second @mode)) num)))

(def rhythm
  (chain-beats
   (for [beat-num (range 8)]
     (base-beat 2 #(beep (next-freq beat-num))))))

(defn next-choon-freq [num]
  (midi->hz (nth (degrees->pitches melody (first @mode) (second @mode)) num)))

(def leading-base-beat (leading-beat metro 20))

(def choon
  (chain-beats
   (for [beat-num (range (count melody))]
     (leading-base-beat 8 #(ctl theremin :freq (next-choon-freq beat-num))))))

(defn play-all []
  (theremin)
  (loop-metro metro drums rhythm choon))

