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
    
    (+ (* noise noise-env) (* son env) #_(* discord discord-env))))

(def theme [:v :vi :iv :v :iii :iv :ii :i])

(def mode (atom [:major :c3]))

(defn next-freq [num]
  (midi->hz (nth (degrees->pitches theme (first @mode) (second @mode)) num)))

(def base-beat (metro-beat metro))

(def drums
  (chain-beats [(base-beat 3 hat #(beep 330))
                                        ;
                                        ;
                (base-beat 1 hat)
                (base-beat 2 #(beep 330))
                                        ;
                (base-beat 2 hat)
                                        ;
                ]))

(def choon
  (chain-beats
   (for [beat-num (range 8)]
     (base-beat 2 #(beep (next-freq beat-num))))))

(defn play-all []
  (loop-metro metro drums choon))

