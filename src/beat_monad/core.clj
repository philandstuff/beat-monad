(ns beat-monad.core
  (:use overtone.core)
  (:use clojure.algo.monads))

(defn metro-beat [m beats & insts]
  (fn [beat-num]
    (fn [c]
      (let [ next-beat (+ beat-num beats)]
        (doseq [inst insts]
          (at (m beat-num) (inst)))
        (apply-at (m next-beat) c next-beat [])))))

(defn chain-beats [beats]
  (with-monad cont-m
    (m-chain beats)))

(def metro (metronome 400))

(defn loop-metro [mbeat metro]
  ((fn restart [beat-num]
     ((mbeat beat-num) restart))
   (metro)))

;;; example:

(definst hat [volume 1.0]
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* volume 0.7 src env)))

(definst beep [freq 1100]
  (let [noise  (white-noise)
        noise-env (env-gen (perc 0.001 0.01))
        son (+ (* 2   (sin-osc freq))
               (* 0.4 (sin-osc (* 2 freq)))
               (* 0.4 (sin-osc (* 3 freq)))
               (* 0.4 (sin-osc (* 4 freq)))
               (* 0.4 (sin-osc (* 5 freq))))
        env (env-gen (perc 0.001 3) :action FREE)
        discord (+ (* 0.1 (sin-osc (* 3.7 freq)))
                   (* 0.04 (sin-osc (* 5.2 freq)))
                   )
        discord-env (env-gen (perc 0.001 6) :action FREE)]
    
    (+ (* noise noise-env) (* son env) (* discord discord-env))))

(def theme [:v :vi :iv :v :iii :iv :ii :i])

(def mode (atom [:major :c3]))

(defn next-freq [num]
  (midi->hz (nth (degrees->pitches theme (first @mode) (second @mode)) num)))

(def drums (chain-beats [(metro-beat metro 1  hat #(beep 330))
                         (metro-beat metro 1 )
                         (metro-beat metro 1 )
                         (metro-beat metro 1  hat)
                         (metro-beat metro 1 #(beep 330))
                         (metro-beat metro 1 )
                         (metro-beat metro 1  hat)
                         (metro-beat metro 1 )
                         ]))

(def choon (chain-beats [(metro-beat metro 2 #(beep (next-freq 0)))
                         (metro-beat metro 2 #(beep (next-freq 1)))
                         (metro-beat metro 2 #(beep (next-freq 2)))
                         (metro-beat metro 2 #(beep (next-freq 3)))
                         (metro-beat metro 2 #(beep (next-freq 4)))
                         (metro-beat metro 2 #(beep (next-freq 5)))
                         (metro-beat metro 2 #(beep (next-freq 6)))
                         (metro-beat metro 2 #(beep (next-freq 7)))
                         ]))

(defn play-shit []
  (do
    (loop-metro drums metro)
    (loop-metro choon metro)))

