(ns beat-monad.core
  (:use overtone.core)
  (:use clojure.algo.monads))

(defn beat [dur & insts]
  (fn [time]
    (fn [c]
      (doseq [inst insts]
        (at time (inst)))
      (apply-at (+ time dur) c (+ time dur) []))))

(defn chain-beats [beats]
  (with-monad cont-m
    (m-chain beats)))

(defn play-beat [beat]
  ((beat (now)) identity))

(defn loop-beat [beat]
  ((fn restart [time]
     ((beat time) restart))
   (now)))

;;; using metronome instead of explicit durations?

(defn metro-beat [m beats & insts]
  (fn [beat-num]
    (fn [c]
      (let [ next-beat (+ beat-num beats)]
        (doseq [inst insts]
          (at (m beat-num) (inst)))
        (apply-at (m next-beat) c next-beat [])))))

(def metro (metronome 400))

(def metro-beats (chain-beats [(metro-beat metro 3 hat)
                               (metro-beat metro 3 hat)
                               (metro-beat metro 2 beep)]))

(defn loop-metro [mbeat metro]
  ((fn restart [beat-num]
     ((mbeat beat-num) restart))
   (metro)))

(defn endless-metro-beats [beat-number]
  ((metro-beats beat-number) endless-metro-beats))

(endless-metro-beats (metro))

(stop)

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

()

(defn flam [dur]
  (let [flam-dur 20
        rest-dur (- dur flam-dur)]
    (chain-beats [(beat flam-dur #(hat 0.5))
                  (beat rest-dur hat)])))

(def notes (atom (map note [:c3 :c3 :a3 :a3 :c3 :c3 :g3 :g3])))

(defn next-freq [num]
  (midi->hz ((vec @notes) num)))

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

(defn modulate [semis]
  (swap! notes (fn [x] (map #(+ semis %) x))))

(def modulator (chain-beats [(metro-beat metro 8 #(modulate 0))
 
                             #_(metro-beat metro 8 #(modulate 5))
                             #_(metro-beat metro 8 #(modulate -4))
                             #_(metro-beat metro 8 #(modulate -2))]))

(play-beat beats)

(defn play-shit []
  (do
    (loop-metro drums metro)
    (loop-metro choon metro)
    (loop-metro modulator metro)))

(play-beat (chain-beats [(beat 300 hat)
                         (flam 300)
                         (beat 300 hat)
                         (flam 300)
                         (beat 300 hat)]))

(stop)

(defn beats2 [t0]
  (domonad cont-m
           [_  ((beat   0 hat) t0)
            t1 ((beat 600 beep) t0)
            t2 ((beat 200 hat) t1)
            t3 ((beat 400 beep) t2)
            t4 ((beat 400 hat) t3)]
           t4))

(play-beat beats2)

;;((beats (now)) identity)

(defn endless-beats [time]
  ((beats time) endless-beats))

;;(endless-beats (now))


;;; metronomes in a time-passing rather than beat-passing style

;;; first, a modification to the existing metronome to allow getting
;;; beat number for a time

(defn bmetronome
  "A metronome is a beat management function.  Tell it what BPM you want,
  and it will output beat timestamps accordingly.  Call the returned function
  with no arguments to get the next beat number, or pass it a beat number
  to get the timestamp to play a note at that beat.

  (def m (metronome 128))
  (m :time (now))  ; => <current beat number>
  (m 200)      ; => <timestamp of beat 200>
  (m :bpm 140) ; => set bpm to 140"
  [bpm]
  (let [start   (atom (now))
        tick-ms (atom (beat-ms 1 bpm))
        beat-of (fn [time] (inc (long (/ (- time @start) @tick-ms))))
        cmds    {
                 :bpm (fn [bpm]
                        (let [tms (beat-ms 1 bpm)
                              cur-beat (long (/ (- (now) @start) @tick-ms))
                              new-start (- (now) (* tms cur-beat))]
                          (reset! tick-ms tms)
                          (reset! start new-start))
                        [:bpm bpm])
                 :time beat-of
                 }
        ]
    (fn
      ([] (beat-of (now)))
      ([beat] (+ (* beat @tick-ms) @start))
      
      ([cmd arg]
         ((cmds cmd) arg)))))

(defn mbeat [m beats inst]
  (fn [time]
    (fn [c]
      (let [this-beat (m :time time)
            next-beat (+ this-beat beats)
            next-time (m next-beat)]
        (at time (inst))
        (apply-at next-time c next-time [])))))

(def bmetro (bmetronome 800))

(def bmetro-beats (chain-beats [(mbeat bmetro 1 beep)
                                (mbeat bmetro 2 beep)
                                (mbeat bmetro 1 beep)
                                (mbeat bmetro 2 beep)
                                (mbeat bmetro 2 beep)
                                (mbeat bmetro 2 beep)]))

(loop-beat bmetro-beats)

(bmetro :bpm 700)

(stop)