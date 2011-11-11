(ns beat-monad.normal
  (:use overtone.core)
  (:use clojure.algo.monads))

(defn beat [dur inst]
  (fn [time]
    (let [next-time (+ time dur)]
      (at time (inst))
      next-time)))

;;; example:

(definst hat []
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

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

(def beats (-> (now)
               ((beat 0 hat))
               ((beat 300 beep))
               ((beat 100 hat))
               ((beat 200 beep))
               ((beat 200 hat))))

(stop)