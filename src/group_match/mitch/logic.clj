(ns group-match.mitch.logic
  (:require [clojure.math.numeric-tower :as c_tower]
            [clojure.core.logic.fd :as c_fd])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; My (Mitch Thomas) first attempt at using core.logic... WOOT!

;; Give me all combinations where new two items appear in the same group more than once (or twice if we want to be a little looser).
;; (def classes [:C1 :C2 :C3])
;; (def students [:F :L :J :M :A :B]
;; Possible answer
;; C1: (F L A) (J M B)
;; C2: (F J A) (M L B)
;; C3: (F M A) (J L B)

;; taken from https://github.com/ejackson/EuroClojure-2012-Talk/blob/master/src/timetable/demo.clj
(defne permo
  "Succeeds if v1 is a permutation of v2."
  [v1 v2]
  ([[] []])
  ([[h . t] v2]
     (fresh [rv2]
            (membero h v2)
            (rembero h v2 rv2)
            (permo t rv2))))

;; taken from https://github.com/ejackson/EuroClojure-2012-Talk/blob/master/src/timetable/demo.clj
(defne subseto
  "Succeeds if x1 is a subset of x2.  x1 is distinct with elements of x2 but is shorter than or equal to x2"
  [x1 x2]
  ([[] _])
  ([[a . r] x2]
     (fresh [rx2]
            (membero a x2)
            (rembero a x2 rx2)
            (subseto r rx2))))

;; I forgot where I got this
(defne partitiono [a b c d]
  ([[x . l] _ [x . l1] _]
     (conda
       ((project [x b]
          (== (<= x b) true))
        (partition l b l1 d))
       (partition l b c d))))

;; matche is great, check this out
;; https://github.com/frenchy64/Logic-Starter/wiki

;; Attempt to re-state the problem space in terms of goals  
;; given a total set of students
;; create groups for each class
;; each class should have unique groups from the previous class
;; where unique is defined as
;;  for group least one member of the group is different

(def students [:Finley :Lark :Julie :Mitch :Gary :Jim :Yvonne :Elaine :A :B :C :D :E :F :G :H :I :J :K :L])

;; Successfull for any even divisions of ct
(defne even-splito [ct gt out]
  ([]
     (fresh [x y]
         (c_fd/in y (c_fd/interval gt (- ct 1)))
         (c_fd/in x (c_fd/interval 1 (- ct 1)))
         (c_fd/* x y ct)
         (== out [x y]))))

(defne groupo [l out]
  ([]
     (subseto out l)
     (matche [out] ([[_ _ _ _]] succeed))))

(comment
  (run 40 [out] (groupo students out))

  (run 4 [out]
       (permuteo out students)
       (matche [out] ([[_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _]] succeed))
       )

  (let [ct (count students)]
    (run* [q] (even-splito ct 2 q)))
)
