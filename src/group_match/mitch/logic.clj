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

(def students [:Finley :Lark :Julie :Mitch :Gary
               :Jim :Yvonne :Elaine :Coby :Klarisa
               :Emily :Cora :Kevin :Heather :Rick
               :Hazel :Beau :Duke :Scout :Happy])

(def students1 [:Finley :Lark :Julie :Duke :Gary :Jim])

(comment
  ;; Taking inspiration from http://worrydream.com/#!/LearnableProgramming, trying to "break it down" (and get funky!)
  ;; For each session, separate the class of N students into groups of X students
  ;; For each session, each group of students should be unique from group in the previous session
  ;; "Unify" a class of students to class
  ;; (== class [1 2 3 4 5 6 7 8 9 10 11 12 13 141 5 16 17 18 19 20])
  ;; Break down the class into named groups
  ;; session => (== class [( == g1 [_ _ _ _ _]) (== g2 [_ _ _ _ _]) (== g3 [_ _ _ _ _]) (== g4 [_ _ _ _ _])])
  ;; For each named group, this group should be uniqe from the previous group
  ;; session => (unique g1 prev-g1) (unique g2 prev-g2) (unique g3 prev-g3) (unique g4 prev-g4)

  (run 5 [grouped-class]
       (fresh [g1 g2 g3]
              (!= g1 g2)
              (!= g1 g3)
              (!= g2 g3)
              (!= g1 '())
              (!= g2 '())
              (!= g3 '())
              (distincto g1)
              (distincto g2)
              (distincto g3)
              (subseto g1 students1)
              (subseto g2 students1)
              (subseto g3 students1)
;;              (permuteo [g1 g2 g3] students1)
              (== grouped-class [g1 g2 g3])
              ))

  (run 3 [out]
       (fresh [g1 g2 g3 g4]
       (matche [students]
               ([[?a ?b ?c ?d ?e . _]] (== g1 ["a " ?a ?b ?c ?d ?e]))
               ([[_ _ _ _ _ ?a ?b ?c ?d ?e . _]] (== g2 ["b " ?a ?b ?c ?d ?e]))
               ([[_ _ _ _ _ _ _ _ _ _ ?a ?b ?c ?d ?e . _]] (== g3 ["c " ?a ?b ?c ?d ?e]))
               ([[_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ?a ?b ?c ?d ?e . _]] (== g4 ["d " ?a ?b ?c ?d ?e])))
       (== out [g1 g2 g3 g4])))

  (run 10 [o]
       (fresh [o2] 
              (permuteo students o2)
              (matche [o2] ([[?a1 ?a2 ?a3 ?a4 ?a5 ?a6 ?a7 ?a8 ?a9 ?a10 ?a11 ?a12 ?a13 ?a14 ?a15 ?a16 ?a17 ?a18 ?a19 ?a20]]
                      (== o [[?a1 ?a2 ?a3 ?a4] [?a5 ?a6 ?a7 ?a8] [?a9 ?a10 ?a11 ?a12] [?a13 ?a14 ?a15 ?a16] [?a17 ?a18 ?a19 ?a20]])))))

  
  (let [ct (count students)]
    (run* [q] (even-splito ct 2 q)))
)
