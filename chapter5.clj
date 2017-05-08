(ns chapter5.core)

;
; exercise 1
;

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

(defn attr [attribute] (comp attribute :attributes))


;
; exercise 2: implement the comp function
;


; first solution

(defn my-comp1
  ([] my-comp1)
  ([f] #(apply f %&))
  ([f & fns] #(f (apply (apply my-comp1 fns) %&))))


;; better solution

(defn my-comp2
  [& fs]
  (fn
    [& args]
    (reduce
       #(%2 %1)
       (apply (last fs) args)
       (reverse (drop-last fs)))))


; next solutions from: https://stackoverflow.com/questions/21371860/clojure-implementing-the-comp-function

; from user "A. Webb" 

(defn my-comp3 [& fns]
  (fn [& args]
    (let [ordered-fns (reverse fns)
          first-result (apply (first ordered-fns) args)
          remaining-fns (rest ordered-fns)]
    (reduce 
      (fn [result-so-far next-fn] (next-fn result-so-far))
      first-result
      remaining-fns))))


; from user "toolkit"

(defn my-comp4
  [& fs]
  (reduce
     (fn [f g] #(f (apply g %&)))
     fs))


; Best Solution: from user "jbm"

(defn chain
  [f g]
  #(f (apply g %&)))

;; (defn chain
;;   [f g]
;;   (fn
;;     [& args]
;;     (f (apply g args))))

(defn my-comp5
  [& fs]
  (reduce
     chain
     identity
     fs))

