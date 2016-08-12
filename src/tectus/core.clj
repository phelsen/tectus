(ns tectus.core)

(defn str->nb [str_]
  (apply + (map int str_)))

(defn distribute [nb nb-parts]
  (for [i  (range 1 (inc nb-parts))]
    (* i (int (/ nb nb-parts)))))

(defn  shorten  [str_ nb]
  (let [sum (str->nb str_)
        dist (distribute sum nb)]
    (apply str (map char dist))))

(defn my+ [a b]
  (let [sum (+ a b)]
    (if (> sum 65535) (- sum 65535) sum)))

(defn my- [a b]
  (let [sum (- a b)]
    (if (< sum 0) (+ sum 65535) sum)))


(defn nth_ [ str_ n]
  (let [l (count str_)
        n (if (>= n l) (mod n l) n)]
    (nth str_  n)))

(defn trans [encrypt? elem nb key_]
  (let [step (-> key_ (nth_ nb) int)]
    (if encrypt? (my+ elem step) (my- elem step))))

(defn en-or-de-crypt  [str_ key_ encrypt?]
  (let [str-len (count str_)
        key-len (count key_)
        key__ (if (< str-len key-len) (shorten key_ str-len) key_)
        ints (map int str_ )
        transed (map-indexed #(trans encrypt? %2 %1 key__) ints)
        chars_ (map char transed)]
    (apply str chars_)))

(defn encr [str_ key_] (en-or-de-crypt str_ key_ true ))
(defn decr [str_ key_] (en-or-de-crypt str_ key_ false ))

