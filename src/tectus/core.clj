(ns tectus.core
  (:require [clojure.java.io :as io]))

(defn- str->nb [str_]
  (apply + (map int str_)))

(defn-  distribute [nb nb-parts]
  (for [i  (range 1 (inc nb-parts))]
    (* i (int (/ nb nb-parts)))))

(defn-  shorten  [str_ nb]
  (let [sum (str->nb str_)
        dist (distribute sum nb)]
    (apply str (map char dist))))

(defn- my+ [a b]
  (let [sum (+ a b)]
    (if (> sum 65535) (- sum 65535) sum)))

(defn- my- [a b]
  (let [sum (- a b)]
    (if (< sum 0) (+ sum 65535) sum)))

(defn- nth_ [ str_ n]
  (let [l (count str_)
        n (if (>= n l) (mod n l) n)]
    (nth str_  n)))

(defn- trans [encrypt? elem nb key_]
  (let [step (-> key_ (nth_ nb) int)]
    (if encrypt? (my+ elem step) (my- elem step))))

(defn- en-or-de-crypt  [str_ key_ encrypt?]
  (let [str-len (count str_)
        key-len (count key_)
        key__ (if (< str-len key-len) (shorten key_ str-len) key_)
        ints (map int str_ )
        transed (map-indexed #(trans encrypt? %2 %1 key__) ints)
        chars_ (map char transed)]
    (apply str chars_)))


(defn encrypt [str_ key_] (en-or-de-crypt str_ key_ true ))
(defn decrypt  [str_ key_] (en-or-de-crypt str_ key_ false ))


(defn- file-path->byte-array [url]
  (let [f (io/file url)
        ar (byte-array (.length f))
        is (io/input-stream f)
        ]
    (.read is ar)
    (.close is)
    ar))

(defn- byte-array->file-path [arr url]
  (with-open [os (io/output-stream url)]
    (.write  os arr)))


(defn- bytes->byte-string [bytes_]
  (apply str
         (for [b bytes_]
           (char  (+ 255 b  )))))

(defn-  byte-string->bytes
"only for 'byte-strings', strings under 256"
  [str_]
  (byte-array
   (for [i str_]
     (-  (int i) 255))))

(defn- bytes->encrypted-byte-string [b key_]
  (encrypt  
   (bytes->byte-string b)
   key_))

(defn- encrypted-byte-string->bytes [bs key_]
  (byte-string->bytes (decrypt bs key_)))

(defn self-test [] ( = (-> "tectus" (encrypt "jos") (decrypt "jos")) "tectus"))


(defn encrypt-file-to [file-path encrypted-file-path key_]
    (let [bytes_ (file-path->byte-array file-path)
          encr-bs (bytes->encrypted-byte-string bytes_ key_)]
      (spit  encrypted-file-path encr-bs)))

(defn decrypt-file-to [encrypted-file-path decrypted-file-path key_]
  (let [encr-bs (slurp encrypted-file-path)        
        decr-bytes (encrypted-byte-string->bytes encr-bs key_)]
      (byte-array->file-path decr-bytes decrypted-file-path)))

