(ns prtflo-clj-str-b64.support)

;-----------------------------------------------------------------
; Values
;-----------------------------------------------------------------

(def b64-characters 
  "The acceptable Base64 character set" 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;-----------------------------------------------------------------
; String helpers
;-----------------------------------------------------------------

(defn left-pad
  "Pad a given string to the width specified by len."
  [len input]
  (let [real-len (count input)
        to-add   (- len real-len)
        padding  (clojure.string/join "" (repeat to-add "0"))]
    (str padding input)))

(defn str-join
  "Join a seq together into a string, using \"\" (empty string) 
  as the separator."
  [items]
  (clojure.string/join "" items))

;-----------------------------------------------------------------
; Partitioning
;-----------------------------------------------------------------

(defn regroup
  "Re-partition a seq of binary numbers (strings) into
  partitions of the given size."
  [size input]
  (let [flattened  (clojure.string/join "" input)
        chr-groups (partition size size [] flattened)
        groups     (map #(clojure.string/join "" %) chr-groups)]
    groups))

;-----------------------------------------------------------------
; String helpers
;-----------------------------------------------------------------

(defn strip-equals
  "Strip out any equals-signs from a string."
  [input]
  (clojure.string/replace input #"=" ""))

(defn strip-alphanum
  "Strip out any alphanumeric characters from
  a string."
  [input]
  (clojure.string/replace input #"\w" ""))

(defn strip-num
  "Strip out any numeric characters from 
  a string."
  [input]
  (clojure.string/replace input #"\d" ""))

;-----------------------------------------------------------------
; Conversion helpers
;-----------------------------------------------------------------

(defn bin-str-to-int
  "Convert a binary number (string) to an integer."
  [input]
  (read-string (str "2r" input)))

(defn int-to-bin-str
  "Convert an integer to a binary number (string)"
  [input]
  (Integer/toString input 2))