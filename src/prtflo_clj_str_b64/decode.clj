(ns prtflo-clj-str-b64.decode
  (:require [prtflo-clj-str-b64.support :as support]))

;  DECODE PROCESS EXAMPLE
;  0.  Start:        "aGVsbG8="
;  1.  As b64 chrs:  ["a" "G" "V" "s" "b" "G" "8="]
;  2.  As b64 ints:  ["26" "6" "21" "44" "27" "6" "60="]
;  3.  raw-b64-bits: ["11010" "110" "10101" "101100" "11011" "110" "111100="]
;  4.  pad-b64-bits: ["011010" "000110" "010101" "101100" "011011" "000110" "111100="]
;  5.  fin-b64-bits: ["011010" "000110" "010101" "101100" "011011" "000110" "1111"]
;  6.  bits:         ["01101000" "01100101" "01101100" "01101100" "01101111"]
;  7.  ints:         [104 101 108 108 111]
;  8.  chars:        ["h" "e" "l" "l" "o"]
;  9.  End:          "hello"

;-----------------------------------------------------------------
; Values
;-----------------------------------------------------------------

(def b64-integer-lookup 
  "A map for looking up integers by
  Base64-characters.
  
  {A 0, B 1, ...}"
  (zipmap support/b64-characters (range 0 64)))

;-----------------------------------------------------------------
; Forward declarations
;-----------------------------------------------------------------

(declare str-to-b64-chars
         b64-chars-to-b64-ints
         b64-ints-to-raw-b64-bits
         raw-b64-bits-to-padded-b64-bits
         padded-b64-bits-to-final-b64-bits
         final-b64-bits-to-bits
         bits-to-ints
         ints-to-char-strs)

;-----------------------------------------------------------------
; Decoder
;-----------------------------------------------------------------

(defn decode
  "Base64 decode the given value.
  
  This value must be an acceptable string,
  but this function will not perform 
  validation."
  [input]
  (->> input
       str-to-b64-chars
       b64-chars-to-b64-ints
       b64-ints-to-raw-b64-bits
       raw-b64-bits-to-padded-b64-bits
       padded-b64-bits-to-final-b64-bits
       final-b64-bits-to-bits
       bits-to-ints
       ints-to-char-strs
       support/str-join))

;-----------------------------------------------------------------
; STEP 1 - String to [b64-characters]
;-----------------------------------------------------------------

(defn- reducer
  "Reducer function for converting
  a Base64 string to a seq of Base64
  characters-as-strings.
  
  [] \"a\" -> [\"a\"]
  [\"a\"] \"b\" -> [\"a\" \"b\"]
  [\"a\"] \"=\" -> [\"a=\"]"
  [lst cur]
  (let [tmp-init (butlast lst)
        init     (if (nil? tmp-init) [] (vec tmp-init))
        end      (last lst)
        cur-str  (str cur)]
    (if (= cur-str "=")
      (conj init (support/str-join [end cur-str]))
      (conj lst cur-str))))

(defn- str-to-b64-chars
  "Converts a Base64 string to a seq
  of Base64 characters-as-strings
  
  \"abc=\" -> [\"a\" \"b\" \"c=\"]"
  [input]
  (reduce reducer [] input))

;-----------------------------------------------------------------
; STEP 2 - [b64-characters] to [b64-integers]
;-----------------------------------------------------------------

(defn- b64-char-to-b64-int
  "Converts a Base64-character to
  a Base64-integer, adding any equals
  signs..
  
  \"Q==\" -> \"16==\""
  [input]
  (let [char-str  (support/strip-equals input)
        character (nth char-str 0)
        integer   (get b64-integer-lookup character)
        equals    (support/strip-alphanum input)]
    (str integer equals)))

(defn- b64-chars-to-b64-ints
  "Converts a seq of Base64-characters 
  to Base64-integers, adding any equals
  signs.
  
  [\"Q==\"] -> [\"16==\"]"
  [input]
  (map b64-char-to-b64-int input))

;-----------------------------------------------------------------
; STEP 3 - [b64-integers] to [unpadded b64-bit-string]
;-----------------------------------------------------------------

(defn- b64-int-to-raw-b64-bits
  "Converts a Base64-integer to a raw
  Base64-binary-string, adding any equals
  signs.
  
  \"60=\" -> \"111100=\"
  \"6\" -> \"110\""
  [input]
  (let [int-str (support/strip-equals input)
        equals  (support/strip-num input)
        integer (read-string int-str)
        bits    (Integer/toString integer 2)]
    (str bits equals)))

(defn- b64-ints-to-raw-b64-bits
  "Converts a seq of Base64-integers
  to raw Base64-binary-strings, adding
  any equals signs.
  
  [\"60=\"] -> [\"111100=\"]
  [\"6\"] -> [\"110\"]"
  [input]
  (map b64-int-to-raw-b64-bits input))

;-----------------------------------------------------------------
; STEP 4 - [unpadded b64-bit-string] to [padded b64-bit-string]
;-----------------------------------------------------------------

(defn- raw-b64-bit-seq-to-padded-b64-bit-seq
  "Pads a raw Base64-binary-string to 6
  characters, adding any equals signs.
  
  \"110\" -> \"000110\""
  [input]
  (let [bits        (support/strip-equals input)
        equals      (support/strip-num input)
        padded-bits (support/left-pad 6 bits)]
    (str padded-bits equals)))

(defn- raw-b64-bits-to-padded-b64-bits
  "Converts a seq of unpadded Base64-
  binary-strings to 6-width Base64-
  binary-strings, adding any equals signs.
  
  [\"110\"] -> [\"000110\"]"
  [input]
  (map raw-b64-bit-seq-to-padded-b64-bit-seq input))

;-----------------------------------------------------------------
; STEP 5 - [padded b64-bit-strng] to [final b64-bit-string]
;-----------------------------------------------------------------

(defn- padded-b64-bit-seq-to-final-b64-bit-seq
  "Adjusts a Base64-binary-string for any
  equals signs.
  
  \"111100=\" -> \"1111\""
  [input]
  (let [bits       (support/strip-equals input)
        equals     (support/strip-num input)
        to-drop    (* (count equals) 2)
        bit-len    (count bits)
        target-len (- bit-len to-drop)
        fin-bits   (take target-len bits)]
    (support/str-join fin-bits)))

(defn- padded-b64-bits-to-final-b64-bits
  "Adjust a seq of Base64-binary-strings
  for equals signs.
  
  [\"111100=\"] -> [\"1111\"]
  [\"111100\"] -> [\"111100\"]"
  [input]
  (map padded-b64-bit-seq-to-final-b64-bit-seq input))

;-----------------------------------------------------------------
; STEP 6 - [final b64-bit-string] to [bit string]
;-----------------------------------------------------------------

(defn- final-b64-bits-to-bits
  "Converts a seq of unpadded Base64-binary-
  strings to a seq of padded binary-strings.
  
  [\"011000\" \"01\"] -> [\"01100001\"]"
  [input]
  (support/regroup 8 input))

;-----------------------------------------------------------------
; STEP 7 - [bit string] to [integer]
;-----------------------------------------------------------------

(defn- bits-to-ints
  "Converts a seq of binary-strings
  to integers.
  
  [\"01101111\"] -> [111]"
  [input]
  (map support/bin-str-to-int input))

;-----------------------------------------------------------------
; STEP 8 - [integer] to [character-string]
;-----------------------------------------------------------------

(defn- ints-to-char-strs
  "Converts a seq of integers
  to characters-as-strings.
  
  [111] -> [\"o\"]"
  [input]
  (map (comp str char) input))