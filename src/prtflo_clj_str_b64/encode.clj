(ns prtflo-clj-str-b64.encode
  (:require [prtflo-clj-str-b64.support :as support]))

;  ENCODE PROCESS EXAMPLE
;  0.   Start:        "hello"
;  1.   As chrs:      ["h" "e" "l" "l" "o"]
;  2.   As ints:      [104 101 108 108 111]
;  3.   As bits:      ["1101000" "1100101" "1101100" "1101100" "1101111"]
;  4.   As p-bits:    ["01101000" "01100101" "01101100" "01101100" "01101111"]
;  5.   b64-bits-raw: ["011010" "000110" "010101" "101100" "011011" "000110" "1111"]
;  6.   b64-bits:     ["011010" "000110" "010101" "101100" "011011" "000110" "111100="]
;  7.   p-b64-bits:   ["00011010" "00000110" "00010101" "00101100" "00011011" "00000110" "00111100="]
;  8.   As b64 ints:  ["26" "6" "21" "44" "27" "6" "60="]
;  9.   As b64 chrs:  ["a" "G" "V" "s" "b" "G" "8="]
;  10.  End:          "aGVsbG8="

;-----------------------------------------------------------------
; Values
;-----------------------------------------------------------------

(def b64-character-lookup 
  "A map for looking up Base64 characters by
  index, given an integer.
  
  {0 A, 1 B, ...}"
  (zipmap (range 0 64) support/b64-characters))

;-----------------------------------------------------------------
; Forward declarations
;-----------------------------------------------------------------

(declare str-to-char-strs
         char-strs-to-ints
         ints-to-bits
         bits-to-padded-bits
         padded-bits-to-b64-bits
         b64-bits-to-padded-b64-bits
         padded-b64-bits-to-full-b64-bits
         full-b64-bits-to-b64-ints
         b64-ints-to-b64-chars)

;-----------------------------------------------------------------
; Encoder
;-----------------------------------------------------------------

(defn encode
  "Base64 encode the given value.
  
  This value must be an acceptable string,
  but this function will not perform 
  validation."
  [input]
  (->> input
       str-to-char-strs
       char-strs-to-ints
       ints-to-bits
       bits-to-padded-bits
       padded-bits-to-b64-bits
       b64-bits-to-padded-b64-bits
       padded-b64-bits-to-full-b64-bits
       full-b64-bits-to-b64-ints
       b64-ints-to-b64-chars
       support/str-join))

;-----------------------------------------------------------------
; STEP 1 - String to [char-strings]
;-----------------------------------------------------------------

(defn- str-to-char-strs
  "Convert a string to a seq of
  characters-as-strings.
  
  \"abc\" -> [\"a\" \"b\" \"c\"]"
  [input]
  (map str input))

;-----------------------------------------------------------------
; STEP 2 - [char-strings] to [ints]
;-----------------------------------------------------------------

(defn- char-strs-to-ints
  "Convert a seq of characters-as-
  strings to a seq of integers.
  
  [\"a\" \"b\" \"c\"] -> [97 98 99]"
  [input]
  (map (comp int first) input))

;-----------------------------------------------------------------
; STEP 3 - [ints] to [unpadded bit-string]
;-----------------------------------------------------------------

(defn- ints-to-bits
  "Converts a seq of integers to a
  seq of binary-strings.
  
  [97] -> [\"1100001\"]"
  [input]
  (map support/int-to-bin-str input))

;-----------------------------------------------------------------
; STEP 4 - [unpadded bit-string] to [padded bit-string]
;-----------------------------------------------------------------

(defn- bits-to-padded-bits
  "Converts a seq of unpadded binary-strings
  to a seq of 8-width padded binary-strings.
  
  [\"1100001\"] -> [\"01100001\"]"
  [input]
  (map #(support/left-pad 8 %) input))

;-----------------------------------------------------------------
; STEP 5 - [padded bit-string] to [unpadded b64-bit-string]
;-----------------------------------------------------------------

(defn- padded-bits-to-b64-bits
  "Converts a seq of padded binary-strings
  to a seq of unpadded Base64-binary-strings.
  
  [\"01100001\"] -> [\"011000\" \"01\"]"
  [input]
  (support/regroup 6 input))

;-----------------------------------------------------------------
; STEP 6 - [unpadded b64-bit-string] to [padded b64-bit-string]
;-----------------------------------------------------------------

(defn- pad-b64-bits
  "Pads a Base64-binary-string to 8
  characters, adding any equals
  signs.
  
  \"01\" -> \"010000==\""
  [input]
  (let [len     (count input)
        to-add  (- 6 len)
        padding (case to-add
                  0 ""
                  2 "00="
                  4 "0000==")]
    (str input padding)))

(defn- b64-bits-to-padded-b64-bits
  "Pads a seq of Base64-binary-strings to
  6 characters, adding any equals signs.
  
  [\"01\"] -> [\"010000==\"]"
  [input]
  (map pad-b64-bits input))

;-----------------------------------------------------------------
; STEP 7 - [padded b64-bit-string] to [full b64-bit-string]
;-----------------------------------------------------------------

(defn- padded-b64-bits-to-full-b64-bit-seq
  "Pads a padded Base64-binary-string
  to 8 characters, adding any equals
  signs
  
  \"010000==\" -> \"00010000==\""
  [input]
  (let [bits (subs input 0 6)
        eqs  (subs input 6)
        padded (support/left-pad 8 bits)]
    (str padded eqs)))

(defn- padded-b64-bits-to-full-b64-bits
  "Pads a seq of padded Base64-binary-
  strings to 8 characters, adding any
  equals signs.
  
  [\"010000==\"] -> [\"00010000==\"]"
  [input]
  (map padded-b64-bits-to-full-b64-bit-seq input))

;-----------------------------------------------------------------
; STEP 8 - [padded b64-bit-string] to [b64-int]
;-----------------------------------------------------------------

(defn- full-b64-bits-to-b64-int
  "Converts a Base64-binary-string to
  a Base64-integer, adding any equals
  signs.
  
  \"00010000==\" -> \"16==\""
  [input]
  (let [bits        (subs input 0 8)
        eqs         (subs input 8)
        integer     (support/bin-str-to-int bits)]
    (str integer eqs)))

(defn- full-b64-bits-to-b64-ints
  "Converts a seq of Base64-binary-strings
  to Base64-integers, adding any equals signs.
  
  [\"00010000==\"] -> [\"16==\"]"
  [input]
  (map full-b64-bits-to-b64-int input))

;-----------------------------------------------------------------
; STEP 9 - [b64-int] to [b64-char]
;-----------------------------------------------------------------

(defn- b64-int-to-b64-char
  "Converts a Base64-integer to a 
  Base64-charcter, adding any equals
  signs.
  
  \"16==\" -> \"Q==\""
  [input]
  (let [int-str   (support/strip-equals input)
        integer   (read-string int-str)
        equals    (support/strip-num input)
        character (get b64-character-lookup integer)]
    (str character equals)))

(defn- b64-ints-to-b64-chars
  "Converts a seq of Base64-integers
  to Base64-characters, adding any
  equals signs.
  
  [\"16==\"] -> [\"Q==\"]"
  [input]
  (map b64-int-to-b64-char input))