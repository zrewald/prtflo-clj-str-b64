(ns prtflo-clj-str-b64.core
  (:require [prtflo-clj-str-b64.encode :as encode]
            [prtflo-clj-str-b64.decode :as decode]
            [prtflo-clj-str-b64.support :as support]))

;-----------------------------------------------------------------
; Forward declarations
;-----------------------------------------------------------------

(declare perform
         validate-encode-input
         validate-decode-input
         is-string
         is-ascii
         is-base64)

;-----------------------------------------------------------------
; Encoders/decoders
;-----------------------------------------------------------------

(defn encode
  "Base64-encode a value.
  
  The value must be:
  - a string
  - ASCII-only"
  [input]
  (perform validate-encode-input encode/encode input))

(defn decode
  "Base64-decode a value.
  
  The value must be:
  - a string
  - Base64-characters only"
  [input]
  (perform validate-decode-input decode/decode input))

(defn perform
  "Perform a given encoding/decoding
  function after validation.
  
  - Runs validate-fnc against input
  - If validate-fnc returns nil, run 
    action-fnc against input; otherwise,
    throw an exception with validate-fnc's
    return value as the message."
  [validate-fnc action-fnc input]
  (let [e (validate-fnc input)]
    (if (nil? e)
      (action-fnc input)
      (throw (Exception. e)))))

;-----------------------------------------------------------------
; Validators
;-----------------------------------------------------------------

(defn validate-encode-input
  "Determine if input can be
  Base64-encoded."
  [input]
  (cond
    (not (is-string input)) "Input must be a string"
    (not (is-ascii  input)) "Input must be ASCII-only"
    :else nil))

(defn validate-decode-input
  "Determine if input can be
  Base64-decoded."
  [input]
  (cond
    (not (is-string input)) "Input must be a string"
    (not (is-base64 input)) "Input is not base64"
    :else nil))

;-----------------------------------------------------------------
; Predicates
;-----------------------------------------------------------------

(defn is-string
  "Determine if input is a string."
  [input]
  (instance? String input))

(defn is-ascii
  "Determine if input contains
  only ASCII characters."
  [input]
  (let [ints         (map int input)
        out-of-range #(> % 128)
        bads         (filter out-of-range ints)]
    (= 0 (count bads))))

(defn is-base64
  "Determine if input is a
  Base64 value."
  [input]
  (let [b64-chars       (map char support/b64-characters)
        chars           (conj b64-chars \=)
        not-in-charset? #(not (some #{%} chars))
        bads            (filter not-in-charset? input)]
    (= 0 (count bads))))