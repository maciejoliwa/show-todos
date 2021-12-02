(ns pr-todo-collec.collector
  (:require [clojure.string :as str]))

; TODO - Add support for "" comments
; TODO - Add support for FIXME, FIXME, ! etc.

(defn comment?
  [^String line]
  (str/starts-with? (str/triml line) ";"))

(defn trim-semicolon
  [^String line]
  (str/replace line #";" ""))

(defn todo?
  [^String line]
  (cond
    (comment? line) (-> line
                        (trim-semicolon)
                        (str/lower-case)
                        (str/triml)
                        (str/starts-with? "todo"))
    :else false))
