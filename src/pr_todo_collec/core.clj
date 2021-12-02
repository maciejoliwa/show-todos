(ns pr-todo-collec.core
  (:require [clojure.string :as str]
            [pr-todo-collec.collector :as collector])
  (:gen-class))

(def clj-extensions '("clj", "cljs", "cljc"))
(def color-green "\033[92m")
(def color-yellow "\033[93m")
(def color-reset "\033[0m")

(defn- clojure-file?
  "Checks if a given string filename is a clojure file"
  [extensions ^String filename]
  (let [ext (last (str/split filename #"\."))]
    ; TODO - replace it with a better way of doing it
    (not (nil? (some #(= ext %) extensions)))))

(defn get-clojure-files
  "Takes a directory string as an argument, it turns it into a java File (directory), and then it filters the list of files based on
  file extensions of clojure files (clj, cljs, cljc). It simply returns list of filenames that are clojure source files.
  "
  [^String directory]
  ; TODO - Make it check subfolders
  ; TODO - Make it return nil if there are no Clojure files in a given directory
  (let [dir (clojure.java.io/file directory) files (file-seq dir)]
    (filter (partial clojure-file? clj-extensions) (map #(str (clojure.java.io/file directory (.getName %))) files))))

(defn get-todos
  "First, it takes a list of file names, it transforms it into a list of vectors likes this:
  -> '('test.clj', 'core.clj')
  -> '(['test.clj' 'file-content], ['core.clj' 'file-content']))

  After that, it filters file contents looking for lines that are todos, it returns a list of vectors like this:
  '(['filename' '(';todo 1' ';todo2')])
  "
  [file-list]
  (let [result-map (map #(vector % (str/split-lines (slurp %))) file-list)]
    (map #(vector (first %) (filter collector/todo? (last %))) result-map)))

(defn print-todos
  "Prints all todos of one file"
  [todos]
  (doseq [todo todos] (println "\t- " (str/triml todo))))

(defn print-results
  [todos]
  (doseq [v todos]
    (let [filename (first v) file-todos (last v) todo-count (count file-todos)]
      (cond
        (= todo-count 1) (do (println (str "1 TODO found in " color-green filename color-reset ":")) (print-todos file-todos))
        (> todo-count 1) (do (println (str todo-count " TODOS found in " color-green filename color-reset ":")) (print-todos file-todos))
        (zero? todo-count) (println (str color-yellow "No TODOS found in " color-green filename color-reset))))))

(defn -main
  [& args]
  (let [folder (first args)]
    (if (nil? folder)
      (println "Not a directory")
      (let [todos (get-todos (get-clojure-files folder))]
        (print-results todos)))))
