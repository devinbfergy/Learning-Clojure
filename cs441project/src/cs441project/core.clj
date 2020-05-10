(ns cs441project.core
  (:require [clojure.java.io :as io] )
  (:require [com.climate.claypoole :as cp]))

(comment this merge function is taken from "https://gist.github.com/alco/2135276")
(defn merge-stuff
  ([left right]
     (merge-stuff (list left right)))
  ([[left right]]
    (loop [l left, r right, result []]
      (let [lhead (first l), rhead (first r)]
        (cond
          (nil? lhead) (concat result r)
          (nil? rhead) (concat result l)
          (<= lhead rhead) (recur (rest l) r (conj result lhead))
          true (recur l (rest r) (conj result rhead)))))))

(defn merge-sort [nums]
  (if (< (count nums) 2)
    nums
    (apply merge-stuff (map merge-sort (split-at (/ (count nums) 2) nums)))))

(defn merge-sort-concurrent [nums threads]
  (if (< (count nums) 2)
    nums
    (apply merge-stuff (cp/pmap threads merge-sort (split-at (/ (count nums) 2) nums)))))

(defn -main []
  (println "Beginning Merge Sort of numbers.txt with Concurrent Threads")
  (def data-file (io/resource "numbers.txt"))
  (comment how I read in the file into a list of integers "https://stackoverflow.com/a/48437887")
  (def numbers (read-string (str "(" (slurp data-file) ")")))
  (print "1   thread: ")
  (def sorted-1 (time (merge-sort numbers)))

  (print "2  threads: ")
  (def sorted-2 (time (merge-sort-concurrent numbers 2)))

  (print "4  threads: ")
  (def sorted-4 (time (merge-sort-concurrent numbers 4)))

  (print "8  threads: ")
  (def sorted-8 (time (merge-sort-concurrent numbers 8)))

  (print "16 threads: ")
  (def sorted-16 (time (merge-sort-concurrent numbers 16)))

  (print "32 threads: ")
  (def sorted-32 (time (merge-sort-concurrent numbers 32)))

  (System/exit 0)
  )





