;; Example 1 : clojure script
;; 
;; In this example, we create some instances of patterns,
;; train a Chrest model with them, and then print out 
;; and display what Chrest has learnt.
;;
;; To run this script you will need clojure.jar, jchrest.jar and 
;; example-1.clj together in the same directory.  Then type:
;; java -jar clojure.jar example-1.clj

(add-classpath "file:jchrest.jar")
(import 'jchrest.architecture.Chrest)
(import 'jchrest.gui.ChrestView)
(import 'jchrest.lib.Pattern)
(import 'jchrest.lib.NumberPattern)
(import 'jchrest.lib.ListPattern)

;; Create an instance of the Chrest model
(def model (new Chrest))

;; Convenience function to convert a list of integers into 
;; an ArrayList and then creating a ListPattern
(defn make-number-pattern [numbers]
     (def pat (new ListPattern))
     (doseq [num numbers]
       (.add pat (. NumberPattern create num)))
     pat)

;; Create three pattern instances
(def pattern-1 (make-number-pattern '(1 2 3)))
(def pattern-2 (make-number-pattern '(1 3 2)))
(def pattern-3 (make-number-pattern '(2 1 3)))

;; store them in a list
(def patterns (list pattern-1 pattern-2 pattern-3))

;; Train the model a few times on the patterns
(dotimes [i 4]
  (doseq [pat patterns]
         (.recogniseAndLearn model pat)))

;; Display the results
(print "Current model time: ") (println (.getClock model))
(doseq [pat patterns]
       (println (format "For pattern: %s model retrieves %s" 
                        (.toString pat)
                        (.toString (.recallPattern model pat)))))

;; And display the model in a graphical view
(new ChrestView nil model)

