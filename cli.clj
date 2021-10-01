(ns cypress.cli
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [svg-clj.composites :refer [svg]]
            [svg-clj.tools :as tools]
            [cypress.main :as cyp]))

(def cli-options
  [["-h" "--help"]
   ["-o" "--output OUTPUT" "The output path/filename."
    :default "output.svg"]])

(defn -main
  [& args]
  (let [parsed (cli/parse-opts args cli-options)
        opts (:options parsed)]
    (cond
      (:help opts)
      (println (str "Usage:\n" (:summary parsed)))

      :else
      (do
        (println "Running (art-grid) function, saving output to" (:output opts))
        (-> (cyp/art-grid)
            svg
            (tools/save-svg (:output opts)))))))

(apply -main *command-line-args*)
