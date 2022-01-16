(ns instant-runoff.core
  (:require [clojure.string :as str])
  (:gen-class))

;;;; Helpers

(defn- nth-votes
  "From a pool of votes, get all nth votes.  Returns a map with the
  frequencies for each relevant candidate."
  [^long n votes]
  (letfn [(safe-nth [xs]
            (when (< n (count xs))
              (nth xs n)))]
    (frequencies (keep safe-nth (into [] (vals votes))))))

;; An entry with an associated score, which represents the votes that
;; submission received *in some round*.
(defrecord Entry [name score])

(defn- partition-by-values
  "Partition a map of votes by the scores of the entries.  Returns a list
  of `Entry`s."
  [votes]
  (->> votes
       (sort-by second)                 ; value
       (map (fn [[k v]] (->Entry k v))) ; cries in `curry`
       (partition-by :score)))

;;;; Parsing

(defn- correct [w]
  (case w
    "сonstructor"            "constructor"              ; cyrillic `c`
    "miminalist"             "minimalist"
    "submission 2"           "muelphil's submission 2"
    "lambda x"               "lambda-x"
    "racoon (ofc)"           "racoon"
    "pheonix"                "phoenix"
    "pohenix"                "phoenix"
    "minimal xm2"            "minimal xm 2"
    "minimal xm1"            "minimal xm 1"
    "curvy meets x"          "curvy wide"
    "curvey wide"            "curvy wide"
    "muelphils submission 2" "muelphil's submission 2"
    "4 screens"              "4screens"
    ;; assumptions (non-critical)
    "modern basic"           "modern basic 1"
    "minimal xm"             "minimal xm 1"
    w))

(defn- parse-line [l]
  (case (correct (str/lower-case (str/replace-first l #"\s*\d+.\s*" "")))
    "lambda-x"                    :lambda-x
    "vmon"                        :vmon
    "crowbar"                     :Crowbar
    "constructor"                 :Constructor
    "minimal xm 2"                :Minimal-XM-2
    "xclock"                      :XClock
    "curvy wide"                  :curvy-wide
    "homenad"                     :Homenad
    "gradient patriot"            :Gradient-Patriot
    "monado"                      :monado
    "ssfwshutterbug's submission" :ssfw's-sub
    "x >>="                       :X->>=
    "wide x v1"                   :Wide-X-v1
    "xmonad spaces"               :XMonad-Spaces
    "phoenix"                     :Phoenix
    "greenmonad"                  :greenmonad
    "muelphil's submission 2"     :mp's-sub-2
    "xmondrian"                   :XMondrian
    "haskell meets x"             :Haskell-meets-X
    "new monad"                   :New-Monad
    ">x="                         :>X=
    "bugreporteur's submission"   :bugrep's-sub
    "4screens"                    :four-screens
    "modern basic 2"              :Modern-Basic-2
    "modern basic 1"              :Modern-Basic-1
    "powermonad"                  :powermonad
    "minimal xm 1"                :Minimal-XM-1
    "xpinwheel"                   :XPin-Wheel
    "xemonad"                     :Xemonad
    "minimalist"                  :minimalist
    "xmonovad"                    :Xmonovad
    "racoon"                      :racoon))

(defn parse [input]
  (into {}
        (comp (map str/split-lines)      ; individual votes with voter
              (map (fn [[voter & votes]]
                     {(keyword voter)
                      (mapv parse-line votes)})))
        (str/split input #"\n\n")        ; group by submission
        ))

;;;; Tie breaking

(defn- least-nth-votes
  "Check which (if any) of the given candidates has the least nth votes,
  for every sensible n larger than 0 (i.e., do not check first choice
  votes and stop when all lists are empty).  Only advances to rank n+1
  with the lowest ranking submissions from rank n.

  Returns `nil` if there is a tie all the way through."
  [votes cs]
  (let [max-length (apply max (map (comp count second) votes))]
    (loop [n 1, candidates cs]
      (when (<= n max-length)
        (let [nthvotes  (nth-votes n votes)
              cands     (map #(vector % (get nthvotes % 0)) candidates)
              min-cands (map :name (first (partition-by-values cands)))]
          (if (= 1 (count min-cands))
            min-cands
            (recur (inc n) min-cands)))))))

(defn- tie-break
  "If necessary—i.e., if there are multiple candidates with the lowest
  number of first-choice votes—we tie break as follows:
    1. If all lowest votes combined do not reach the value of the
       second-lowest position, they can't possibly overtake it and are
       thus eliminated all at once.
    2. If there is a candidate with fewer nth place votes than all
       others, eliminate that one.
    3. Otherwise, randomly eliminate someone."
  [votes first-votes]
  (let [sorted-votes (partition-by-values first-votes)
        lows         (first sorted-votes)]
    (if (or (= 1 (count lows))
            (< (apply + (map :score lows))                ; 1.
               (or (:score (first (second sorted-votes)))
                   0)))
      (map :name lows)
      (if-let [least-nth (least-nth-votes votes (map :name lows))]
        least-nth                                         ; 2.
        [(:name (rand-nth lows))]))))                     ; 3.

;;;; Instant runoff

(defn instant-runoff [all-votes]
  (loop [votes  all-votes
         rounds []]
    (let [first-votes    (nth-votes 0 votes)
          [hi-kw hi-val] (apply max-key val first-votes)
          elims          (tie-break votes first-votes)
          ;; Re-calculate this in every round in case all of people's
          ;; votes become void (in which case they are removed).
          barrier        (quot (apply + (vals first-votes)) 2)]
      (if (> hi-val barrier)
        [[hi-kw hi-val] (conj rounds {:votes votes})] ; > 50% ⇒ only one winner
        (recur (into {}
                     (comp (map (fn [[voter [v & vs :as vts]]]
                                  [voter (if (some #{v} elims) vs vts)]))
                           (filter (comp some? second)))
                     votes)
               (conj rounds {:elims elims, :votes votes}))))))

;;;; Pretty things

(defn -main [& _args]
  (letfn [(delim []
            (println (apply str (repeat 72 "-"))))
          (first-choice [votes]
            (into {}
                  (map (fn [[k v]] [(name k) v]))
                  (nth-votes 0 votes)))
          (align [n s]
            (apply str (take n (apply str s (repeat n \space)))))
          (align-coll [votes cur-row]
            (mapv (partial align (apply max (map count (vals votes))))
                  cur-row))]

    (let [input (parse (slurp "resources/input.txt"))
          [[win win-val] & rounds] (instant-runoff input)]

      ;; individual rounds
      (doseq [[n {:keys [elims votes]}] (apply map vector (range) rounds)]
        (delim)
        (println "Round" (inc n) "first-choice votes:" (first-choice votes))
        (println "Eliminating:" (mapv name elims))
        (println "Remaining votes:")
        (doseq [[v vs] votes]
          (println "  "
                   (align 20 (str (name v) ":"))
                   (align-coll votes (mapv name vs))))
        (println ""))

      (delim)
      (println "End result:" (name win) "wins with" win-val "first-choice votes")
      (delim)

      ;; Just making sure :)
      (println "Running the election 100 times, just for fun:"
               (reduce (fn [mmm _]
                         (let [[[win _] & _] (instant-runoff input)]
                           (merge-with + mmm {win 1})))
                       {}
                       (range 0 100))))))
