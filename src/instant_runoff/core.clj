(ns instant-runoff.core
  (:require [clojure.string :as str])
  (:gen-class))

;;;; Helpers

(defn- value [coll]
  (second coll))

(defn- name-of [coll]
  (first coll))

(defn- nth-votes
  "From a pool of votes, get all nth votes.  Returns a map with the
  frequencies for each relevant candidate."
  [n votes]
  (letfn [(safe-nth [n xs]
            (when (< n (count xs))
              (nth xs n)))]
    (frequencies (keep (partial safe-nth n) (vals votes)))))

(defn- partition-by-values
  "Partition a map of votes by value."
  [votes]
  (partition-by value (sort-by value votes)))

;;;; Parsing

(defn- correct [w]                      ; TODO: fill out
  w)

(defn- parse-line [l]
  (case (correct (str/lower-case (str/replace-first l #"\d+.\s+" "")))
    "lambda-x"                    :lambda-x
    "vmon"                        :vmon
    "crowbar"                     :Crowbar
    "сonstructor"                 :Сonstructor
    "minimal xm 2"                :Minimal-XM-2
    "xclock x"                    :Clock
    "curvy wide"                  :curvy-wide
    "homenad"                     :Homenad
    "gradient patriot"            :Gradient-Patriot
    "monado"                      :monado
    "ssfwshutterbug's submission" :ssfwshutterbug's-submission
    "x >>="                       :X->>=
    "wide x v1"                   :Wide-X-v1
    "xmonad spaces"               :XMonad-Spaces
    "phoenix"                     :Phoenix
    "greenmonad"                  :greenmonad
    "muelphil's submission 2"     :muelphil's-submission-2
    "xmondrian"                   :XMondrian
    "haskell meets x"             :Haskell-meets-X
    "new monad"                   :New-Monad
    ">x="                         :>X=
    "bugreporteur's submission"   :bugreporteur's-submission
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
              min-cands (first (partition-by-values cands))]
          (if (= 1 (count min-cands))
            (map name-of min-cands)
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
  [votes]
  (let [first-votes  (nth-votes 0 votes)
        sorted-votes (partition-by-values first-votes)
        lows         (first sorted-votes)]
    (if (or (= 1 (count lows))
            (< (apply + (map value lows))
               (or (value (first (second sorted-votes))) ; possible next value
                   0)))
      (map name-of lows)
      (if-let [least-nth (least-nth-votes votes lows)]
        least-nth
        (do
          (println "Selecting randomly...")
          [(name-of (rand-nth lows))])))))

;;;; Instant runoff

(defn instant-runoff [all-votes]
  (loop [votes  all-votes
         rounds []]
    (let [first-votes    (nth-votes 0 votes)
          [hi-kw hi-val] (apply max-key val first-votes)
          elims          (tie-break votes)
          ;; Re-calculate this in every round in case all of people's
          ;; votes become void (in which case they are removed).
          barrier        (quot (apply + (vals first-votes)) 2)
          ]
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
  (let [delim (fn [] (println (apply str (repeat 72 "-"))))
        input (parse nil)                       ; TODO
        [[win win-val] & rounds] (instant-runoff input)]
    (doseq [[n {:keys [elims votes]}] (apply map vector (range) rounds)]
      (delim)
      (println (str "Eliminating " (mapv name elims) " in round " (inc n)))
      (delim)
      (println (str "Remaining votes:"))
      (doseq [[v vs] votes]
        (println (str "  " (name v) "'s votes: " (mapv name vs))))
      (println ""))
    (delim)
    (println (str "End result: "
                  (name win) " wins with "
                  win-val " first votes"))
    (delim)
    ;; Just making sure :)
    (reduce (fn [mmm _]
              (let [[[win _] & _] (instant-runoff input)]
                (merge-with + mmm {win 1})))
            {}
            (range 0 1000))))
