(ns master-pdf.core
  (:require [clj-pdf.core :as pdf])
  (:gen-class))

(declare document-style flatten-1 coversheets)

;;;;;-----------------------------------
;;;;;  Only public function

(defn produce-coversheets
  "Produces pdf file of coversheets for student exams
  vec-of-maps each map contains the data for one student
  filename is the file to place the pdf output"
  [vec-of-maps ^String filename]
  (let [document [document-style (flatten-1 (mapv coversheets vec-of-maps))]]
    (pdf/pdf document filename)))

;; Sample input data
;; All keys are required
;; :number = student number for exam can be integer or string
;; should :time be divided into :start-time and :end-time?
#_(def data
  [{:name   "Roger Whitney"
    :number 1
    :term   "Fall 2014"
    :exam   "Database Management Systems"
    :date   "Saturday, August 23, 2014"
    :time   "8:00-10:00 AM"
    :room   "GMCS 214"}
   {:name   "B"
    :number 2
    :term   "Fall 2014"
    :exam   "Database Management Systems"
    :date   "Saturday, August 23, 2014"
    :time   "8:00-10:00 AM"
    :room   "GMCS 214"}])

;; Sample calling of function
#_(produce-coversheets data "doc.pdf")

;;;;----------------------------------------
;;;; Styles used in document

(def styles
  {:centered {:align :center}
   :bold     {:style :bold :align :center}
   :strong   {:align :center :styles [:bold :underline]}})

(def document-style
  {:stylesheet styles
   :font       {:size 30}
   :footer     {:page-numbers false}})

;;;;--------------------------------------
;;;; Methods to help construct documents

(defn- centered
  [text]
  [:paragraph.centered text [:spacer]])

(defn- bold
  [text]
  [:paragraph.bold text [:spacer]])

(defn- strong
  [text]
  [:paragraph.strong text [:spacer 2]])

(defn- header-part
  [label value]
  [:paragraph [:phrase {:color [180 180 180]} (str label "   ")] [:phrase value] [:spacer]])

;;;;-------------------------------------
;;;; Methods used to reduce the nesting of vectors to
;;;; keep format acceptable to pdf generator

(defn- append
  "Adds elements of add-vec to base-vec
   (append [1 2] [3 [4 5]]) -> [1 2 3 [4 5]]"
  [base-vec add-vec]
  (if (empty? add-vec)
    base-vec
    (recur (merge base-vec (first add-vec)) (rest add-vec))))

(defn- flatten-1
  "Input vector of vectors, reomves one level of vector by
  replacing the inner vectors with their contents
  (flatten-1 [[1] [2 3 [4]]]) -> [1 2 3 [4]]"
  [vec-of-vec]
  (loop [items vec-of-vec, answer []]
    (if (empty? items)
      answer
      (recur (rest items) (append answer (first items))))))

;;;;-------------------------------------
;;;; The 3 pages in our document

(defn- coversheet-1
  [{:keys [name number date time room exam term]}]
  [
   [:spacer 3]
   (header-part "Number:" (str number))
   (header-part "Name:" name)
   [:spacer 5]
   (centered "Department of Computer Science")
   (centered  term )
   (centered "Master Exam")
   [:spacer 5]
   (bold exam)
   [:spacer 5]
   (centered date)
   (centered time)
   [:spacer 3]
   (centered room)
   [:spacer 5]
   (strong "DO NOT WRITE YOUR NAME")
   (strong "ON ANY PAGE OF THE EXAM")
   [:pagebreak]]
   )

(defn- coversheet-2
  [{:keys [ number date time room exam term]}]
  [
   [:spacer 3]
   (header-part "Number:" (str number))
   [:spacer 5]
   (centered "Department of Computer Science")
   (centered  term )
   (centered "Master Exam")
   [:spacer 5]
   (bold exam)
   [:spacer 5]
   (centered date)
   (centered time)
   [:spacer 3]
   (centered room)
   [:spacer 5]
   (strong "DO NOT WRITE YOUR NAME")
   (strong "ON ANY PAGE OF THE EXAM")
   [:pagebreak]])

(defn- coversheet-3
  [{:keys [number term]}]
  [
   [:spacer 3]
   (header-part "Number:" (str number))
   [:spacer 10]
   (centered term)
   (centered "CS Master Exam")
   (centered "Answer Sheets")
   [:spacer 20]
   (strong "DO NOT WRITE YOUR NAME")
   (strong "ON ANY PAGE OF THE EXAM")
   [:pagebreak]
   ])

(defn- coversheets
  "Combine the 3 pages into one document"
  [data]
  (flatten-1 [(coversheet-1 data)
              (coversheet-2 data)
              (coversheet-3 data)]))

;;;;;------------
;;;;; Orginal raw example of first coversheet
#_(pdf/pdf
  [{:stylesheet styles
    :font       {:size 30}
    :top-margin 100
    :footer     {:page-numbers false}}
   [:spacer 4]
   [:paragraph [:phrase {:color [180 180 180]} "Name:  "] [:phrase "Roger Whitney"]]
   [:spacer 2]
   [:paragraph [:phrase {:color [180 180 180]} "Number:  "] [:phrase "1"] [:spacer]]
   [:spacer 5]
   [:paragraph.centered "Department of Computer Science" [:spacer]]
   [:paragraph.centered "Fall 2014" [:spacer]]
   [:paragraph.centered "Master Exam" [:spacer]]
   [:spacer 5]
   [:paragraph.bold "Database Management Systems" [:spacer]]
   [:spacer 5]
   [:paragraph.centered "Saturday, August 23, 2014" [:spacer]]
   [:paragraph.centered "8:00-10:00 AM" [:spacer]]
   [:spacer 3]
   [:paragraph.centered "GMCS 214" [:spacer]]
   [:spacer 5]
   [:paragraph.strong "DO NOT WRITE YOUR NAME" [:spacer 2]]
   [:paragraph.strong "ON ANY PAGE OF THE EXAM" [:spacer]]
   ]
  "doc.pdf")