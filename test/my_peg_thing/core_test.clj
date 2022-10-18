(ns my-peg-thing.core-test
  (:require [clojure.test :refer :all]
            [my-peg-thing.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest tri-test
  (testing "Tri function"
    (is (= (take 2 tri) '(1 3)))))

(deftest triangular?-test
  (testing "Validate 5 is not a triangular number"
    (is (= (triangular? 5) false)))
  (testing "Validate 6 is a triangular number"
    (is (= (triangular? 6) true))))

(deftest test-row-tri
  (testing "Get the triangular number at the end of row n"
    (are [x y] (= (row-tri x) y)
          1 1
          2 3
          3 6)))

(deftest test-row-number
  (testing "Returns row number"
    (are [x y] (= (row-num x) y)
          1 1
          5 3)))

(def connections1and4 
  {1 {:connections {4 2}}
   4 {:connections {1 2}}})

(deftest test-connect
  (testing "Form a mutual connection between two positions"
    (is (= (connect {} 15 1 2 4) connections1and4))))

(def connection-for-1
  {1 {:connections {4 2}}
   4 {:connections {1 2}}})

(def connection-for-3
  {3 {:connections {10 6}}
   10 {:connections {3 6}}})

(def connection-for-4
  {4 {:connections {6 5}}
   6 {:connections {4 5}}})

(deftest test-connect-down-left
  (testing "Connect position to destination down left"
    (is (= (connect-down-left {} 15 1) connection-for-1))))

(deftest test-connect-down-right
  (testing "Connect position to destination down right"
    (is (= (connect-down-right {} 15 3) connection-for-3))))

(deftest test-connect-right
  (testing "Connect position"
    (is (= (connect-right {} 15 4) connection-for-4))))

(def board-pos1
  { 1 {:connections {6 3, 4 2}, :pegged true}
    4 {:connections {1 2}}
    6 {:connections {1 3}}})

(def board-pos4
  { 4 {:connections {13 8, 11 7, 6 5}, :pegged true}
    13 {:connections {4 8}}
    11 {:connections {4 7}}
    6 {:connections {4 5}}})

(def board-pos6
  { 6 {:connections {15 10, 13 9}, :pegged true}
   15 {:connections {6 10}}
   13 {:connections {6 9}}})

(deftest test-add-pos
  (testing "Add peg in a position"
    (is (= (add-pos {} 15 1) board-pos1))
    (is (= (add-pos {} 15 4) board-pos4))
    (is (= (add-pos {} 15 6) board-pos6))))


(def board {1  {:pegged true, :connections {6 3, 4 2}},
            2  {:pegged true, :connections {9 5, 7 4}},
            3  {:pegged true, :connections {10 6, 8 5}},
            4  {:pegged true, :connections {13 8, 11 7, 6 5, 1 2}},
            5  {:pegged true, :connections {14 9, 12 8}},
            6  {:pegged true, :connections {15 10, 13 9, 4 5, 1 3}},
            7  {:pegged true, :connections {9 8, 2 4}},
            8  {:pegged true, :connections {10 9, 3 5}},
            9  {:pegged true, :connections {7 8, 2 5}},
            10 {:pegged true, :connections {8 9, 3 6}},
            11 {:pegged true, :connections {13 12, 4 7}},
            12 {:pegged true, :connections {14 13, 5 8}},
            13 {:pegged true, :connections {15 14, 11 12, 6 9, 4 8}},
            14 {:pegged true, :connections {12 13, 5 9}},
            15 {:pegged true, :connections {13 14, 6 10}},
            :rows 5})

(deftest test-new-board
  (testing "Create a board"
    (is (= (new-board 5) board))))

(deftest test-pegged?
  (testing "Is pegged?"
    (is (pegged? board 1))))

(deftest test-remove-peg
  (testing "remove a peg"
    (let [board-removed (remove-peg board 1)]
      (is (not (pegged? board-removed 1)))
      (is (pegged? board-removed 2)))))


(deftest test-place-peg
  (testing "Place a peg"
    (let [board-removed (remove-peg board 1)
          board-placed (place-peg board-removed 1)]
      (is (pegged? board-placed 1)))))


(deftest test-move-peg
  (testing "move-peg"
    (let [board-moved (move-peg board 1 3)]
      (is (pegged? board-moved 3))
      (is (not (pegged? board-moved 1))))))



(def board-valid-for-13 {1  {:pegged true, :connections {6 3, 4 2}},
            2  {:pegged true, :connections {9 5, 7 4}},
            3  {:pegged true, :connections {10 6, 8 5}},
            4  {:pegged true, :connections {13 8, 11 7, 6 5, 1 2}},
            5  {:pegged true, :connections {14 9, 12 8}},
            6  {:pegged false, :connections {15 10, 13 9, 4 5, 1 3}},
            7  {:pegged true, :connections {9 8, 2 4}},
            8  {:pegged true, :connections {10 9, 3 5}},
            9  {:pegged true, :connections {7 8, 2 5}},
            10 {:pegged true, :connections {8 9, 3 6}},
            11 {:pegged false, :connections {13 12, 4 7}},
            12 {:pegged true, :connections {14 13, 5 8}},
            13 {:pegged true, :connections {15 14, 11 12, 6 9, 4 8}},
            14 {:pegged true, :connections {12 13, 5 9}},
            15 {:pegged false, :connections {13 14, 6 10}},
            :rows 5})

(def board-end-game {1  {:pegged false, :connections {6 3, 4 2}},
                         2  {:pegged false, :connections {9 5, 7 4}},
                         3  {:pegged false, :connections {10 6, 8 5}},
                         4  {:pegged false, :connections {13 8, 11 7, 6 5, 1 2}},
                         5  {:pegged false, :connections {14 9, 12 8}},
                         6  {:pegged false, :connections {15 10, 13 9, 4 5, 1 3}},
                         7  {:pegged false, :connections {9 8, 2 4}},
                         8  {:pegged false, :connections {10 9, 3 5}},
                         9  {:pegged false, :connections {7 8, 2 5}},
                         10 {:pegged true, :connections {8 9, 3 6}},
                         11 {:pegged false, :connections {13 12, 4 7}},
                         12 {:pegged false, :connections {14 13, 5 8}},
                         13 {:pegged false, :connections {15 14, 11 12, 6 9, 4 8}},
                         14 {:pegged false, :connections {12 13, 5 9}},
                         15 {:pegged false, :connections {13 14, 6 10}},
                         :rows 5})

(deftest test-valid-moves
  (let [moves (valid-moves board-valid-for-13 13)]
    (is (= moves {15 14, 11 12, 6 9}))))

(deftest test-valid-move?
  (is (= (valid-move? board-valid-for-13 13 11) 12))
  (is (= (valid-move? board-valid-for-13 13 10) nil)))

(deftest test-make-move
  (testing "test to make move from 6 to 4, jumped 5"
    (let [my-board (assoc-in (new-board 5) [4 :pegged] false)
          board-moved (make-move my-board 6 4)]
      (is (not (pegged? board-moved 5)))
      (is (not (pegged? board-moved 6)))
      (is (pegged? board-moved 4)))))


(deftest test-can-move?
  (testing "test if I can move to continue the game"
    (is (= (can-move? board-valid-for-13) {6 3}))
    (is (= (can-move? board-end-game) nil))))

