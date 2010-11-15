(ns scapegoat.test.core
  (:use [scapegoat.core] :reload-all)
  (:use [clojure.test]))

;; Alpha value used in Galperin Scapegoat paper
(def galperin-alpha 0.57)

;; Example initial tree from paper
(def galperin-before-insert
     [2 [1 nil nil]
      [6
       [5 [4 [3 nil nil] nil] nil]
       [15
        [12 [9 [7 nil nil] [11 [10 nil nil] nil]] [13 nil [14 nil nil]]]
        [16 nil [17 nil [18 nil nil]]]]]])

;; Example after insert of 8 causing a rebuild at the scapegoat of 6
(def galperin-after-insert
     [2 [1 nil nil]
      [11
       [7 [5 [4 [3 nil nil] nil] [6 nil nil]] [9 [8 nil nil] [10 nil nil]]]
       [15 [13 [12 nil nil] [14 nil nil]] [17 [16 nil nil] [18 nil nil]]]]])

(def simple-tree
     [10
      [8 [5 [4 nil nil] nil] [9 nil nil]]
      [20 [15 nil [17 nil nil]] [25 nil nil]]])

;; 10 has two children and is the root
(def simple-tree-del-10
     [15
      [8 [5 [4 nil nil] nil] [9 nil nil]]
      [20 [17 nil nil] [25 nil nil]]])

;; 25 has no children
(def simple-tree-del-25
     [10
      [8 [5 [4 nil nil] nil] [9 nil nil]]
      [20 [15 nil [17 nil nil]] nil]])

;; 15 has only a right child
(def simple-tree-del-15
     [10
      [8 [5 [4 nil nil] nil] [9 nil nil]]
      [20 [17 nil nil] [25 nil nil]]])

;; 5 has only a left child
(def simple-tree-del-5
     [10
      [8 [4 nil nil] [9 nil nil]]
      [20 [15 nil [17 nil nil]] [25 nil nil]]])

;; Deleted 8, 25, 10, 4.  Should cause rebalance
(def simple-tree-rebalanced
     [15 [9 [5 nil nil] nil] [20 [17 nil nil] nil]])

(defn vector->node
  "Create a node from a nested vector"
  [new-node-fn vector]
  (if vector
    (let [[key a2 a3 a4]     vector
          left               (if a4 a3 a2)
          right              (if a4 a4 a3)
          value              (if a4 a2 true)
          [left-size  left]  (vector->node new-node-fn left)
          [right-size right] (vector->node new-node-fn right)]
      [(+ 1 left-size right-size)
       (new-node-fn key value left right)])
    [0 nil]))

(defn vector->tree
  "Create a new tree from a nested vector"
  [mutable alpha vector]
  (let [[size root] (vector->node
                     (if mutable new-mutable-node new-immutable-node)
                     vector)]
    ((if mutable new-mutable-tree new-tree) alpha root size size)))

(defn are-nodes-equal
  "Test whether two given nodes and all their children have the same keys, values,
   and structure"
  [node1 node2]
  ;; key and value of both children should be equal
  (is (= (node-key node1) (node-key node2)))
  (is (= (node-value node1) (node-value node2)))
  ;; nilness of both children should be equal
  (is (= (nil? (left-child node1)) (nil? (left-child node2))))
  (is (= (nil? (right-child node1)) (nil? (right-child node2))))
  (if (and (left-child node1) (left-child node2))
    (are-nodes-equal (left-child node1) (left-child node2)))
  (if (and (right-child node1) (right-child node2))
    (are-nodes-equal (right-child node1) (right-child node2))))

(defn are-trees-equal
  "Tests equality of tree keys, values, and structure. Does NOT test
   the value of the tree's size, max-size, or alpha attributes."
  [tree1 tree2]
  (are-nodes-equal (root-node tree1) (root-node tree2)))

(defn is-valid-bst-node
  "Tests whether a node is valid according to the BST property"
  [node]
  (let [key (node-key node)]
    (when-let [left (left-child node)]
      (is (<= (node-key left) key))
      (is-valid-bst-node left))
    (when-let [right (right-child node)]
      (is (>= (node-key right) key))
      (is-valid-bst-node right))))

(defn is-valid-bst
  "Tests whether a tree is valid according to the BST property"
  [tree]
  (is-valid-bst-node (root-node tree)))

(defmacro deftest-binding-set
  "Define vector of test set bindings in the following format:
   (deftest-set-bindings set
     [name1 bindings1,
      name2 bindings2,
      ...])"
  [name bindings]
  `(def ~name '~bindings))

(defmacro deftests-for-binding-set
  "Generates deftests for each set of bindings from deftest-set-bindings
   For example, with the following input:
     (deftests-for-set set bar
       (testing ...))
   The result is essentially:
   (let bindings1
     (deftest set-name1-bar
       (testing ...)))
   (let bindings2
     (deftest set-name2-bar
       (testing ...)))"
  [set-bindings test-name & body]
  `(do
     ~@(map (fn [[binding-name bindings]]
              `(deftest ~(symbol (str binding-name "-" test-name))
                 (let ~bindings
                   ~@body)))
            (partition 2 @(resolve set-bindings)))))

(deftest-binding-set mutability
  [immutable [mutable  false
              new-tree new-tree]
   mutable   [mutable  true
              new-tree new-mutable-tree]])

(deftests-for-binding-set mutability new-tree
  (testing "Initial values"
    (let [tree (new-tree galperin-alpha)]
      (is (= (max-size tree) (tree-size tree) 0))
      (is (= (root-node tree) nil))
      (is (= (tree-alpha tree) galperin-alpha)))))

(deftests-for-binding-set mutability insert
  (testing "Insert, delete, insert should set the root to last inserted node"
    (let [tree (-> (new-tree galperin-alpha)
                   (insert 80 :foo)
                   (delete 80)
                   (insert 9 :bar))
          root (root-node tree)]
      (is (= (node-key root) 9))
      (is (= (node-value root) :bar))))
  (testing "Rebalancing after insert of 8 in example from Galperin paper"
    (let [expected (vector->tree mutable galperin-alpha galperin-after-insert)
          actual   (-> (vector->tree mutable galperin-alpha
                                     galperin-before-insert)
                       (insert 8 true))]
      (is-valid-bst expected)
      (is-valid-bst actual)
      (are-trees-equal expected actual))))

(deftests-for-binding-set mutability delete
  (testing "BST deletion with two children"
    (let [expected (vector->tree mutable galperin-alpha simple-tree-del-10)
          actual   (-> (vector->tree mutable galperin-alpha simple-tree)
                       (delete-key 10 true))]
      (is-valid-bst expected)
      (is-valid-bst actual)
      (are-trees-equal expected actual)))
  (testing "BST deletion with one child on the left"
    (let [expected (vector->tree mutable galperin-alpha simple-tree-del-5)
          actual   (-> (vector->tree mutable galperin-alpha simple-tree)
                       (delete-key 5 true))]
      (is-valid-bst expected)
      (is-valid-bst actual)
      (are-trees-equal expected actual)))
  (testing "BST deletion with one child on the right"
    (let [expected (vector->tree mutable galperin-alpha simple-tree-del-15)
          actual   (-> (vector->tree mutable galperin-alpha simple-tree)
                       (delete-key 15 true))]
      (is-valid-bst expected)
      (is-valid-bst actual)
      (are-trees-equal expected actual)))
  (testing "BST deletion with no children"
    (let [expected (vector->tree mutable galperin-alpha simple-tree-del-25)
          actual   (-> (vector->tree mutable galperin-alpha simple-tree)
                       (delete-key 25 true))]
      (is-valid-bst expected)
      (is-valid-bst actual)
      (are-trees-equal expected actual)))
  (testing "Update of size but not max-size upon deletion when there
            is no rebalance"
    (let [tree (vector->tree mutable galperin-alpha simple-tree)
          size (tree-size tree)
          ms   (max-size tree)
          del  (delete-key tree 10 true)]
      (is (= (dec size) (tree-size del)))
      (is (= ms (max-size del)))))
  (testing "Rebalance after delete when needed, updating size and
            setting size = max-size"
    (let [expected (vector->tree mutable galperin-alpha simple-tree-rebalanced)
          actual   (-> (vector->tree mutable galperin-alpha simple-tree)
                       (delete-key 8 true)
                       (delete-key 25 true)
                       (delete-key 10 true)
                       (delete-key 4 true))]
      ;; size should equal max-size since the tree was just rebalanaced
      (is (= (max-size expected) (tree-size expected)
             (max-size actual) (tree-size actual)
             5))
      (is-valid-bst expected)
      (is-valid-bst actual)
      (are-trees-equal expected actual))))
