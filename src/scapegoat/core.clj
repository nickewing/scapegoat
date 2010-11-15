(ns scapegoat.core
  (:require [clojure.zip :as zip]
            [clojure.contrib.string :as str]))

(defprotocol Tree
  (tree-alpha  [this] "Return the alpha value for the tree")
  (max-size    [this] "Return the max node size for the tree")
  (root-node   [this] "Return the node at the root tree")
  (tree-size   [this] "Return the total size of the tree")
  (alter-tree  [this root size ms] "Alter a tree's attributes")
  (new-node    [this key val left right] "Create a new node for the tree")
  (delete-key  [this key balanced] "Delete a key from the tree")
  (insert-node [this node balanced] "Insert a node into the tree"))

(defprotocol Node
  (node-key    [this]"Return the key of the node")
  (node-value  [this] "Return the value of the node")
  (left-child  [this] "Return the left child of the node")
  (right-child [this] "Return the right child of the node")
  (alter-node  [this k v left right] "Alter a node returning another node"))

;; DummyNode used required for rebuild-tree
(deftype
  ^{:private true}
  DummyNode [left right]
  Node
  (node-key    [this] nil)
  (node-value  [this] nil)
  (left-child  [this] (.left this))
  (right-child [this] (.right this))
  (alter-node  [this k v l r] (DummyNode. l r)))

(defn- alter-left
  "Alter the left child of a node"
  [node left]
  (alter-node node (node-key node) (node-value node) left (right-child node)))

(defn- alter-right
  "Alter the right child of a node"
  [node right]
  (alter-node node (node-key node) (node-value node) (left-child node) right))

(defn- node-zipper
  "Create a zipper for a tree node"
  [node]
  (zip/zipper identity
              #(seq [(left-child %) (right-child %)])
              (fn [node [left right]]
                (alter-node node
                            (node-key node)
                            (node-value node)
                            left
                            right))
              node))

(defn- search-node
  "Do a binary search on the tree"
  [node key]
  (cond (nil? node)             nil
        (< key (node-key node)) (recur (left-child node) key)
        (> key (node-key node)) (recur (right-child node) key)
        true                    (node-value node)))

(defn search
  "Search a tree for a key"
  [tree key]
  (search-node (root-node tree) key))

(defn- node-size
  "Recursively compute the size of a node"
  [node]
  (if node
    (+ 1 (node-size (left-child node)) (node-size (right-child node)))
    0))

(defn- print-node
  "Recursively print a tree downwards from the given node"
  [node d side]
  (when node
    (print (str/repeat (* d 2) " ")
           side (node-key node)
           (node-value node))
    (println)
    (print-node (left-child node)  (inc d) "L")
    (print-node (right-child node) (inc d) "R")
    node))

(defn print-tree
  "Print a tree starting at its root"
  [tree]
  (println (str "Tree(size: " (tree-size tree)
                ", max-size: " (max-size tree) "):"))
  (if-let [root (root-node tree)]
    (print-node root 1 "Root")
    (println "<Empty tree>"))
  tree)

(defn- flatten-node
  "Flatten node and all its children into a chain of nodes where all
   nodes are in the right child of the orignal node.  All left children
   in the returned list are garbage"
  [x y]
  (if x
    (let [x (alter-right x (flatten-node (right-child x) y))]
      (recur (left-child x) x))
    y))

(defn- build-tree
  "Recursively build a flattened tree chain into a new
   1/2-weight-balanced tree"
  [n x]
  (if (= n 0)
    (alter-left x nil)
    (let [r (build-tree (Math/ceil  (/ (- n 1) 2)) x)
          s (build-tree (Math/floor (/ (- n 1) 2)) (right-child r))
          r (alter-right r (left-child s))
          s (alter-left s r)]
      s)))

(defn- rebuild-tree-from-node
  "Rebuild a node and all its children into a 1/2-weight-balanced tree"
  [n balance-node]
  (left-child (build-tree n (flatten-node balance-node (DummyNode. nil nil)))))

(defn- height-alpha
  "Calculate h_a(n) as defined by Galperin: h_a(n) = floor(log_(1/a)(n))"
  [alpha n]
  (int (Math/floor (/ (Math/log n) (Math/log (/ 1 alpha))))))

(defn- rebalance-on-scapegoat
  "Do an upward search for a scapegoat from the given node.  When one is found,
   rebalance off of it.  A node is deemed a scapegoat if
   i > height-alpha(size(x_i)), where i is the number of levels moved upward
   from the inserted node, x_0, and x_i is the ancestor i-levels up from x_0."
  [alpha pos]
  (loop [x0 pos size0 1 i0 0] ;; size(x0) = 1, since we know it's a leaf
    (let [node0 (zip/node x0)
          i1    (inc i0)
          x1    (zip/up x0)
          node1 (zip/node x1)
          size1 (if (= (left-child node1) node0)
                  (+ 1 size0 (node-size (right-child node1)))
                  (+ 1 size0 (node-size (left-child node1))))]
      (if (> i1 (height-alpha alpha size1))
        (zip/replace x1 (rebuild-tree-from-node size1 node1))
        (recur x1 size1 i1)))))

(defn- balanced-insert-node
  "Insert into tree and rebalance if needed"
  [alpha dd pos new-node depth]
  (if (zip/node pos)
    (if (< (node-key new-node) (-> pos zip/node node-key))
      (recur alpha dd (zip/down pos) new-node (inc depth))
      (recur alpha dd (-> pos zip/down zip/right) new-node (inc depth)))
    (zip/root
     (let [pos (zip/replace pos new-node)]
       ;; (println (str "depth: " depth " dd: " dd))
       (if (> depth dd)
         (rebalance-on-scapegoat alpha pos)
         pos)))))

(defn- standard-insert-node
  "Recurse into tree and insert new node"
  [pos new-node]
  (if (zip/node pos)
    (if (< (node-key new-node) (-> pos zip/node node-key))
      (recur (zip/down pos) new-node)
      (recur (-> pos zip/down zip/right) new-node))
    (zip/root (zip/replace pos new-node))))

(defn- insert-node-into-tree
  "Insert a node into a tree and rebalance if needed"
  [tree new-node & [balanced]]
  (if-let [root (root-node tree)]
    (let [size   (+ 1 (tree-size tree))
          zipper (node-zipper root)]
      (alter-tree
       tree
       (if-not (= balanced false)
         (let [alpha (tree-alpha tree)
               dd    (height-alpha alpha size)]
           (balanced-insert-node alpha dd zipper new-node 0))
         (standard-insert-node zipper new-node))
       size
       (max (max-size tree) size)))
    (alter-tree tree new-node 1 1)))

(defn insert
  "Insert a key-value pair into a tree"
  [tree key value]
  (insert-node tree (new-node tree key value nil nil) true))

(defn- find-min-pos
  "Find the min from a given node"
  [pos d]
  (if (-> pos zip/down zip/node)
    (recur (zip/down pos) (inc d))
    [pos d]))

(defn- apply-n-times
  "Repeadedly apply f, n times first with the argument of x then
   successively with the result of each previous call."
  [n f x]
  (if (= n 0) x (recur (dec n) f (f x))))

(defn- delete-with-no-children
  "Delete current ndoe with no children"
  [pos]
  (zip/replace pos nil))

(defn- delete-with-one-child
  "Delete current node with one child"
  [pos node]
  (if (left-child node)
    (zip/replace pos (left-child node))
    (zip/replace pos (right-child node))))

(defn- delete-with-two-children
  "Delete current node in the case of two children.  Delete successor
   and replace the current node's key and value with the successor key
   and value"
  [pos]
  (let [right-tree (-> pos zip/down zip/right)
        [min-pos min-d] (find-min-pos right-tree 1)
        min-node (zip/node min-pos)
        pos (apply-n-times
             min-d
             zip/up
             (zip/replace min-pos (right-child min-node)))
        node (zip/node pos)]
    (zip/replace pos
                 (alter-node node
                             (node-key min-node)
                             (node-value min-node)
                             (left-child node)
                             (right-child node)))))

(defn- delete-current-node
  "Delete the node at the current zipper position"
  [pos node]
  (zip/root
   (cond
    (and (left-child node) (right-child node))
      (delete-with-two-children pos)
    (or  (left-child node) (right-child node))
      (delete-with-one-child pos node)
    :else
      (delete-with-no-children pos))))

(defn- find-and-delete-key
  "Recurse into tree and delete node with given key"
  [pos delete-key]
  (if-let [node (zip/node pos)]
    (cond
     ;; delete key is less than current key, recurse left
     (< delete-key (node-key node))
       (recur (zip/down pos) delete-key)
     ;; delete key is greater than current key, recurse right
     (> delete-key (node-key node))
       (recur (-> pos zip/down zip/right) delete-key)
     ;; delete key is equal to current key, delete it
     :else
       [true (delete-current-node pos node)])
    [false (zip/root pos)]))

(defn- delete-key-from-tree
  "Delete a node from a tree"
  [tree delete-key & [balanced]]
  (let [zipper         (node-zipper (root-node tree))
        size           (dec (tree-size tree))
        max-size       (max-size tree)
        [success root] (find-and-delete-key zipper delete-key)]
    ;; (println "Rebuild?" size "<" (* alpha max-size))
    (if success
      (if (and (not= balanced false)
               (< size (* (tree-alpha tree) max-size)))
        (alter-tree tree
                    (rebuild-tree-from-node size root)
                    size
                    size)
        (alter-tree tree root size max-size))
      (throw (Exception. "Key does not exist to delete")))))

(defn delete
  "Delete a node from a tree with the given key"
  [tree key]
  (delete-key tree key true))

;; Immutable Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare new-immutable-node)

(deftype ImmutableNode [key value left right]
  Node
  (node-key    [this] (.key this))
  (node-value  [this] (.value this))
  (left-child  [this] (.left this))
  (right-child [this] (.right this))
  (alter-node  [this k v left right]
               (new-immutable-node k v left right)))

(defn new-immutable-node
  [key val left right]
  (ImmutableNode. key val left right))

(deftype ImmutableTree [alpha root size ms]
  Tree
  (tree-alpha  [this] (.alpha this))
  (max-size    [this] (.ms this))
  (root-node   [this] (.root this))
  (tree-size   [this] (.size this))
  (alter-tree  [this root size ms]
               (ImmutableTree. (tree-alpha this) root size ms))
  (new-node    [this key val left right]
               (ImmutableNode. key val left right))
  (delete-key  [this key balanced]
               (delete-key-from-tree this key balanced))
  (insert-node [this node balanced]
               (insert-node-into-tree this node balanced)))

(defn new-tree
  "Create a new tree"
  [alpha & [root size max-size]]
  (ImmutableTree. alpha
                  root
                  (or size     (if root 1 0))
                  (or max-size (if root 1 0))))

;; Mutable Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: the mutable implementation is just an experiment and is often much
;; slower than the immutable version anyway.  If a mutable tree is needed, a
;; ref containing an immutable tree is likely sufficient.

(deftype MutableNode [key value left right]
  Node
  (node-key    [this] @(.key this))
  (node-value  [this] @(.value this))
  (left-child  [this] @(.left this))
  (right-child [this] @(.right this))
  (alter-node  [this k v left right]
               (ref-set (.key  this)  k)
               (ref-set (.value this) v)
               (ref-set (.left  this) left)
               (ref-set (.right this) right)
               this))

(defn new-mutable-node
  [key val left right]
  (MutableNode. (ref key) (ref val) (ref left) (ref right)))

(deftype MutableTree [alpha root size ms]
  Tree
  (tree-alpha  [this] (.alpha this))
  (max-size    [this] @(.ms this))
  (root-node   [this] @(.root this))
  (tree-size   [this] @(.size this))
  (alter-tree  [this root size ms]
               (ref-set (.root this) root)
               (ref-set (.size this) size)
               (ref-set (.ms this) ms)
               this)
  (new-node    [this key val left right]
               (new-mutable-node key val left right))
  (delete-key  [this key balanced]
               (dosync (delete-key-from-tree this key balanced)))
  (insert-node [this node balanced]
               (dosync (insert-node-into-tree this node balanced))))

(defn new-mutable-tree
  "Create a new tree"
  [alpha & [root size max-size]]
  (MutableTree. alpha
                (ref root)
                (ref (or size     (if root 1 0)))
                (ref (or max-size (if root 1 0)))))
