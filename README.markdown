#Overview

Scapegoat is an implementation of the Scapegoat Tree, a self balancing binary
tree with worst-case O(log n) lookup time and O(log n) amortized insertion and
deletion time.

See [the Wikipedia page](http://en.wikipedia.org/wiki/Scapegoat_tree) for more
details.

#Implementation

This implementation uses a zipper in order to traverse the tree and make
updates.  The result is an immutable tree with shared structure between updates.

#Usage

    (use 'scapegoat.core)

    (-> (new-tree 0.707) ;; where 0.707 is the alpha balance of the tree
        (insert 5 :v1)   ;; where 5 is the key and :v1 is the value
        (insert 10 :v2)
        (delete 5)       ;; where 5 is a key in the tree
        (search 10))     ;; returns :v1

