;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |5001 _ Team2 |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 2.

;; A BTN is one of 
;; - 'leaf
;; - (make-node Number BTN BTN)

;;; Template
;; btn-fn: BTN -> ???
#;(define (btn-fn bt)
    (cond
      [(symbol? bt)  ...]
      [(node? bt)  ... (node-val bt) ...
                   ... (btn-fn (node-left bt)) ...
                   ... (btn-fn (node-right bt)) ...]))

;; A BTS is one of 
;; - 'leaf
;; - (make-node String BTS BTS)

;;; Template
;; bts-fn: BTS -> ???
#;(define (bts-fn bt)
    (cond
      [(symbol? bt)  ...]
      [(node? bt)  ... (node-val bt) ...
                   ... (bts-fn (node-left bt)) ...
                   ... (bts-fn (node-right bt)) ...]))


(define-struct node (val left right))
;; INTERP: represents a node with a value
;; a left and right subtree

;;; Template
;; node-fn: Node -> ???
#; (define (node-fn a-node)
       ... (node-val a-node) ...
       ... (node-left a-node) ...
       ... (node-right a-node) ...)


;; 1.
;;; Signature
;; tree-sum: BTN -> Num

;;; Purpose
;; Given a BTN and returns the sum of all numbers in BTN

;;; Function
(define (tree-sum btn)
  (cond
    [(symbol? btn)  0]
    [(node? btn)  (+ (node-val btn) 
                    (tree-sum (node-left btn)) 
                    (tree-sum (node-right btn)))]))
;;;Test
(check-expect (tree-sum 'leaf) 0)
(check-expect (tree-sum (make-node 5
                                   (make-node 7
                                              (make-node 8 'leaf 'leaf)
                                              (make-node 9 'leaf 'leaf))
                                   'leaf))
              29)


;; 2.
;;; Signature
;; tree-prod: BTN -> Num

;;; Purpose
;; Given a BTN and returns the product of all numbers in BTN
                    
;;; Function
(define (tree-prod btn)
  (cond
    [(symbol? btn) 1]
    [(node? btn)  (* (node-val btn) 
                    (tree-prod (node-left btn)) 
                    (tree-prod (node-right btn)))]))
;;;Test
(check-expect (tree-prod 'leaf) 1)
(check-expect (tree-prod (make-node 1
                                    (make-node 1
                                               (make-node 2 'leaf 'leaf)
                                               (make-node 3 (make-node 5 'leaf 'leaf)
                                                          'leaf))
                                    'leaf))
              30)

;; 3.
;;; Signature
;; tree-append: BTS -> String

;;; Purpose
;; Given a BTS and returns the concatenation of all strings in BTS

;;; Function
(define (tree-append bts)
  (cond
    [(symbol? bts) ""]
    [(node? bts)  (string-append (node-val bts)
                                (tree-append (node-left bts))
                                (tree-append (node-right bts)))]))
;;;Test                   
(check-expect (tree-append 'leaf) "")
(check-expect (tree-append (make-node "Apple"
                                      (make-node " and" 'left 'left)
                                      (make-node " Banana"
                                                 (make-node " and" 'left 'left)
                                                 (make-node " Pineapple" 'left 'left))))
              "Apple and Banana and Pineapple")
                                                 
                                      


;; 4.
;; BT[X] is one of 
;; - 'leaf
;; - (make-node X BT[X] BT[X])

#;(define (BT-fn BT[X])
    (cond
      [(symbol? BT[X])  ...]
      [(node? BT[X])  ... (node-X BT[X]) ...
                      ... (BT-fn (node-left BT[X])) ...
                      ... (BT-fn (node-right BT[X])) ...]))


;;; Signature
;; tree-op-generic: op BT[X] -> Any

;;; Purpose
;; Given a BT[X], an accumlator and operation of sum, product and string-append,
;; return the result

;;; Function
(define (tree-op-generic op acc bt)
  (cond
    [(symbol? bt) acc]
    [(node? bt) (op (node-val bt)
                    (tree-op-generic op acc (node-left bt))
                    (tree-op-generic op acc (node-right bt)))]))
    

;;;Test
(check-expect (tree-op-generic + 0
                               (make-node 5
                                          (make-node 7
                                                     (make-node 8 'leaf 'leaf)
                                                     (make-node 9 'leaf 'leaf))
                                          'leaf))
              (tree-sum (make-node 5
                                   (make-node 7
                                              (make-node 8 'leaf 'leaf)
                                              (make-node 9 'leaf 'leaf))
                                   'leaf)))
              
(check-expect (tree-op-generic * 1
                               (make-node 1
                                          (make-node 1
                                                     (make-node 2 'leaf 'leaf)
                                                     (make-node 3 (make-node 5 'leaf 'leaf)
                                                                  'leaf))
                                          'leaf))
              (tree-prod (make-node 1
                                    (make-node 1
                                               (make-node 2 'leaf 'leaf)
                                               (make-node 3 (make-node 5 'leaf 'leaf)
                                                           'leaf))
                                    'leaf)))

(check-expect (tree-op-generic string-append ""
                               (make-node "Apple"
                                          (make-node " and" 'left 'left)
                                          (make-node " Banana"
                                                     (make-node " and" 'left 'left)
                                                     (make-node " Pineapple" 'left 'left))))
              
              (tree-append (make-node "Apple"
                                      (make-node " and" 'left 'left)
                                      (make-node " Banana"
                                                 (make-node " and" 'left 'left)
                                                 (make-node " Pineapple" 'left 'left)))))








