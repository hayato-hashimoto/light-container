(define debug-table (make-hash-table))
(define (debug-mark a)
  (if (hash-table-exists? debug-table a)
    (update! (~ debug-table a) (pa$ + 1))
    (set! (~ debug-table a) 1)))
(define-macro (debug-if a b)
  `(if (and (hash-table-exists? debug-table ,a) (not (= 0 (~ debug-table ,a))))
     (begin
       (debug-print ,b)
       (update! (~ debug-table ,a) (pa$ + -1))
       ,b)
     ,b))

(define-class <style-rule> ()
  ((specificity :init-keyword :specificity)
   (rule :init-keyword :rule)))

(define-class <box> ()
  ((mom  :init-value #f :init-keyword :mom)
   (kids :init-value '() :init-keyword :kids)
   (frame  :init-value #f)
   (id     :init-keyword :id :init-value #f)
   (class  :init-keyword :class :init-value '())
   (text   :init-keyword :text :init-value #f)
   (style-rules :init-value '())
   (frame  :init-value #f)
   (style  :init-value (make-hash-table))))

(define-class <text-box> (<box>)
  ())

(define-method write-object ((self <box>) port)
  (cond
    ((~ self 'id) (display (format "#<box #~a>" (~ self 'id)) port))
    ((null? (~ self 'class)) (next-method))
    (else (display (format "#<box ~a>" (~ self 'class)) port))))

(define-method write-object ((self <text-box>) port)
  (display (format "#<text-box \"~a\">" (~ self 'text)) port))

(define-class <frame> ()
  ((pos :init-keyword :pos :init-value #(0 0))
   (size :init-value #(0 0))
   (box  :init-value #f :init-keyword :box)
   (mom  :init-value #f :init-keyword :mom)
   (kids :init-value '() :init-keyword :kids)
   (lines :init-value '())
   (style :init-value (make-hash-table) :init-keyword :style)))

(define-class <text-frame> (<frame>)
  ((text :init-keyword :text)))

(define-class <br-frame> (<text-frame>)
  ((text :init-value "")))

(define-class <page-frame> (<frame>)
  ())

(define-class <line-frame> (<frame>)
  ())
  
(define-method write-object ((self <frame>) port)
  (cond
    ((or (not (slot-bound? self 'box)) (not (~ self 'box))) (next-method))
    ((~ self 'box 'id) (display (format "#<frame #~a>" (~ self 'box 'id)) port))
    ((null? (~ self 'box 'class)) (next-method))
    (else (display (format "#<frame ~a>" (~ self 'box 'class)) port))))

(define-method write-object ((self <text-frame>) port)
  (display (format "#<text-frame \"~a\">" (~ self 'text)) port))

(define-method write-object ((self <br-frame>) port)
  (display "#<br-frame>" port))

(define (dump-tree tree)
  (define (loop node nest)
    (dotimes (i nest) (display " "))
    (print node) 
    (for-each (cut loop <> (+ nest 1)) (~ node 'kids)))
  (loop tree 0))

(define (split-frame f ptr k)
  (let loop ((i 1))
    (let ((subk (make <text-frame> :text (string-copy (~ k 'text) 0 i) :style (~ k 'style))))
      (meter% subk)
      (if (move-pointer-dry-run f ptr (~ subk 'size) subk)
        (- i 1)
        (loop (+ i 1))))))

(define-method flow ((f <text-frame>))
; do nothing
)

(define (debug-message . _) identity)

(define-method move (frame x y)
  (define dx (- x (~ frame 'pos 0)))
  (define dy (- y (~ frame 'pos 1)))
  (let loop ((f frame))
    (set! (~ f 'pos) (vector (+ (~ f 'pos 0) dx) (+ (~ f 'pos 1) dy)))
    (for-each loop (~ f 'kids))))

(define (get-axis direction)
  (case direction
      ((horizontal) 0)
      ((vertical page) 1)))

(define-method flow ((f <frame>))
  (define ptr (vector 0 0))
  (define h 0)
  (define w 0)
  (define x #f)
  (define split-position #f)
  (define ptr3 #f)
  (define text #f)
  (define lines '())
  (define current-line '(0))
  (define orientation
    (get-axis (~ f 'style :flow)))
  (define (run-text k)
    (if (vector? (~ k 'style :position))
      (flow k)
      (begin
        (set! ptr3 (vector-copy ptr))
        ; set pointer to the head of frame k
        (case (~ f 'style :flow)
          ((vertical page)   (rel-move-to ptr 0 (~ k 'style :margin-top))
                        (set! (~ k 'pos) (map-to <vector> + (~ f 'pos) (vector (+ (~ ptr 0) (~ k 'style :margin-left)) (~ ptr 1)))))
          ((horizontal) (rel-move-to ptr (~ k 'style :margin-left) 0)
                        (set! (~ k 'pos) (map-to <vector> + (~ f 'pos) (vector (~ ptr 0) (+ (~ ptr 1) (~ k 'style :margin-top)))))))
        ; flow the content of frame k, and determine the size of k
        (when (not (is-a? k <text-frame>))
          (if (not (eq? 'horizontal (~ k 'style :scroll)))
            (set! (~ k 'style :max-size 0)
              (min
                (~ k 'style :max-size 0)
                (- (~ f 'style :max-size 0) (~ k 'style :margin-left) (~ k 'style :margin-right)))))
          (if (not (eq? 'vertical (~ k 'style :scroll)))
            (set! (~ k 'style :max-size 1)
              (min
                (~ k 'style :max-size 1)
                (- (~ f 'style :max-size 1) (~ k 'style :margin-top) (~ k 'style :margin-bottom))))))
        (when (~ k 'style :flow) (flow k))
        ; run inline frames
        (when (and (not (is-a? k <text-frame>))
                   (not (~ k 'style :flow)))
          (for-each run-text (~ k 'kids)))
        ; set pointer to the tail of frame k
        (case (~ f 'style :flow)
          ((horizontal)
            (rel-move-to ptr (+ (~ k 'style :margin-right) (~ k 'style :padding-right) (~ k 'size 0)) 0)
            (set! w (max w (min (~ ptr 0) (~ f 'style :max-size 0))))
            (set! h (max h (+ (~ k 'style :margin-top) (~ k 'size 1) (~ k 'style :margin-bottom))))
            (push! current-line k))
          ((vertical page)
            (rel-move-to ptr 0 (+ (~ k 'style :margin-bottom) (~ k 'style :padding-bottom) (~ k 'size 1)))
            (set! w (max w (min (~ ptr 1) (~ f 'style :max-size 1))))
            (set! h (max h (+ (~ k 'style :margin-left) (~ k 'size 0) (~ k 'style :margin-right))))
            (push! current-line k)))
        (let1 continue-to-next-line?
          (and
            (or (is-a? k <br-frame>)
              (if (eq? (~ f 'style :flow) 'page)
                (or (>= (~ ptr 0) (~ f 'style :max-size 0))
                    (>= (~ ptr 1) (~ f 'style :max-size 1)))
                (>= (~ ptr orientation) (~ f 'style :max-size orientation))))
            (case (~ f 'style :flow)
              ((horizontal)
                (push! lines (reverse current-line))
                (set! current-line (list (~ ptr 0)))
                (set! (~ ptr 0) 0)
                (rel-move-to ptr 0 (* h (~ f 'style :line-height)))
                (and (is-a? k <text-frame>) (not (is-a? k <br-frame>))))
              ((page)
                (set! ptr (vector 0 0))
                (set! (~ k 'style :page-break-after) #t)
                (and (is-a? k <text-frame>) (not (is-a? k <br-frame>))))
              ((vertical)
                (push! lines (reverse current-line))
                (set! current-line (list (~ ptr 1)))
                (set! (~ ptr 1) 0)
                (rel-move-to ptr (* h (~ f 'style :line-height)) 0)
                (and (is-a? k <text-frame>) (not (is-a? k <br-frame>))))))

          (when continue-to-next-line?
            (set! split-position (split-frame f ptr3 k))
            (set! h 0)
            (set! text (~ k 'text))
            (set! (~ k 'text)
              (string-copy (~ k 'text) 0 split-position))
            (meter% k)
            (set! x
              (make <text-frame>
                :box (~ k 'box)
                :style (~ k 'style)
                :kids '()
                :mom f
                :text (string-copy text split-position)))
            (meter% x)
            (push! (~ f 'kids) x)
            (case (~ f 'style :flow)
              ((vertical)   (rel-move-to ptr 0 (~ k 'style :margin-top)))
              ((horizontal) (rel-move-to ptr (~ k 'style :margin-left) 0)))
            (case (~ f 'style :flow)
              ((vertical)   (set! (~ x 'pos)
                              (map-to <vector> +
                                (~ f 'pos)
                                (vector (+ (~ ptr 0) (~ k 'style :margin-left)) (~ ptr 1)))) 
                            (set! h
                              (max h
                                (+ (~ k 'style :margin-left) (~ k 'size 0) (~ k 'style :margin-right)))))
              ((horizontal) (set! (~ x 'pos)
                              (map-to <vector> +
                                (~ f 'pos)
                                (vector (~ ptr 0) (+ (~ ptr 1) (~ k 'style :margin-top)))))
                            (set! h
                              (max h
                                (+ (~ k 'style :margin-top) (~ k 'size 1) (~ k 'style :margin-bottom))))))
            (run-text x))))))
     
      (rel-move-to ptr (~ f 'style :padding-left) (~ f 'style :padding-top))

    ;#?=((debug-message "entering flow ...") f)
     
    (when (vector? (~ f 'style :position)) (set! (~ f 'pos) (~ f 'style :position)))
    (for-each run-text (~ f 'kids))
    (case (~ f 'style :flow) 
      ((vertical page)
         (set! (~ f 'lines) (reverse lines))
         (rel-move-to ptr (* h (~ f 'style :line-height)) 0))
      ((horizontal)
         (set! (~ f 'lines) (reverse lines))
         (rel-move-to ptr 0 (* h (~ f 'style :line-height)))))
    (set! (~ f 'size) (debug-if 'test (vector
      (max
        (case (~ f 'style :flow)
          ((vertical page) (+ (~ ptr 0) (~ f 'style :padding-right)))
          ((horizontal) (+ (~ f 'style :padding-left) w (~ f 'style :padding-right))))
        (~ f 'style :min-size 0))
      (max
        (case (~ f 'style :flow)
          ((vertical page) (+ (~ f 'style :padding-top) w (~ f 'style :padding-bottom)))
          ((horizontal) (+ (~ ptr 1) (~ f 'style :padding-bottom))))
        (~ f 'style :min-size 1)))))
    ;#?=((debug-message "exiting flow ...") f)
)

(define (move-pointer-dry-run f ptr v m)
  (define orientation
    (get-axis (~ f 'style :flow)))
  (define ptr2 (vector-copy ptr))
  (case (~ f 'style :flow)
    ((horizontal) (rel-move-to ptr2 (+ (~ m 'style :margin-right) (~ v 0)) 0))
    ((vertical page)   (rel-move-to ptr2 0 (+ (~ m 'style :margin-bottom) (~ v 1)))))
  (if (eq? (~ f 'style :flow) 'page)
    (or (>= (~ ptr2 0) (~ f 'style :max-size 0))
        (>= (~ ptr2 1) (~ f 'style :max-size 1)))
    (>= (~ ptr2 orientation) (~ f 'style :max-size orientation))))

(define (func-or . a)
  (if (null? a) #f (or (car a) (apply func-or (cdr a)))))

(define-class <flow-context> ()
  ((pos :init-value #(0 0))))

(define (rel-move-to v x y)
  (update! (~ v 0) (pa$ + x))
  (update! (~ v 1) (pa$ + y)))
   
(define-class <style-props> ()
  ((spec :init-keyword :spec)
   (prop :init-keyword :prop)
   (rule :init-keyword :rule)))
