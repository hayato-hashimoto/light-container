(load "./frame-and-box.scm")

; phase 1
; parse SPD: S-expression plain document.
; returns list of parsed <box> fragments
(define (spd-parse port)
  (define hash (make-hash-table))
  (define (spd-construct sexp mom)
    (define classes '())
    (define kids  '())
    (define id #f)
    (cond 
      ((string? sexp) (make <text-box> :text sexp :mom mom))
      ((pair? sexp)
        (let1 box (make <box> :mom mom)
          (define (in-rect e) (and (< (~ box 'frame 'pos 0) (~ e 'motion 'x) (+ (~ box 'frame 'pos 0) (~ box 'frame 'size 0)))
                                   (< (~ box 'frame 'pos 1) (~ e 'motion 'y) (+ (~ box 'frame 'pos 1) (~ box 'frame 'size 1)))))
          (for-each (^s
            (cond
              ((string?  s) (push! (~ box 'kids) (spd-construct s box)))
              ((pair?    s) (push! (~ box 'kids) (spd-construct s box)))
              ((symbol?  s) (push! (~ box 'class) s))
              ((keyword? s) (set! (~ box 'id) s))))
            sexp)
          (and (~ box 'id) (set! (~ hash id) box))
          (for-each (^c
            (set! (~ hash c) (cons box (hash-table-get hash c '()))))
             classes)
          (connect SDL-mouse-hover (cons in-rect #f)
            (^e (if (in-rect e)
                (push! (~ box 'class) '.hover)
                (update! (~ box 'class) (^c (remove! (pa$ eq? '.hover) c))))
              (paint window)))
          (set! (~ box 'kids) (reverse (~ box 'kids)))
          box))))
    (let1 viewport (make <box> :id :viewport :class '() :text #f :kids (port-map (cut spd-construct <> #f) (lambda () (read port))))
      (set! (~ hash :viewport) viewport) (list viewport hash)))


; phase 2
; apply SST: S-expression STyle sheet.
; add <style-props> to <box>
(define (list->hash-table lis)
  (define hash (make-hash-table))
  (let loop ((h hash) (l lis))
    (if (null? l) h
      (begin
        (set! (~ h (car l)) (cadr l))
        (loop h (cddr l))))))

(define (sst-reset box)
  (set! (~ box 'style-rules) '())
  (for-each (^b (sst-reset b)) (~ box 'kids)))

(define (sst-apply box port)
  (define (apply-rule boxes s)
    (for-each (^b
        (when (match-rule b s) (push! (~ b 'style-rules) s))
        (apply-rule (~ b 'kids) s))
      boxes))
  (port-map (^r r
      (let* ((rule (car r)) (prop (cdr r))
             (specificity (calc-specificity rule))
             (props (make <style-props> :rule rule :spec specificity :prop (list->hash-table prop))))
        (apply-rule (list box) props)))
    (cut read port)))

(define (match-rule box style)
  (cond
    ((symbol?  (~ style 'rule))  (any (pa$ eq? (~ style 'rule)) (~ box 'class)))
    ((keyword? (~ style 'rule))  (eq? (~ box 'id) (~ style 'rule)))
    (else #f)))

(define (calc-specificity rule)
  (cond
    ((symbol? rule) 10)   ;class
    ((keyword? rule) 100) ;id
    ((pair? rule) (+ (calc-specificity (car rule)) (calc-specificity (cdr rule)))))) ;path

(define (inherited-style ancestor property)
  (if ancestor
    (let1 style (filter-most-specific (~ ancestor 'style-rules))
      (if (eq? 'inherit (~ style property))
        (inherited-style (~ ancestor 'mom) property)
        (~ style property)))
     (copy (~ default-style-properties-no-inheritance property))))

; phase 3
; construct <frame>s from <box>es
(define (list-join lis glue)
  (cond 
    ((null? lis) '())
    ((null? (cdr lis)) (list (car lis)))
    (else `(,(car lis) ,glue ,@(list-join (cdr lis) glue)))))
    
(define-method spd-frame-construct ((b <box>))
  (car (spd-frame-construct-internal b)))

(define-method spd-frame-construct-internal ((b <text-box>))
  (map (^(text)
    (let1 frame
      (if (string=? text "\n")
        (make <br-frame> :box b :kids '())
        (make <text-frame> :box b :kids '() :text text))
      (let1 style (filter-most-specific (~ b 'style-rules))
        (set! (~ b 'frame) frame)
        (set! (~ frame 'style) style)
        (set! (~ frame 'style :line-height) (inherited-style (~ b 'mom) :line-height))
        (set! (~ frame 'style :font-face) (inherited-style (~ b 'mom) :font-face))
        (set! (~ frame 'style :font-size) (inherited-style (~ b 'mom) :font-size))
        (set! (~ frame 'style :font-weight) (inherited-style (~ b 'mom) :font-weight))
        (set! (~ frame 'style :color) (inherited-style (~ b 'mom) :color)))
      frame)) (list-join (string-split (~ b 'text) "\n") "\n")))

(define-method spd-frame-construct-internal ((b <box>))
  (list (let1 frame (make <frame> :box b)
    (set! (~ b 'frame) frame) 
    (set! (~ frame 'style) (filter-most-specific (~ b 'style-rules)))
    (for-each (^(property)
        (when (eq? (~ frame 'style property) 'inherit)
          (set! (~ frame 'style property) (inherited-style (~ b 'mom) property))))
      (hash-table-keys (~ frame 'style)))
    (set! (~ frame 'kids) (apply append (map spd-frame-construct-internal (~ b 'kids))))
    frame)))

(define (filter-most-specific rules)
  (define table (make-hash-table))
  (define spec-table (make-hash-table))
  (for-each
    (^r (for-each 
        (^k (when (> (~ r 'spec) (hash-table-get spec-table k -inf.0))
            (set! (~ table k)      (copy (~ r 'prop k)))
            (set! (~ spec-table k) (copy (~ r 'spec)))))
        (hash-table-keys (~ r 'prop))))
    (cons (default-style-rule) rules))
  table)

(define-method copy ((x <string>))
  (string-copy x))
(define-method copy ((x <pair>))
  (map identity x)) ; XXX non-propoer list
(define-method copy ((x <vector>))
  (map-to <vector> identity x))
(define-method copy (x) x)

(define-method meter ((f <frame>) size)
  (for-each (cut meter <> size) (~ f 'kids)))

(define-method meter ((f <text-frame>) size)
  (if (~ f 'text)
    (set! (~ f 'size) (vector
      (min 
        (~ f 'style :max-size 0)
        (max
          (size (~ f 'text) (~ f 'style))
          (~ f 'style :min-size 0)))
      (min
        (~ f 'style :max-size 1)
        (max
          (* (~ f 'style :line-height) (~ f 'style :font-size))
          (~ f 'style :min-size 1)))))
    (set! (~ f 'size) (~ f 'style :min-size)))
  (next-method))

(define (dump frame)
  (frame-print frame)
  (for-each dump (~ frame 'kids)))

(define (frame-print frame)
  (cond 
    ((equal? 'bold (~ frame 'style :font-weight)) (print (format "[[~a]]" (~ frame 'text))))
    (else (print (format "~a:(~a x ~a) ~a" (~ frame 'box 'id)(~ frame 'size 0) (~ frame 'size 1) (~ frame 'text))))))

(define (default-style-rule) (make <style-props> :prop (list->hash-table default-style-properties) :spec 0))
(define default-style-properties '(
  :position auto
  :display #t
  :scroll #f
  :border-style-left none
  :border-style-bottom none
  :border-style-right none
  :border-style-top none
  :background-color transparent
  :background-image none
  :margin-bottom 0
  :margin-right 0
  :margin-left 0
  :margin-top 0
  :padding-bottom 0
  :padding-right 0
  :padding-left 0
  :padding-top 0
  :flow #f
  :max-size #(+inf.0 +inf.0)
  :min-size #(0 0)
  :line-height inherit
  :color inherit
  :font-weight normal
  :font-slant normal
  :font-size 14
  :font-face "M+ 1p"
  :shape rectanble))

(define default-style-properties-no-inheritance (list->hash-table '(
  :font-face "M+ 1p"
  :line-height 1.2
  :color #(0 0 0))))
