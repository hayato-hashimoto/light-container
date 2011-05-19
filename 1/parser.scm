(load "./frame-and-box.scm")

; phase 1
; parse SPD: S-expression plain document.
; returns list of parsed <box> fragments
(define (spd-parse port)
  (define hash (make-hash-table))
  (define (spd-construct sexp)
    (define classes '())
    (define kids  '())
    (define text #f)
    (define id #f)
    (for-each (^s
      (cond
        ((string?  s) (set! text s))
        ((pair?    s) (push! kids (spd-construct s)))
        ((symbol?  s) (push! classes s))
        ((keyword? s) (set! id s)))) sexp)
    (let1 box (make <box> :class classes :id id :kids kids :text text)
      (and id (set! (~ hash id) box))
      (for-each (^c (set! (~ hash c) (cons box (hash-table-get hash c '()))))
         classes)
      box))
  (list (spd-construct (read port)) hash))


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

(define (sst-apply box port)
  (define viewport (make <box> :id :viewport :class '() :kids '() :text #f))
  (define (apply-rule boxes s)
    (for-each (^b
        (when (match-rule b s) (push! (~ b 'style-rules) s))
        (apply-rule (~ b 'kids) s))
      boxes))
  (port-map (^r
      (let* ((rule (car r)) (prop (cdr r))
             (specificity (calc-specificity rule))
             (props (make <style-props> :rule rule :spec specificity :prop (list->hash-table prop))))
        (apply-rule (list box viewport) props)))
    (cut read port))
    viewport)

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

; phase 3
; construct <frame>s from <box>es
(define (spd-frame-construct b)
  (let1 frame (make <single-glyph-frame> :kids (map spd-frame-construct (reverse (~ b 'kids))) :text (~ b 'text))
    (set! (~ frame 'style) (filter-most-specific (~ b 'style-rules)))
    #?=(hash-table-get (~ frame 'style) :color "n/a")
    frame))

(define (filter-most-specific rules)
  (define table (make-hash-table))
  (define spec-table (make-hash-table))
  (for-each
    (^r (for-each 
        (^k (when #?=(> (~ r 'spec) (hash-table-get spec-table k -inf.0))
            (set! (~ table k)      #?=(~ r 'prop k))
            (set! (~ spec-table k) #?=(~ r 'spec))))
        (hash-table-keys (~ r 'prop))))
    (cons (default-style-rule) rules))
  table)

(define-method meter ((f <frame>))
  (for-each meter (~ f 'kids)))

(define-method meter ((f <single-glyph-frame>))
  (if (~ f 'text)
    (set! (~ f 'size) #?=(vector (* (~ f 'style :font-size) (* 0.6 (string-length (~ f 'text)))) (* 1.4 (~ f 'style :font-size))))
    (set! (~ f 'size) #(0 0)))
  (next-method))

(define (dump frame)
  (frame-print frame)
  (for-each dump (~ frame 'kids)))

(define (frame-print frame)
  (and (~ frame 'text)
  (cond 
    ((equal? 'bold (~ frame 'style :font-weight)) (print (format "[[~a]]" (~ frame 'text))))
    (else (print (~ frame 'text))))))

(define (default-style-rule) (make <style-props> :prop (list->hash-table default-style-properties) :spec 0))
(define default-style-properties '(
  :border-style-left none
  :border-style-bottom none
  :border-style-right none
  :border-style-top none
  :background-color transparent
  :background-image none
  :margin-bottom 10
  :margin-right 10
  :margin-left 10
  :margin-top 10
  :flow horizontal
  :max-size #(600 +inf.0)
  :line-height 28
  :color #(0 0 0)
  :font-weight normal
  :font-slant normal
  :font-size 18
  :font-face "M+ 1p"))
