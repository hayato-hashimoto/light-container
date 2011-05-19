(load "./cairolib.scm")
(load "./cairo-wrapper.scm")

(define-class <box> ()
  ((fillae :init-value '())
   (mater)
   (frame)
   (text :init-keyword :text)
   (style :init-value (make-hash-table))))

(define-class <frame> ()
  ((geometry :init-keyword :geometry)
   (size :init-value #(0 0))
   (fillae :init-value '())
   (style :init-value (make-hash-table))))

(define-class <single-glyph-frame> (<frame>)
  ((text :init-keyword :text)))

(define-class <paint-context> ()
  ((cairo :init-keyword :cairo)))

(define-class <flow-context> ()
  ((geometry :init-value #(0 0))))

(define-method rel-move-to ((c <flow-context>) x y)
  (update! (~ c 'geometry 0) (pa$ + x))
  (update! (~ c 'geometry 1) (pa$ + y)))

(define-method paint (c (f <frame>))
  (paint-content c f)
  (for-each (cut paint c <>) (~ f 'fillae)))

(define-method paint-content (c (f <single-glyph-frame>))
  (set-source-rgb   (~ c 'cairo) (~ f 'style 'color 0) (~ f 'style 'color 1) (~ f 'style 'color 2))
  (select-font-face (~ c 'cairo) (~ f 'style 'font-face) (slant (~ f 'style 'font-slant)) (weight (~ f 'style 'font-weight)))
  (set-font-size    (~ c 'cairo) (~ f 'style 'font-size))
  (move-to          (~ c 'cairo) (~ f 'geometry 0) (~ f 'geometry 1))
  (show-text        (~ c 'cairo) (~ f 'text)))

(define (norm len scale accessor)
  (case (car len)
    ((rel) (* (cadr len) (accessor (~ scale 'mater))))
    (else (car len))))

(define (? . a)
  (if (null? (cddr a))
    (%? (car a) (cadr a))
    (and (%? (car a) (cadr a))
         (apply (pa$ ? (ref (car a) (cadr a))) (cddr a)))))

(define-method %? ((hash <hash-table>) key)
  (hash-table-exists? hash key))
(define-method %? (obj slot) #t)

(define (make-frame b)
  (let1 f (make <single-glyph-frame>)
    (set! (~ b 'frame) f)
    (set! (~ f 'size) #(300 300))
    (if (? b 'style 'top)
      (set! (~ f 'geometry) (vector 100 (norm (~ b 'style 'top) b (cut ~ <> 'frame 'size 0))))
      (set! (~ f 'geometry) #(100 100)))
    (set! (~ f 'style 'font-weight) 400)
    (set! (~ f 'style 'font-slant)  'oblique)
    (set! (~ f 'style 'font-face)   "M+ 1p")
    (set! (~ f 'style 'font-size)   20)
    (set! (~ f 'style 'color)       #(0.4 0.4 1))
    (set! (~ f 'text) (~ b 'text))
    (set! (~ f 'fillae) (map make-frame (~ b 'fillae)))
    f))

(define (flow f)
  (define c (make <flow-context>))
  (for-each (^filla
      (set! (~ filla 'geometry) (~ c 'geometry))
      (rel-move-to c (~ f 'size 0) 0))
    (~ f 'fillae)))

(define (flow-vertical f)
  (define c (make <flow-context>))
  (for-each (^filla
      (set! (~ filla 'geometry) (~ c 'geometry))
      (rel-move-to c 0 (~ f 'size 1)))
    (~ f 'fillae)))

(define (test)
  (define root-box (make <box> :text ""))
  (define test-box (make <box> :text "a"))
  (set! (~ test-box 'style 'top) '(rel 0.8))
  (set! (~ test-box 'mater) root-box)
  (set! (~ root-box 'fillae) (list test-box))
  (set! (~ root-box 'mater) root-box)
  (paint (make-context) (make-frame root-box))
  (SDL_Flip sdl-sf))

(define (make-context)
  (init 300 300)
  (make <paint-context> :cairo cairo))

(define (weight value)
  (case value
    ((500 600 700 800 900) CAIRO_FONT_WEIGHT_BOLD)
    ((100 200 300 400) CAIRO_FONT_WEIGHT_NORMAL)))

(define (slant value)
  (case value
    ((italic)  CAIRO_FONT_SLANT_ITALIC)
    ((oblique) CAIRO_FONT_SLANT_OBLIQUE)
    ((normal)  CAIRO_FONT_SLANT_NORMAL)))
