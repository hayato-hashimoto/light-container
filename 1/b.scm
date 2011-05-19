(load "./cairolib.scm")
(load "./cairo-wrapper.scm")
(load "./parser.scm")

(define-method paint (c (f <frame>))
  (paint-content c f)
  (for-each (cut paint c <>) (~ f 'kids)))

(define-method set-source-color ((cr (ptr <cairo_t>)) vec)
  (if (eq? 3 (vector-length vec))
    (set-source-rgb  cr (~ vec 0) (~ vec 1) (~ vec 2))
    (set-source-rgba cr (~ vec 0) (~ vec 1) (~ vec 2) (~ vec 3))))

(define-method paint-content (c (f <single-glyph-frame>))
  (define cr (~ c 'cairo))
  ; background
  (when (not (eq? (~ f 'style :background-color) 'transparent))
    (set-source-color (~ c 'cairo) (~ f 'style :background-color))
    (cairo_rectangle  (~ c 'cairo) (~ f 'pos 0) (~ f 'pos 1) 
                                   (~ f 'size 0) (~ f 'size 1))
    (cairo_fill  (~ c 'cairo)))
  ; background-image
  ; border
  (cairo_move_to cr (~ f 'pos 0) (~ f 'pos 1))
  (when (not (eq? (~ f 'style :border-style-left) 'none))
    (set-source-color (~ c 'cairo) (~ f 'style :border-color-left))
    (cairo_rel_line_to cr 0 (~ f 'size 1))
    (cairo_stroke cr))

  (cairo_move_to cr (~ f 'pos 0) (~ f 'pos 1))
  (cairo_rel_move_to cr 0 (~ f 'size 1))
  (when (not (eq? (~ f 'style :border-style-bottom) 'none))
    (set-source-color (~ c 'cairo) (~ f 'style :border-color-bottom))
    (cairo_rel_line_to cr (~ f 'size 0) 0)
    (cairo_stroke cr))

  (cairo_move_to cr (~ f 'pos 0) (~ f 'pos 1))
  (cairo_rel_move_to cr (~ f 'size 0) (~ f 'size 1))
  (when (not (eq? (~ f 'style :border-style-right) 'none))
      (set-source-color (~ c 'cairo) (~ f 'style :border-color-right))
      (cairo_rel_line_to cr 0 (- (~ f 'size 1)))
      (cairo_stroke cr))

  (cairo_move_to cr (~ f 'pos 0) (~ f 'pos 1))
  (cairo_rel_move_to cr (~ f 'size 0) 0)
  (when (not (eq? (~ f 'style :border-style-top) 'none))
    (set-source-color (~ c 'cairo) (~ f 'style :border-color-top))
    (cairo_rel_line_to cr (- (~ f 'size 0)) 0)
    (cairo_stroke cr))

  ; text 
  (set-source-rgb   (~ c 'cairo) (~ f 'style :color 0) (~ f 'style :color 1) (~ f 'style :color 2))
  (select-font-face (~ c 'cairo) (~ f 'style :font-face) (slant (~ f 'style :font-slant)) (weight (~ f 'style :font-weight)))
  (set-font-size    (~ c 'cairo) (~ f 'style :font-size))
  (move-to          (~ c 'cairo) (~ f 'pos 0) (+ (~ f 'pos 1) (~ f 'style :font-size)))
  (show-text        (~ c 'cairo) (or (~ f 'text) "")))

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

(define-method flow-horizontal ((f <frame>) c)
  (set! (~ f 'pos) (vector-copy (~ c 'pos)))
  (for-each (^k
      (rel-move-to c (~ f 'size 0) 0)
      (flow k c))
    (~ f 'kids)))

(define-method flow-vertical ((f <frame>) c)
  (set! (~ f 'pos) (vector-copy (~ c 'pos)))
  (for-each (^k
      (rel-move-to c 0 #?=(~ f 'size 1))
      (flow k c))
    (~ f 'kids)))

(define-class <window> ()
  ((frame) (document) (document-identifier) (stylesheet) (stylesheet-identifier) (cairo) (surface) (viewport)))

(define (test)
  (define window (make <window> 600 300))
  (set! (~ window 'stylesheet-identifier) "test/1.sst.s")
  (set! (~ window 'document-identifier)   "test/1.spd.s")
  (paint window)
  (mainloop))

(define-method initialize ((w <window>) initargs)
  (init (car initargs) (cadr initargs))
  (set! (~ w 'cairo) cairo)
  (set! (~ w 'surface) sdl-sf))


(define-method paint ((w <window>))
  (set! (~ w 'document)      (car (spd-parse (open-input-file (~ w 'document-identifier)))))
  (set! (~ w 'viewport) (filter-most-specific (~ (sst-apply (~ w 'document) (open-input-file (~ w 'stylesheet-identifier))) 'style-rules)))
  (set!  (~ w 'frame) (spd-frame-construct (~ w 'document)))
  (dump  (~ w 'frame))
  (meter (~ w 'frame))
  (set!  (~ w 'frame 'pos) (vector 0 0))
  (flow  (~ w 'frame))

  (when (not (eq? (~ w 'viewport :background-color) 'transparent))
    (set-source-rgb (~ w 'cairo) (~ w 'viewport :background-color 0) (~ w 'viewport :background-color 1) (~ w 'viewport :background-color 2))
     (cairo_paint (~ w 'cairo)))

  (when (not (eq? (~ w 'viewport :background-image) 'none))
    (let1 png (cairo_image_surface_create_from_png (~ w 'viewport :background-image))
     (cairo_set_source_surface (~ w 'cairo) png 0 0)
     (cairo_paint (~ w 'cairo))))

  (paint w (~ w 'frame))
  (SDL_Flip (~ w 'surface)))

(define (weight value)
  (case value
    ((500 600 700 800 900 bold) CAIRO_FONT_WEIGHT_BOLD)
    ((100 200 300 400 normal) CAIRO_FONT_WEIGHT_NORMAL)))

(define (slant value)
  (case value
    ((italic)  CAIRO_FONT_SLANT_ITALIC)
    ((oblique) CAIRO_FONT_SLANT_OBLIQUE)
    ((normal)  CAIRO_FONT_SLANT_NORMAL)))

(test)
