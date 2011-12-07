#!/usr/bin/gosh
(load "./paint.scm")
(use gauche.threads)
(define (view uri)
  (set! window (make <main-window> 900 1800))
  (connect SDL-event
    (^e (eq? (ref e 'type) SDL_VIDEORESIZE))
    (^e
      (SDL_Flip (~ window 'surface))
      (set! (~ window 'surface) (SDL_SetVideoMode (~ e 'resize 'w) (~ e 'resize 'h) 32 (logior SDL_RESIZABLE 0)))
      ;(SDL_FillRect (~ window 'surface) NULL #xffffffff)
      (SDL_Flip (~ window 'surface))
      (set! (~ window 'cairo) (cairo_create (cairo_image_surface_create_for_data (~ window 'surface 'pixels) CAIRO_FORMAT_ARGB32 (~ window 'surface 'w) (~ window 'surface 'h) (~ window 'surface 'pitch))))
      (set! (~ window 'size) (vector (~ e 'resize 'w) (~ e 'resize 'h))) (paint window)))
  (connect SDL-keydown #\j (^e (update! (~ window 'current-page 'scroll-position 1) (^x (min (ceiling (- (~ window 'current-page 'size 1) (~ window 'size 1))) (+ x 20)))) (repaint window)))
  (connect SDL-keydown #\k (^e (update! (~ window 'current-page 'scroll-position 1) (^x (max 0 (+ x -20)))) (repaint window)))
  (connect SDL-keydown #\h (^e (update! (~ window 'current-page 'scroll-position 0) (^x (max 0 (+ x -20)))) (repaint window)))
  (connect SDL-keydown #\l (^e (update! (~ window 'current-page 'scroll-position 0) (pa$ + 20)) (repaint window)))
  (connect SDL-keydown #\g (^e (set! (~ window 'current-page 'scroll-position 1) 0) (repaint window)))
  (connect SDL-keydown #\f (^e (show-hints (~ window 'current-page 'document) (~ window 'current-page 'document-overlay))))
  (connect SDL-keydown #\space (^e (update! (~ window 'current-page 'scroll-position 1) (^x (min (ceiling (- (~ window 'current-page 'size 1) (~ window 'size 1))) (+ x 240)))) (repaint window)))
  (set! (~ window 'current-page) (make <window>))
  (set! (~ window 'stylesheet-uri) "preferences/userContent.sst")
  (set! (~ window 'document-uri) uri)
  (paint window)
  (mainloop))

(define (show-hints doc target)
  (define hint-text-chars "asdfqwer")
  (define next-text
    (define i 0)
    (define base (string-length hint-text-chars))
    (lambda ()
      (begin0
        (if (= i 0) "a"
          (string
            (unfold i (^i (= i 0)) (^i (values (~ hint-text-chars (floor (/ i base))) (- i (floor (/ i base))))))))
        (set! i (+ i 1)))))
  (for-each 
    (^x
      (append-child! target
        (vertice `(.hint :style (:position ,(~ x 'style :position)) ,(next-text)))))
    (query doc .a)))
