(define-class <lightcontainer-geometry> () (
  (width :init-keyword  :width)
  (height :init-keyword :height)
  (x :init-keyword :x)
  (y :init-keyword :y)))

(define-class <frame> (<geometry>) (
  (parent)
  (style))

(resolve-size

  (min ((~ 'max prop) (max (~ 'min prop) (~ 'ref prop))))
  (min max-width (max min-width width))

(define-method resolve-geometry ((f <frame>))
; resolve size
  (set!
    (parent 

; resolve coordinate
  (set!
    

(define-class <lightcontainer-stylechain>)
(define-class <lightcontainer>
  (width

(define (resolve-% container percentage)
  (~ container 'parent 'width)

'(

global
 layout flow
 flow rtl ttb

#id
 width
  min rel disp 0.4
  rel 0.8
 height 
  rel 1

#t
 width
  min rel 1
 height
  rel lh 1
 padding
  hr rel ::self 0.2
 align
  hr center
 bottom
  just 0
 bg color # eee 

#t *
 font face sans
 bg color

menu
 layout fixed
