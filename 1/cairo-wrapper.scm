(define-method set-source-rgb ((cairo (ptr <cairo_t>)) r g b)
  (cairo_set_source_rgb cairo r g b))
(define-method set-source-rgba ((cairo (ptr <cairo_t>)) r g b a)
  (cairo_set_source_rgba cairo r g b a))
(define-method show-text ((cairo (ptr <cairo_t>)) text)
  (cairo_show_text cairo text))
(define-method move-to ((cairo (ptr <cairo_t>)) x y)
  (cairo_move_to cairo x y))
(define-method select-font-face ((cairo (ptr <cairo_t>)) arg1 arg2 arg3)
  (cairo_select_font_face cairo arg1 arg2 arg3))
(define-method set-font-size ((cairo (ptr <cairo_t>)) arg1)
  (cairo_set_font_size cairo arg1))
