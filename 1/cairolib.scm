(use c-wrapper)
(define quit exit)

(c-load "SDL/SDL.h" :libs "-lSDL")
(c-load "cairo/cairo.h" :libs "-lcairo")

(set! exit (lambda x (quit)))
(define sdl-sf '())
(define cairo-sf '())
(define cairo '())
(define display-list '())

(define (init w h)
  (init-video w h)
  (load "./sdl-wrapper.scm")
  (init-events)
  (load "./config.scm")
  (connect SDL-keydown (~ config 'keys 'quit) exit))

(define (init-video w h)
  (SDL_Init SDL_INIT_VIDEO)
  (set! sdl-sf (SDL_SetVideoMode w h 32 (logior 0 SDL_HWSURFACE SDL_RESIZABLE SDL_DOUBLEBUF)))
  (set! cairo-sf (cairo_image_surface_create_for_data (~ sdl-sf 'pixels) CAIRO_FORMAT_ARGB32 (~ sdl-sf 'w) (~ sdl-sf 'h) (~ sdl-sf 'pitch)))
  (set! cairo (cairo_create cairo-sf)))
