(use gauche.collection)
(use gauche.time)
(use srfi-1)
(define (graph-for-each proc vertice :key order first compair max-depth)
  (cond
    ((and (eq? order :post) (eq? first :width)) (graph-for-each-post-order-width-first proc vertice))
    ((eq? first :width) (graph-for-each-pre-order-width-first proc vertice))
    ((eq? order :post) (graph-for-each-post-order-depth-first proc vertice))
    (#t (graph-for-each-pre-order-depth-first proc vertice))))

(define (graph-for-each-pre-order-width-first proc vertice)
  (define V-adj '())
  (proc (car vertice))
  (set! V-adj ((cdr vertice)))
  (for-each
    (lambda (V) (proc (car V))) V-adj)
  (for-each
    (lambda (V) (graph-for-each-pre-order-width-first proc V)) V-adj))

(define (graph-for-each-post-order-width-first proc vertice)
  (define V-adj '())
  (proc (car vertice))
  (set! V-adj ((cdr vertice)))
  (for-each
    (lambda (V) (proc (car V))) V-adj)
  (for-each
    (lambda (V) (graph-for-each-pre-order-width-first proc V)) V-adj)
  (proc (car vertice)))

(define (graph-for-each-pre-order-depth-first proc vertice)
  (proc (car vertice))
  (for-each
    (lambda (e)
      (graph-for-each-pre-order-depth-first proc e)) ((cdr vertice))))

(define (graph-for-each-post-order-depth-first porc vertice)
  (for-each
    (lambda (e)
      (depth-first-for-each proc e)) ((cdr vertice)))
  (proc (car vertice)))


(define (dispatch-event dispatcher event :rest rest)
  (apply (pa$ graph-for-each
    (lambda (proc) (proc event)) (dispatcher->vertice dispatcher (modificate dispatcher event))) rest))

(define-class <event-dispatcher> () (
  (listeners :init-value '())
  (handler :init-keyword :handler :init-value (lambda (e)))))

(define-method dispatcher->vertice ((dispatcher <event-dispatcher>) event)
  (define (adj-vertices)
    (map (lambda (bond) (dispatcher->vertice (cdr bond) event))
            (filter (lambda (bond) ((car bond) event)) (ref dispatcher 'listeners))))
  (cons (ref dispatcher 'handler) adj-vertices))

(define-class <key-event-dispatcher> (<event-dispatcher>) ())
(define-method dispatcher->vertice ((dispatcher <key-event-dispatcher>) event)
  (define (adj-vertices)
    (map (lambda (bond) (dispatcher->vertice (cdr bond) event))
      (filter (lambda (bond) (eq? (car bond) (integer->char (~ event 'key 'keysym 'sym)))) (~ dispatcher 'listeners))))
  (cons (~ dispatcher 'handler) adj-vertices))

(define-class <watch-event-dispatcher> (<event-dispatcher>) ())
(define-method dispatcher->vertice ((dispatcher <watch-event-dispatcher>) event)
  (define (adj-vertices)
    (map (lambda (ev-dis) (dispatcher->vertice (cdr ev-dis) (car ev-dis)))
            (filter-map 
               (lambda (bond) (let1 result ((caar bond) event) 
                 (if (not (eq? result (cdar bond)))
                     (begin result (cdar bond) (set! (cdar bond) result) (cons result (cdr bond)))
                     #f)))
               (ref dispatcher 'listeners))))
  (cons (~ dispatcher 'handler) adj-vertices))

(define-method modificate ((dispatcher <event-dispatcher>) event) event)

(define-method disconnect-all ((src <event-dispatcher>))
  (set! (~ src 'listeners) '()))

(define-method connect ((src <event-dispatcher>) pred (dest <event-dispatcher>))
  (push! (ref src 'listeners) (cons pred dest)))

(define-method connect ((src <event-dispatcher>) pred dest)
  (push! (ref src 'listeners) (cons pred (make <event-dispatcher> :handler dest))))

(define SDL-keydown (make <key-event-dispatcher> :handler (^(e) (print (format "keydown: ~a" (charcode e))))))

(define-class <SDL-event-dispatcher> (<event-dispatcher>) ())
(define-method poll ((dispatcher <SDL-event-dispatcher>))
  (define e (make <SDL_Event>))
  (SDL_PollEvent (ptr e))
  (if (= (ref e 'type) 0)
    #f
    (begin (dispatch-event SDL-event e) #t)))
(define-method pop ((dispatcher <SDL-event-dispatcher>))
  (define e (make <SDL_Event>))
  (SDL_PollEvent (ptr e))
  (if (= (ref e 'type) 0)
    #f
    #t))

(define SDL-event (make <SDL-event-dispatcher>))
(define SDL-key-event (make <SDL-event-dispatcher>) )
(define SDL-mousemotion (make <event-dispatcher>))
(define SDL-mousebuttondown (make <event-dispatcher>))
(define SDL-mouse-hover (make <watch-event-dispatcher>))

(define (charcode e)
  (integer->char (~ e 'key 'keysym 'sym)))

(define (init-events)
  (connect SDL-event (^e (eq? (ref e 'type) SDL_KEYDOWN)) SDL-keydown)
  (connect SDL-event (^e (eq? (ref e 'type) SDL_MOUSEMOTION)) SDL-mousemotion)
  (connect SDL-mousemotion (^e #t) SDL-mouse-hover)
  (connect SDL-event (^e (eq? (ref e 'type) SDL_MOUSEBUTTONDOWN)) SDL-mousebuttondown))

(define c (make <real-time-counter>))
(define (mainloop)
  (define limit 0.1) ; skip events if time to handle evnet exceeds 100ms
  (time-counter-reset! c)
  (time-counter-start! c)
  (while (and (> limit (begin (time-counter-stop! c) (let1 time (time-counter-value c) (time-counter-start! c) time))) (poll SDL-event)) #f)
  (time-counter-stop! c)
  (while (pop SDL-event) #f)
  (SDL_Delay 10)
  (mainloop))

;(define (ternary-tree-push tree element)
;(define (ternary-tree-divide tree)
