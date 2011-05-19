(use gauche.collection)
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

(define (dispatcher->vertice dispatcher event)
  (define (adj-vertices)
    (map (lambda (bond) (dispatcher->vertice (cdr bond) event))
            (filter (lambda (bond) ((car bond) event)) (ref dispatcher 'listeners))))
  (cons (ref dispatcher 'handler) adj-vertices))

(define (dispatch-event dispatcher event :rest rest)
  (apply (pa$ graph-for-each
    (lambda (proc) (proc event)) (dispatcher->vertice dispatcher (modificate dispatcher event))) rest))

(define-class <event-dispatcher> () (
  (listeners :init-value '())
  (handler :init-keyword :handler :init-value (lambda (e)))))

(define-method modificate ((dispatcher <event-dispatcher>) event) event)

(define-method add-event-listener ((src <event-dispatcher>) pred (dest <event-dispatcher>))
  (update! (ref src 'listeners) (pa$ cons (cons pred dest))))

(define-method add-event-listener ((src <event-dispatcher>) pred dest)
  (update! (ref src 'listeners) (pa$ cons (cons pred (make <event-dispatcher> :handler dest)))))

(define SDL-keydown (make <event-dispatcher> :handler (^(e) (print (format "keydown: ~a" (charcode e))))))

(define-class <SDL-event-dispatcher> (<event-dispatcher>) ())
(define-method poll ((dispatcher <SDL-event-dispatcher>))
  (define e (make <SDL_Event>))
  (SDL_PollEvent (ptr e))
  (if (= (ref e 'type) 0)
    #f
    (begin (dispatch-event SDL-event e) #t)))

(define SDL-event (make <SDL-event-dispatcher> :handler print))
(define SDL-key-event (make <SDL-event-dispatcher>) )
(define SDL-mouse-event (make <SDL-event-dispatcher>))

(define (charcode e)
  (integer->char (~ e 'key 'keysym 'sym)))

(define (init-events)
  (add-event-listener SDL-event (^(e) (eq? (ref e 'type) SDL_KEYDOWN)) SDL-keydown))

(define (mainloop)
  (poll SDL-event)
  (SDL_Delay 10)
  (mainloop))

;(define (ternary-tree-push tree element)
;(define (ternary-tree-divide tree)
