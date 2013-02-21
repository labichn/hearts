#lang racket

(require (planet jaymccarthy/mongodb:1:=12)
         json rackunit "opt.rkt")
(provide mongo-count
         mongo-create!
         mongo-read-one
         mongo-update!
         mongo-delete!
         mongo-exists?)

;; CRUD via mongo

(define (mongo-update! bson new db coll host port)
  (and (mongo-exists? bson db coll host port)
       (some? (with-coll db coll host port 
                         (λ (c) (mongo-collection-replace! c bson new))))))

(define (mongo-delete! bson db coll host port)
  (some? (with-coll db coll host port
                    (λ (c) (mongo-collection-remove! c bson)))))

(define (mongo-create! bson db coll host port)
  (if (not (mongo-exists? bson db coll host port))
      (some? (with-coll db coll host port
                            (λ (c) (mongo-collection-insert-one! c bson))))
      #f))

(define (mongo-exists? bson db coll host port)
  (let ((ol (mongo-read bson db coll host port)))
    (if (some? ol) (> (length (some-val ol)) 0) #f)))

(define (mongo-read-one bson db coll host port)
  (opt-map car (mongo-read bson db coll host port)))

(define (mongo-read bson db coll host port)
  (with-coll db coll host port
             (λ (c) (sequence->list (mongo-collection-find c bson)))))

(define (mongo-count db coll host port)
  (with-coll db coll host port (λ (c) (mongo-collection-count c))))

(define (with-coll db coll host port f)
  (with-db db host port (λ (db) (f (mongo-collection db coll)))))

(define (with-db db host port f)
  (with-handlers (((λ (x) #t) (λ (x) (none))))
    (let ((opt-db (some (mongo-db (create-mongo #:host host
                                                #:port port) db))))
      (begin0 (opt-map f opt-db)
        (opt-map (compose1 close-mongo! mongo-db-mongo) opt-db)))))
