#lang racket

(require (for-syntax racket/bool
                     racket/match
                     racket/syntax)
         "util.rkt"
         "persist.rkt"
         racket/flonum
         (planet jaymccarthy/mongodb:1:=12)
         rackunit)
(provide mongo-struct)

;; this will be convenient, and a good way to learn about racket macros

;; first last
;; =>
;; 'first #'person-first
;; 'last #'person-lsat
;; 

;; symbol append
  ;; list[(list ident [x -> jsexpr])]
  ;; =>
  ;; list[#'(list 


  ;; clone!-proc : syntax -> syntax
  ;; makes #'(name-clone! : name [host string] [port nat] [coll string] -> jsexpr)
  ;; create!-proc : syntax -> syntax
  ;; makes #'(name-create! : name [host string] [port nat] [coll string] -> jsexpr)
  ;; read-proc : syntax -> syntax
  ;; makes #'(name-read : name [host string] [port nat] [coll string] -> opt[name])
  ;; update!-proc : syntax -> syntax
  ;; makes #'(name-update! : name [host string] [port nat] [coll string] -> bool)
  ;; delete!-proc : syntax -> syntax
  ;; makes #'(name-delete! : name [host string] [port nat] [coll string] -> bool)
  ;; exists?-proc : syntax -> syntax
  ;; makes #'(name-exists? : jsexpr [host string] [port nat] [coll string] -> bool)
  ;; connected?-proc
  ;; makes #'(name-connected? : name [host string] [port nat] [coll string] -> bool)

;; examples:

;; simplest form:
;; (crudy-struct posn (x y))
;; =>
;; (crudy-struct posn ((x (λ (x) x)) (y (λ (x) x)))
;;                    ("localhost" 12345 "posn")
;;                    #:clone #:create #:read #:update #:delete #:exists)
;; =>

;; A crud struct is a:
;; (crud-struct name (param-clause param-clause ...) [(host port coll)])
;; where name is the identifier name of the struct,
;;       param-clause is one of: {identifier, (identifier [x -> jsexpr])},
;;       host is a string hostname (e.g. "localhost", "192.168.0.101"),
;;       port is a port number,
;;   and coll is a string mongodb collection name
;;
;; concern: need json and mccarthy's mongodb (PLaneT) imported

  #|
  a key is a hash, where required fields are serialized as
  "{ "name":value ...+ }"
  where name is the name of the field and the value is a jsexpr
  required fields need to be defined in the initial syntax
  - (mongo-struct name (clause ...) (? (host port coll)) option ...)
  where name is the identifier name of the struct,
  clauses describe the fields of this structure, and are one of:
  - identifier
  where the value is assumed to be a jsexpr (i.e. the
  serializer and deserializer are the identity function), and
  is not a required field
  - (list identifier [x -> jsexpr] [jsexpr -> x]),
  where the value can be anything (x), as long as it provides its own
  serialzer and deserializer
  - (list identifier #:key)
  - (list identifier #:key [x -> jsexpr] [jsexpr -> x])
  where these two forms are (respectively) identical to the previous
  two forms, but also note that the field denoted by identifier
  is required for the mongo-struct's key
  host is a string hostname (e.g. "localhost", "192.168.0.101"),
  port is a port number,
  coll is a string mongodb collection name,
  and option is one of: (`name' here refers to the struct name)
  done       - #:update,     which generates: [name-update-field : name val -> name]
  done       - #:->jsexpr,   which generates: [name->jsexpr : name -> jsexpr]
  done       - #:jsexpr->,   which generates: [jsexpr->name : jsexpr -> name]
  - #:->key,      which generates: [name->key : name -> key]
  where a key is a subset of the struct that is intended to
  be unique, in jsexpr form. any value of a key must be jsexprs
  only
  - #:create!,    which generates: [name-create! : name -> opt[name]]
  - #:read,       which generates: [name-read : [name | key] -> opt[name]]
  - #:update!,    which generates: [name-update! : key name -> opt[name]]
  - #:delete!,    which generates: [name-delete! : [name | key] -> opt[name]]
  - #:exists?,    which generates: [name-exists? : [name | key] -> bool]
  - #:connected?, which generates: [name-connected? : [name -> bool]]
  If no options are present, all are assumed; if any persistence layer options
  are present ({#:create!,#:read,#:update!,#:delete!}) the #:->jsexpr,
  #:jsexpr-> and #:->key options are assumed.
  |#
  #|
  expand : list[clause] -> list[(list sym bool [x -> jsexpr] [jsexpr -> x])]
  a clause is one of:
- identifier
where the value is assumed to be a jsexpr (i.e. the
  serializer and deserializer are the identity function), and
  is not a required field
  - (list identifier [x -> jsexpr] [jsexpr -> x]),
  where the value can be anything (x), as long as it provides its own
  serialzer and deserializer
  - (list identifier '#:key)
  - (list identifier '#:key [x -> jsexpr] [jsexpr -> x])
  where these two forms are (respectively) identical to the previous
  two forms, but also note that the field denoted by identifier
  is required for the mongo-struct's key
  |#

(begin-for-syntax
 (define (default-connection n)
   `("test" ,(symbol->string n) "localhost" 27017))
                                        ; miscellaneous helpers
 (define (sym-app . args)
   (string->symbol (apply string-append (map symbol->string args))))
 (define d->s datum->syntax)
 (define s->d syntax->datum)
                                        ; ops on the config
 (define conf-coll cadddr)
 (define conf-db caddr)
 (define conf-port cadr)
 (define conf-host car)
                                        ; ops on the field clauses
 (define f-name car)
 (define f-key? cadr)
 (define f-serializer caddr)
 (define f-deserializer cadddr)
 (define (guard sxp)
   `(compose1 ,sxp (λ (x) (if (flonum? x) (fl->exact-integer x) x))))
 (define (exps->hash-refs name exps inst ctx)
   (map (λ (quad) (d->s ctx `(,(guard (f-deserializer quad))
                              (hash-ref ,inst ',(f-name quad))))) exps))
 (define (exps->inst->hash-args exps name)
   (λ (inst)
      (foldl (λ (n a)
                (cons `',(f-name n)
                      (cons (list `,(guard (f-serializer n))
                                  (list (accessor-name name (f-name n)) inst))
                            a)))
             '()
             exps)))
 (define (exps->inst->key-hash-args exps name)
   (exps->inst->hash-args (filter f-key? exps) name))
 (define gen-fields '(host port db coll))
 (define (add-gen-fields clauses)
   (append clauses gen-fields))
 (define (remove-gen-fields quads)
   (filter (λ (x) (andmap (λ (y) (not (symbol=? (f-name x) y))) gen-fields)) quads))
 (define (expand clauses)
   (define (key? k) (and (keyword? k) (equal? (keyword->string k) "key")))
   (map (λ (x)
           (match x
             ((? symbol? i)
              (list i #f (λ (x) x) (λ (x) x)))
             ((list (? symbol? i) (? key? k))
              (list i #t (λ (x) x) (λ (x) x)))
             ((list (? symbol? i) serializer deserializer)
              (list i #f serializer deserializer))
             ((list (? symbol? i) (? key? k) serializer deserializer)
              (list i #t serializer deserializer))
             (_ (raise-syntax-error 'expand
                                    "Bad syntax in mongo-struct field clauses" x))))
        clauses))
                                        ; names for procs
 (define (accessor-name n f) (sym-app n '- f))
 (define (new-name n) (sym-app 'new- n))
 (define (jsexpr->-name n) (sym-app 'jsexpr-> n))
 (define (->jsexpr-name n) (sym-app n '->jsexpr))
 (define (->key-name n) (sym-app n '->key))
 ;; procs
 (define (update-proc n f)
   #`(define (#,(d->s n (sym-app (s->d n) '-update- f)) s val)
       (struct-copy #,n s [#,(d->s n f) val])))
 (define (->jsexpr-proc name exps ctx)
   #`(define (#,(d->s ctx (->jsexpr-name name)) a)
       (hash #,@((exps->inst->hash-args (remove-gen-fields exps) name) #'a))))
 (define (jsexpr->-proc name exps ctx)
   #`(define (#,(d->s ctx (jsexpr->-name name)) a)
       (#,(d->s ctx (new-name name))
        #,@(exps->hash-refs name (remove-gen-fields exps) #'a ctx))))
 (define (->key-proc name exps ctx)
   #`(define (#,(d->s ctx (->key-name name)) a) ;; look into format-id
       (hash #,@((exps->inst->key-hash-args exps name) #'a))))
 (define (new-proc name fields conf ctx)        ;; build struct procs
   #`(define (#,(d->s ctx (new-name name)) #,@fields)
       (#,(d->s ctx name) #,@fields #,@conf)))
 (define (read-proc name conf ctx)
   #`(define (#,(d->s ctx (sym-app name '-read)) k)
       (opt-map (λ (x) (#,(jsexpr->-name name) x))
                (mongo-read-one k #,@conf))))
 (define (mongo-proc f sym name conf ctx)
   #`(define (#,(d->s ctx (sym-app name sym)) a)
       (#,f (#,(->jsexpr-name name) a) #,@conf)))
 (define (count-proc name conf ctx)
   #`(define (#,(d->s ctx (sym-app name '-count))) (mongo-count #,@conf)))
 (define (update!-proc name conf ctx)
   #`(define (#,(d->s ctx (sym-app name '-update!)) a b)
       (mongo-update! (#,(->jsexpr-name name) a)
                      (#,(->jsexpr-name name) b)
                      #,@conf))))

(define-syntax (mongo-struct stx)
  (define (full-def stx)
    (syntax-case stx ()
      ((mongo-struct name clauses)
       (full-def #`(mongo-struct name
                                 clauses
                                 #,(default-connection (s->d #'name)))))
      ((mongo-struct name clauses conf)
       (let* ((expanded (expand (add-gen-fields (s->d #'clauses))))
              (fields (map f-name expanded))
              (name-sym (s->d #'name))
              (conf-sym (s->d #'conf)))
         #`(begin
             (struct name #,(d->s #'name fields) #:transparent)
             #,(new-proc name-sym
                         (map f-name (remove-gen-fields expanded))
                         conf-sym
                         #'name)
             #,@(map (λ (field) (update-proc #'name field)) fields)
             #,(->jsexpr-proc name-sym expanded #'name)
             #,(jsexpr->-proc name-sym expanded #'name)
             #,(->key-proc    name-sym expanded #'name)
             #,(read-proc     name-sym conf-sym #'name)
             #,(mongo-proc 'mongo-exists? '-exists? name-sym conf-sym #'name)
             #,(mongo-proc 'mongo-delete! '-delete! name-sym conf-sym #'name)
             #,(mongo-proc 'mongo-create! '-create! name-sym conf-sym #'name)
             #,(count-proc name-sym conf-sym #'name)
             #,(update!-proc name-sym conf-sym #'name))))))
  (full-def stx))

(define-syntax (json-struct stx)
  (define (full-def stx)
    (syntax-case stx ()
      ((_ name clauses)
       (let* ((expanded (expand (s->d #'clauses)))
              (fields (map f-name expanded))
              (name-sym (s->d #'name)))
         #`(begin
             (struct name #,(d->s #'name fields) #:transparent)
             #,(new-proc name-sym (map f-name expanded) '() #'name)
             #,@(map (λ (field) (update-proc #'name field)) fields)
             #,(->jsexpr-proc name-sym expanded #'name)
             #,(jsexpr->-proc name-sym expanded #'name)
             #,(->key-proc    name-sym expanded #'name))))))
  (full-def stx))
(json-struct nsop (x y))
(check-equal? (jsexpr->nsop (nsop->jsexpr (new-nsop 2 1))) (new-nsop 2 1))

(mongo-struct posn ((x #:key) (y #:key)))
(mongo-struct posn1 ((x #:key) y) ("test" "posn1" "localhost" 27017))
(mongo-struct posn2 ((x (λ (x) x) (λ (x) x))
                     (y #:key)))
(mongo-struct pair-o-posns
              ((p1 posn1->jsexpr jsexpr->posn1) (p2 posn2->jsexpr jsexpr->posn2))
              ("test" "pop" "localhost" 27017))

(define p1 (new-posn1 1 2))
(define p2 (new-posn2 3 4))
(define pop (new-pair-o-posns p1 p2))

(check-equal? (new-posn1 1 2) p1)
(check-equal? (new-posn2 3 4) p2)
(check-true (not (equal? (new-posn1 2 2) p1)))
(check-true (not (equal? (new-posn2 3 5) p2)))
(check-equal? (posn1-update-x p1 3) (new-posn1 3 2))
(check-equal? (posn2-update-x p2 2) (new-posn2 2 4))
(check-equal? (posn1-update-y p1 3) (new-posn1 1 3))
(check-equal? (posn2-update-y p2 5) (new-posn2 3 5))
(check-equal? (posn1->jsexpr p1) (hash 'x 1 'y 2))
(check-equal? (posn2->jsexpr p2) (hash 'x 3 'y 4))
(check-equal? (pair-o-posns->jsexpr pop)
              (hash 'p1 (hash 'x 1 'y 2) 'p2 (hash 'x 3 'y 4)))
(check-equal? (jsexpr->posn (posn->jsexpr (new-posn 1 2))) (new-posn 1 2))
(check-equal? (jsexpr->pair-o-posns (pair-o-posns->jsexpr pop))
              pop)
(check-equal? (posn->key (new-posn 1 2)) (hash 'x 1 'y 2))
(check-equal? (posn1->key (new-posn1 1 2)) (hash 'x 1))
(check-equal? (posn2->key (new-posn2 0 -1)) (hash 'y -1))
(check-equal? (pair-o-posns->key (new-pair-o-posns (new-posn1 3 2) (new-posn2 0 1)))
              (hash))
(check-true (not (posn-exists? (new-posn 2 1))))
;; only run if you have connection to db
(check-equal? (posn-read (posn->key (new-posn 1 2))) (some (new-posn 1 2)))
(check-true (posn-exists? (new-posn 1 2)))
(check-true (posn-create! (new-posn 3 4)))
(check-true (posn-delete! (new-posn 3 4)))
(check-true (posn-create! (new-posn 3 4)))
(check-true (not (posn-create! (new-posn 3 4))))
(check-true (posn-delete! (new-posn 3 4)))
(check-true (posn-delete! (new-posn 3 4)))
(check-true (not (posn1-update! (new-posn1 99 100) (new-posn1 100 100))))
(check-true (posn1-create! (new-posn1 99 100)))
(check-true (posn1-update! (new-posn1 99 100) (new-posn1 100 100)))
(check-equal? (posn1-read (posn1->key (new-posn1 100 100)))
              (some (new-posn1 100 100)))
(check-true (posn1-delete! (new-posn1 100 100)))

;; only run if you don't have connection to db
;(check-equal? (posn-read (posn->key (new-posn 1 2))) (none))
;(check-true (not (posn-exists? (posn->key (new-posn 1 2)))))
;(check-equal? (posn-create! (new-posn 3 4)) (none))
