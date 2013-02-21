#lang racket
(require rackunit "mongo-struct.rkt")

(define host "localhost")
(define port 27017)
(define db "hearts")

(mongo-struct player ((nick #:key) first last) (host port db))
;; a player is a (new-player nick first last)
;; a trick is a
;; - (hash 'a int 'b int 'c int 'd int)
;;     where a, b, and c are the players' nicks, and the
;;     ints are the points that player took that trick
;; a hand is a temporally ordered list[trick]
(mongo-struct game ((gid #:key) hands players) (host port db))
;; a game is a (new-game list[hand] list[player])

;; 
