#lang racket
(require json rackunit "json.rkt")

#|
(provide (struct-out game)
         game->string
         string->game
         (struct-out player)
         json->player
         player->string
         string->player
         (struct-out score))

;; Game level data.

;; a Game is a (game num players hands)
;; where num is a Nat representing the game number,
;;       players is a list of Players in the game, ordered by deal order,
;;   and hands is a temporally ordered list of Hands.
(struct game (num players hands) #:transparent)

;; a Player is a (player nick first last)
;; where nick is the player's String nickname,
;;       first is the player's String first name,
;;   and last is the player's String last name.
(struct player (nick first last) #:transparent)

;; a Hand is a list of Scores

;; a Score is a (score nick val)
;; where nick is the nickname of the player whose Score this is
;;   and val is the difference to the player's total score [-26,26].
(struct score (nick val) #:transparent)

(define hand0 (list (score "n" 0) (score "m" 26) (score "b" 26) (score "j" 26)))
(define hands0 (list hand0 hand0 hand0 hand0))
(define players0 (list (player "n" "Nicholas" "Labich")
                       (player "m" "Michael" "Amirault")
                       (player "b" "Ryan" "Bigelow")
                       (player "j" "Jeff" "Guion")))
(define game0 (game 0 players0 hands0))
(define json0 (string-append "{\"num\":0,"
                             "\"players\":[{\"nick\":\"n\","
                             "\"first\":\"Nicholas\","
                             "\"last\":\"Labich\"},"
                             "{\"nick\":\"m\","
                             "\"first\":\"Michael\","
                             "\"last\":\"Amirault\"},"
                             "{\"nick\":\"b\","
                             "\"first\":\"Ryan\","
                             "\"last\":\"Bigelow\"},"
                             "{\"nick\":\"j\","
                             "\"first\":\"Jeff\","
                             "\"last\":\"Guion\"}],"
                             "\"hands\":[[{\"nick\":\"n\",\"score\":0},"
                             "{\"nick\":\"m\",\"score\":26},"
                             "{\"nick\":\"b\",\"score\":26},"
                             "{\"nick\":\"j\",\"score\":26}],"
                             "[{\"nick\":\"n\",\"score\":0},"
                             "{\"nick\":\"m\",\"score\":26},"
                             "{\"nick\":\"b\",\"score\":26},"
                             "{\"nick\":\"j\",\"score\":26}],"
                             "[{\"nick\":\"n\",\"score\":0},"
                             "{\"nick\":\"m\",\"score\":26},"
                             "{\"nick\":\"b\",\"score\":26},"
                             "{\"nick\":\"j\",\"score\":26}],"
                             "[{\"nick\":\"n\",\"score\":0},"
                             "{\"nick\":\"m\",\"score\":26},"
                             "{\"nick\":\"b\",\"score\":26},"
                             "{\"nick\":\"j\",\"score\":26}]]}"))

;; Pushing this for a record. No need for all this ridiculousness.

;; player->string : Player -> String
;; converts the given player to a string of json
(define (player->jsexpr p)
  (hash 'nick (player-nick p)
        'first (player-first p)
        'last (player-last p)))
(check-equal? (player->string (player "n" "Nicholas" "Labich"))
              "{\"nick\":\"n\",\"first\":\"Nicholas\",\"last\":\"Labich\"}")

;; jsexpr->player : jsexpr -> Player
;; gets a player given a string of json
(define (jsexpr->player j)
  (player (hash-ref j 'nick)
          (hash-ref j 'first)
          (hash-ref j 'last)))
(check-equal? (jsexpr->player (hash 'nick "n"
                                    'last "Labich"
                                    'first "Nicholas"))
              (player "n" "Nicholas" "Labich"))

;; score->string : Score -> String
;; coverts a score into a string of json
(define (score->string s)
  (struct->string (list (list "nick" score-nick #t)
                      (list "score" (λ (x) (number->string (score-val x))) #f))
                s))
(check-equal? (score->string (score "n" 0)) "{\"nick\":\"n\",\"score\":0}")

;; string->score : String -> Score
;; gets a score from a string of json
(define (string->store j)
  (json->store (string->jsexpr j)))
(define (json->store h)
  (score (hash-ref h 'nick) (hash-ref h 'score)))
(check-equal? (string->store "{\"nick\":\"n\",\"score\":0}")
              (score "n" 0))

;; hand->string : Hand -> String
;; converts the given hand into a string of json
(define (hand->string h)
  (list->string h score->string))
(check-equal? (hand->string hand0)
              (string-append "[{\"nick\":\"n\",\"score\":0},"
                             "{\"nick\":\"m\",\"score\":26},"
                             "{\"nick\":\"b\",\"score\":26},"
                             "{\"nick\":\"j\",\"score\":26}]"))

;; string->hand : String -> Hand
;; gets a hand from a string of json
(define (string->hand j)
  (jsons->hand (string->jsexpr j)))
(define (jsons->hand hs)
  (map (λ (x) (score (hash-ref x 'nick) (hash-ref x 'score))) hs))
(check-equal? (string-append "[{\"nick\":\"n\",\"score\":0},"
                             "{\"nick\":\"m\",\"score\":26},"
                             "{\"nick\":\"b\",\"score\":26},"
                             "{\"nick\":\"j\",\"score\":26}]")
              (hand->string hand0))

;; game->string : Game -> String
;; converts the given game to a string of json
(define (game->string g)
  (struct->string (list (list "num"
                              (λ (g) (number->string (game-num g))) #f)
                        (list "players"
                              (λ (g) (list->string (game-players g)
                                                   player->string)) #f)
                        (list "hands"
                              (λ (g) (list->string (game-hands g)
                                                   hand->string)) #f))
                  g))
(check-equal? (game->string game0) json0)

;; string->game : String -> Game
;; gets a game from a string of json
(define (string->game j)
  (let ((hash (string->jsexpr j)))
    (game (hash-ref hash 'num)
          (map json->player (hash-ref hash 'players))
          (map jsons->hand (hash-ref hash 'hands)))))
(check-equal? (string->game json0) game0)
|#
