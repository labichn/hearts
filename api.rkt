#lang racket

(require "game.rkt" "opt.rkt")

#|
localhost:8000/hearts
|#



(define (make-resp ov)
  ())

;; create-game : list[jsepxr[player]] -> jsexpr[game]
;; creates an empty game with the given list of players
(define (create-game ps)
  (let ((g (new-game (add1 (game-count)) (map jsexpr->player ps) '())))
    (if (game-create! g) (some (game->jsexpr g)) (none))))


