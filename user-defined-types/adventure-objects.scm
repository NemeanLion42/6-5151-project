#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;; Object types for Adventure game

(define thing:location
  (make-property 'location
                 'predicate (lambda (x) (container? x))))

(define thing?
  (make-type 'thing (list thing:location)))
(set-predicate<=! thing? object?)

(define make-thing
  (type-instantiator thing?))

(define get-location
  (property-getter thing:location thing?))

(define-generic-procedure-handler set-up! (match-args thing?)
  (lambda (super thing)
    (super thing)
    (add-thing! (get-location thing) thing)))

(define-generic-procedure-handler tear-down! (match-args thing?)
  (lambda (super thing)
    (remove-thing! (get-location thing) thing)
    (super thing)))

(define-generic-procedure-handler send-message!
  (match-args message? thing?)
  (lambda (message thing)
    #f))

;;; Signs

(define sign:message
  (make-property 'message
                 'predicate string?))

(define sign?
  (make-type 'sign (list sign:message)))
(set-predicate<=! sign? thing?)

(define make-sign
  (type-instantiator sign?))

(define get-message
  (property-getter sign:message sign?))

(define set-message!
  (property-setter sign:message sign? string?))

(define (read-sign! sign-name actor)
  (let ((sign (find-object-by-name sign-name (all-things-in-place (get-location actor)))))
    (if (not sign)
        (tell! '("No such sign here.") actor)
        (if (not (sign? sign))
            (tell! '("That is not a sign.") actor)
            (tell! (list "It says" (get-message sign)) actor)))))

;;; Containers

(define container:things
  (make-property 'things
                 'predicate (is-list-of thing?)
                 'default-value '()))

(define container?
  (make-type 'container (list container:things)))
(set-predicate<=! container? object?)

(define get-things
  (property-getter container:things container?))

(define add-thing!
  (property-adder container:things container? thing?))

(define remove-thing!
  (property-remover container:things container? thing?))

;;; Exits

(define exit:from
  (make-property 'from
                 'predicate (lambda (x) (place? x))))

(define exit:to
  (make-property 'to
                 'predicate (lambda (x) (place? x))))

(define exit:direction
  (make-property 'direction
                 'predicate direction?))

(define exit?
  (make-type 'exit (list exit:from exit:to exit:direction)))
(set-predicate<=! exit? object?)

(define make-exit
  (type-instantiator exit?))

(define get-from
  (property-getter exit:from exit?))

(define get-to
  (property-getter exit:to exit?))

(define get-direction
  (property-getter exit:direction exit?))

(define-generic-procedure-handler set-up! (match-args exit?)
  (lambda (super exit)
    (super exit)
    (add-exit! (get-from exit) exit)))

;;; Lockable exits
(define lockable-exit:locked
  (make-property 'locked
                 'predicate boolean?
                 'default-value #t))

(define lockable-exit?
  (make-type 'lockable-exit (list lockable-exit:locked)))
(set-predicate<=! lockable-exit? exit?)

(define make-lockable-exit
  (type-instantiator lockable-exit?))

(define get-locked
  (property-getter lockable-exit:locked lockable-exit?))

(define set-locked!
  (property-setter lockable-exit:locked lockable-exit? boolean?))

;;; Places

(define place:vistas
  (make-property 'vistas
                 'predicate (lambda (x)
                              (and (n:list? x) (every place? x)))
                 'default-value '()))

(define place:exits
  (make-property 'exits
                 'predicate (lambda (x)
                              (and (n:list? x) (every place? x)))
                 'default-value '()))

(define place?
  (make-type 'place (list place:vistas place:exits)))
(set-predicate<=! place? container?)

(define make-place
  (type-instantiator place?))

(define get-vistas
  (property-getter place:vistas place?))

(define add-vista!
  (property-adder place:vistas place? place?))

(define get-exits
  (property-getter place:exits place?))

(define add-exit!
  (property-adder place:exits place? exit?))

(define (find-exit-in-direction direction place)
  (find (lambda (exit)
          (eqv? (get-direction exit) direction))
        (get-exits place)))

(define (people-in-place place)
  (filter person? (get-things place)))

(define (things-in-place place)
  (remove person? (get-things place)))

(define (all-things-in-place place)
  (append (things-in-place place)
          (append-map get-things (people-in-place place))))

(define (takeable-things place)
  (append (filter mobile-thing? (things-in-place place))
          (append-map get-things (people-in-place place))))

(define-generic-procedure-handler send-message!
  (match-args message? place?)
  (lambda (message place)
    (for-each (lambda (person)
                (send-message! message person))
              (people-in-place place))))

;;; Mobile things

(define mobile-thing:origin
  (make-property 'origin
                 'predicate place?
                 'default-to-property thing:location))

(define mobile-thing?
  (make-type 'mobile-thing (list mobile-thing:origin)))
(set-predicate<=! mobile-thing? thing?)

(define make-mobile-thing
  (type-instantiator mobile-thing?))

(define set-location!
  (property-setter thing:location mobile-thing? container?))

(define get-origin
  (property-getter mobile-thing:origin mobile-thing?))

(define enter-place!
  (chaining-generic-procedure 'enter-place! 1
    (constant-generic-procedure-handler #f)))

(define leave-place!
  (most-specific-generic-procedure 'leave-place! 1
    (constant-generic-procedure-handler #f)))

;;; Unstealable things

(define unstealable-thing?
  (make-type 'unstealable-thing '()))
(set-predicate<=! unstealable-thing? mobile-thing?)

;;; Keys
(define (simple-pair? x) (and (pair? x) (not (list? x))))

(define key:locations
  (make-property 'locations
                 'predicate simple-pair?))

(define key?
  (make-type 'key (list key:locations)))
(set-predicate<=! key? unstealable-thing?)

(define make-key
  (type-instantiator key?))

(define get-locations
  (property-getter key:locations key?))

(define (key-opens-exit? key exit)
  (or (equal? (get-locations key) (cons (get-to exit) (get-from exit)))
      (equal? (get-locations key) (cons (get-from exit) (get-to exit)))))

(define (can-open-exit? actor exit)
  (if (find (lambda (thing)
              (and (key? thing)
                   (key-opens-exit? thing exit)))
            (get-things actor))
      #t #f))

;;; Weapons

(define weapon:damage
  (make-property 'damage
                 'predicate integer?))

(define weapon:accuracy
  (make-property 'accuracy
                 'predicate number?))

(define weapon?
  (make-type 'weapon (list weapon:damage weapon:accuracy)))
(set-predicate<=! weapon? unstealable-thing?)

(define make-weapon
  (type-instantiator weapon?))

(define get-damage
  (property-getter weapon:damage weapon?))

(define get-accuracy
  (property-getter weapon:accuracy weapon?))

;;; Armor

(define armor:evasion
  (make-property 'evasion
                 'predicate number?))

(define armor:resistance
  (make-property 'resistance
                 'predicate number?))

(define armor?
  (make-type 'armor (list armor:evasion armor:resistance)))
(set-predicate<=! armor? unstealable-thing?)

(define make-armor
  (type-instantiator armor?))

(define get-evasion
  (property-getter armor:evasion armor?))

(define get-resistance
  (property-getter armor:resistance armor?))

(define (get-armor person)
  (find armor? (get-things person)))

;;; People

(define max-health 10)

(define person:health
  (make-property 'health
                 'predicate n:exact-integer?
                 'default-value max-health))

(define person:bag
  (make-property 'bag
                 'predicate (lambda (x) (bag? x))
                 'default-supplier
                 (lambda () (make-bag 'name 'my-bag))))

(define person?
  (make-type 'person (list person:health person:bag)))
(set-predicate<=! person? mobile-thing?)

(define get-health
  (property-getter person:health person?))

(define set-health!
  (property-setter person:health person? any-object?))

(define get-bag
  (property-getter person:bag person?))

(define-generic-procedure-handler set-up! (match-args person?)
  (lambda (super person)
    (super person)
    (set-holder! (get-bag person) person)))

(define-generic-procedure-handler get-things (match-args person?)
  (lambda (person)
    (get-things (get-bag person))))

(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (narrate! (list person "enters" (get-location person))
              person)
    (let ((people (people-here person)))
      (if (n:pair? people)
          (say! person (cons "Hi" people))))))

(define (when-alive callback)
  (lambda (person)
    (if (n:> (get-health person) 0)
        (callback person))))

(define (people-here person)
  (delv person (people-in-place (get-location person))))

(define (things-here person)
  (things-in-place (get-location person)))

(define (vistas-here person)
  (get-vistas (get-location person)))

(define (exits-here person)
  (get-exits (get-location person)))

(define (peoples-things person)
  (append-map get-things (people-here person)))

(define (suffer! hits person)
  (guarantee n:exact-positive-integer? hits)
  (set-health! person (max 0 (- (get-health person) hits)))
  (say! person (list "Ouch!" hits "hits is more than I want! I'm down to" (get-health person) "of" max-health "HP"))
  (if (< (get-health person) 1)
      (die! person)))

(define (die! person)
  (for-each (lambda (thing)
              (drop-thing! thing person))
            (get-things person))
  (announce!
   '("An earth-shattering, soul-piercing scream is heard..."))
  (narrate! (list (get-name person) "is no more...") person)
  (set-health! person 0)
  (move! person (get-heaven) person))

(define (resurrect! person health)
  (guarantee n:exact-positive-integer? health)
  (set-health! person health)
  (move! person (get-origin person) person))

;;; Combat

(define (rng chance) (< (random 1.0) chance))

(define (attack! target-name weapon-name actor)
  (if (equal? heaven (get-location actor))
      (tell! '("No fighting in heaven!") actor)
      (let ((target (find-object-by-name target-name (people-here actor)))
            (weapon (find-object-by-name weapon-name (get-things actor))))
        (if (not target)
            (tell! (list "No such person here") actor)
            (cond ((eqv? weapon-name 'fist)
                   (do-attack! target 'fist actor 1 0.8))
                  ((weapon? weapon)
                   (do-attack! target (get-name weapon) actor (get-damage weapon) (get-accuracy weapon)))
                  (else
                   (tell! (list "No such weapon") actor)))))))

(define (do-attack! target weapon-name actor damage accuracy)
  (narrate! (list (get-name actor) "attacks" (get-name target) "with their" weapon-name) actor)
  (if (not (rng accuracy))
      (narrate! (list (get-name actor) "misses!") actor)
      (let ((armor (get-armor target)))
        (if (or (and (not armor)
                     (rng 0.3))
                (and armor
                     (rng (get-evasion armor))))
            (narrate! (list (get-name target) "manages to dodge out of the way!") actor)
            (if (and armor
                     (rng (get-resistance armor)))
                (narrate! (list (get-name target) "is protected by their armor!") actor)
                (begin
                  (narrate! (list (get-name actor) "hits!") actor)
                  (suffer! damage target))))))
  #t)

(define (rest! actor)
  (if (= (get-health actor) max-health)
      (begin
        (tell! '("You're already at full health.") actor)
        #f)
      (begin
        (if (rng 0.5)
            (begin
              (set-health! actor (min max-health (+ 1 (get-health actor))))
              (say! actor (list "Ah... I'm up to" (get-health actor) "of" max-health "HP")))
            (tell! '("You feel your strength slowly returning.") actor))
        #t)))

;;; Bags

(define bag:holder
  (make-property 'holder
                 'predicate
                 (lambda (x) (or (not x) (person? x)))
                 'default-value #f))

(define bag?
  (make-type 'bag (list bag:holder)))
(set-predicate<=! bag? container?)

(define make-bag
  (type-instantiator bag?))

(define get-holder
  (property-getter bag:holder bag?))

(define set-holder!
  (property-setter bag:holder bag? person?))

;;; Autonomous people (non-player characters)

(define autonomous-agent:restlessness
  (make-property 'restlessness
                 'predicate bias?))

(define autonomous-agent:acquisitiveness
  (make-property 'acquisitiveness
                 'predicate bias?))

(define autonomous-agent:aggressiveness
  (make-property 'aggressiveness
                 'predicate bias?))

(define autonomous-agent?
  (make-type 'autonomous-agent
             (list autonomous-agent:restlessness
                   autonomous-agent:acquisitiveness
                   autonomous-agent:aggressiveness)))
(set-predicate<=! autonomous-agent? person?)

(define get-restlessness
  (property-getter autonomous-agent:restlessness
                   autonomous-agent?))

(define get-acquisitiveness
  (property-getter autonomous-agent:acquisitiveness
                   autonomous-agent?))

(define get-aggressiveness
  (property-getter autonomous-agent:aggressiveness
                   autonomous-agent?))

(define-generic-procedure-handler set-up!
  (match-args autonomous-agent?)
  (lambda (super agent)
    (super agent)
    (register-with-clock! agent (get-clock))))

(define-generic-procedure-handler tear-down!
  (match-args autonomous-agent?)
  (lambda (super agent)
    (unregister-with-clock! agent (get-clock))
    (super agent)))

(define (do-autonomous-actions! agent)
  (let ((to-take (random-choice (takeable-things-here agent)))
        (to-attack (random-choice (people-here agent))))
    (cond ((equal? (get-name (get-location agent)) 'heaven))
          ((and to-take
                (flip-coin (get-acquisitiveness agent)))
           (take-thing! to-take agent))
          ((and to-attack
                (flip-coin (get-aggressiveness agent)))
           (attack! (get-name to-attack)
                    (best-weapon-name agent)
                    agent))
          ((flip-coin (get-restlessness agent))
           (move-somewhere! agent))
          (else (rest! agent)))))

(define (takeable-things-here agent)
  (append (filter (lambda (object)
                    (and (or (not (get-armor agent))
                             (not (armor? object)))
                         (mobile-thing? object)))
                  (things-here agent))
          (filter (lambda (object)
                    (not (unstealable-thing? object)))
                  (peoples-things agent))))

(define (best-weapon-name agent)
  (let ((weapons (filter weapon? (get-things agent))))
    (if (equal? weapons '())
        'fist
        (get-name (car (sort weapons
                             (lambda (a b)
                               (> (* (get-damage a)
                                     (get-accuracy a))
                                  (* (get-damage b)
                                     (get-accuracy b))))))))))

(define (move-somewhere! agent)
  (let ((exit (random-choice (exits-here agent))))
    (if exit
        (take-exit! exit agent))))

(define-clock-handler autonomous-agent? do-autonomous-actions!)

;;; Students

(define student?
  (make-type 'student '()))
(set-predicate<=! student? autonomous-agent?)

(define make-student
  (type-instantiator student?))

;;; Merchant

(define merchant?
  (make-type 'merchant '()))
(set-predicate<=! merchant? autonomous-agent?)

(define make-merchant
  (type-instantiator merchant?))

;;; House masters

(define house-master:irritability
  (make-property 'irritability
                 'predicate bias?))

(define house-master?
  (make-type 'house-master (list house-master:irritability)))
(set-predicate<=! house-master? autonomous-agent?)

(define make-house-master
  (type-instantiator house-master?))

(define get-irritability
  (property-getter house-master:irritability house-master?))

(define (irritate-students! master)
  (let ((students (filter student? (people-here master)))
        (in-heaven (equal? (get-name (get-location master)) 'heaven)))
    (if (and (flip-coin (get-irritability master))
             (not in-heaven))
        (if (n:pair? students)
            (begin
              (say! master
                    '("What are you doing still up?"
                      "Everyone back to their rooms!"))
              (for-each (lambda (student)
                          (narrate! (list student "goes home to"
                                          (get-origin student))
                                    student)
                          (move! student
                                 (get-origin student)
                                 student))
                        students))
            (say! master
                  '("Grrr... When I catch those students...")))
        (if (and (n:pair? students)
                 (not in-heaven))
            (say! master
                  '("I'll let you off this once..."))))))

(define-clock-handler house-master? irritate-students!)

;;; Trolls

(define troll:hunger
  (make-property 'hunger
                 'predicate bias?))

(define troll?
  (make-type 'troll (list troll:hunger)))
(set-predicate<=! troll? autonomous-agent?)

(define make-troll
  (type-instantiator troll?))

(define get-hunger
  (property-getter troll:hunger troll?))

(define (eat-people! troll)
  (if (and (flip-coin (get-hunger troll))
           (not (equal? (get-name (get-location troll)) 'heaven)))
      (let ((people (people-here troll)))
        (if (n:null? people)
            (narrate! (list (possessive troll) "belly rumbles")
                      troll)
            (let ((victim (random-choice people)))
              (narrate! (list troll "takes a bite out of" victim)
                        troll)
              (suffer! (random-number 3) victim))))))

(define-clock-handler troll? eat-people!)

;;; Avatars

(define avatar:screen
  (make-property 'screen
                 'predicate screen?))

(define avatar?
  (make-type 'avatar (list avatar:screen)))
(set-predicate<=! avatar? person?)

(define make-avatar
  (type-instantiator avatar?))

(define get-screen
  (property-getter avatar:screen avatar?))

(define-generic-procedure-handler send-message!
  (match-args message? avatar?)
  (lambda (message avatar)
    (send-message! message (get-screen avatar))))

(define-generic-procedure-handler enter-place!
  (match-args avatar?)
  (lambda (super avatar)
    (super avatar)
    (look-around avatar)
    (tick! (get-clock))))

(define (look-around avatar)
  (tell! (list "You are in" (get-location avatar))
         avatar)
  (let ((my-things (get-things avatar)))
    (if (n:pair? my-things)
        (tell! (cons "Your bag contains:" my-things)
               avatar)))
  (let ((things
         (append (things-here avatar)
                 (people-here avatar))))
    (if (n:pair? things)
        (tell! (cons "You see here:" things)
               avatar)))
  (let ((vistas (vistas-here avatar)))
    (if (n:pair? vistas)
        (tell! (cons "You can see:" vistas)
               avatar)))
  (tell! (let ((exits (exits-here avatar)))
           (if (n:pair? exits)
               (cons "You can exit:"
                     (map get-direction exits))
               '("There are no exits..."
                 "you are dead and gone to heaven!")))
         avatar))

;;; Motion

(define (take-thing! thing person)
  (move! thing (get-bag person) person))

(define (drop-thing! thing person)
  (move! thing (get-location person) person))

(define (take-exit! exit mobile-thing)
  (if (or (not (lockable-exit? exit))
          (not (get-locked exit)))
      (generic-move! mobile-thing
                     (get-from exit)
                     (get-to exit)
                     mobile-thing)
      (tell! (list "It's locked. Maybe there's a key somewhere...") mobile-thing)))

(define (move! thing destination actor)
  (generic-move! thing
                 (get-location thing)
                 destination
                 actor))

(define generic-move!
  (most-specific-generic-procedure 'generic-move! 4 #f))

;;; TODO: guarantee that THING is in FROM.
;;; Also that the people involved are local.

;; coderef: generic-move:default
(define-generic-procedure-handler generic-move!
  (match-args thing? container? container? person?)
  (lambda (thing from to actor)
    (tell! (list thing "is not movable")
           actor)))

;; coderef: generic-move:steal
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from))
          (new-holder (get-holder to)))
      (cond ((unstealable-thing? mobile-thing)
             (tell! '("You can't find a way to take that from them.") actor))
            ((eqv? from to)
             (tell! (list new-holder "is already carrying"
                          mobile-thing)
                    actor))
            ((eqv? actor former-holder)
             (narrate! (list actor
                             "gives" mobile-thing
                             "to" new-holder)
                       actor))
            ((eqv? actor new-holder)
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder)
                       actor))
            (else
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder
                             "and gives it to" new-holder)
                       actor)))
      (if (not (unstealable-thing? mobile-thing))
          (begin
            (if (not (eqv? actor former-holder))
                (say! former-holder (list "Yaaaah! I am upset!")))
            (if (not (eqv? actor new-holder))
                (say! new-holder (list "Whoa! Where'd you get this?")))
            (if (not (eqv? from to))
                (move-internal! mobile-thing from to)))))))

;; coderef: generic-move:take
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? place? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((new-holder (get-holder to)))
      (cond ((and (armor? mobile-thing) (get-armor new-holder))
             (tell! '("You're already wearing armor! You can't wear another set!") new-holder))
            ((eqv? actor new-holder)
             (narrate! (list actor
                             "picks up" mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             "picks up" mobile-thing
                             "and gives it to" new-holder)
                       actor)))
      (if (not (and (armor? mobile-thing) (get-armor new-holder)))
          (begin
            (if (not (eqv? actor new-holder))
                (say! new-holder (list "Whoa! Thanks, dude!")))
            (move-internal! mobile-thing from to))))))

;; coderef: generic-move:drop
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? place? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from)))
      (cond ((eqv? actor former-holder)
             (narrate! (list actor
                             "drops" mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder
                             "and drops it")
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder
                (list "What did you do that for?")))
      (move-internal! mobile-thing from to))))

(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? place? place? person?)
  (lambda (mobile-thing from to actor)
    (cond ((eqv? from to)
           (tell! (list mobile-thing "is already in" from)
                  actor))
          (else
           (tell! (list "How do you propose to move"
                        mobile-thing
                        "without carrying it?")
                  actor)))))

;; coderef: generic-move:person
(define-generic-procedure-handler generic-move!
  (match-args person? place? place? person?)
  (lambda (person from to actor)
    (let ((exit (find-exit from to)))
      (cond ((or (eqv? from (get-heaven))
                 (eqv? to (get-heaven)))
             (move-internal! person from to))
            ((not exit)
             (tell! (list "There is no exit from" from
                          "to" to)
                    actor))
            ((eqv? person actor)
             (narrate! (list person "leaves via the"
                             (get-direction exit) "exit")
                       from)
             (move-internal! person from to))
            (else
             (tell! (list "You can't force"
                          person
                          "to move!")
                    actor))))))

(define (find-exit from to)
  (find (lambda (exit)
          (and (eqv? (get-from exit) from)
               (eqv? (get-to exit) to)))
        (get-exits from)))

(define (move-internal! mobile-thing from to)
  (leave-place! mobile-thing)
  (remove-thing! from mobile-thing)
  (set-location! mobile-thing to)
  (add-thing! to mobile-thing)
  (enter-place! mobile-thing)
  #t)
