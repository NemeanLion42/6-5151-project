(define (make-line n char)
  (if (<= n 0)
      ""
      (string-append char (make-line (- n 1) char))))

(display (make-line 5 "-"))
(define (string-join lst separator)
  (if (null? lst)
      ""
      (let loop ((slst (cdr lst))
                 (s (car lst)))
        (if (null? slst)
            s
            (loop (cdr slst) (string-append s separator (car slst)))))))


(define (make-box width height content)
  (let ((border (string-append (make-line width "+") "\n"))
        (empty-line (string-append "|" (make-line (- width 2) " ") "|\n"))
        (content-line (string-append "|" content
                                      (make-line (- width (+ 2 (string-length content))) " ")
                                      "|\n")))
    (string-append border
                   (make-line (- height 2) empty-line)
                   content-line
                   (make-line (- height 2) empty-line)
                   border)))

(define tagged-store
  '((BB . ("black board" . "S"))
    (AH . ("alyssa hacker" . "S"))))
(define (lookup-tag tag store)
  (let ((entry (assoc tag store)))
    (if entry
        (cdr entry)
        (error "Tag not found"))))

(define (add-tag tag value property store)
  (cons (cons tag (cons value property)) store))

(define (remove-tag tag store)
  (remove tag store eq?))

(define tagged-store
  '((BB . ("black board" . "S"))
    (AH . ("alyssa hacker" . "S"))))

(define (process-unit input-list)
  (let ((width 34)
        (height 10))
    (if (null? input-list)
        (make-box width height (string-append "|" (make-line (- width 2) " ") "|\n"))
        (let* ((content (map (lambda (tag) (car (lookup-tag tag tagged-store))) input-list))
               (content-string (string-join content " "))
               (content-line (string-append "|" content-string
                                             (make-line (- width (+ 2 (string-length content-string))) " ")
                                             "|\n")))
          (make-box width height content-line)))))



(define (lookup-tag tag store)
  (let ((entry (assoc tag store)))
    (if entry
        (cdr entry)
        (error "Tag not found"))))

(define (add-tag tag value property store)
  (cons (cons tag (cons value property)) store))

(define (remove-tag tag store)
  (filter (lambda (entry) (not (eq? tag (car entry)))) store))

(define new-store
  (add-tag 'CC "cool code" "M" tagged-store))

;(display (lookup-tag 'BB new-store))
;(newline)

;(display (lookup-tag 'CC new-store))
;(newline)

(define new-store2
  (remove-tag 'CC new-store))

;(display (lookup-tag 'BB new-store2))
;(newline)

(define (main)
  (display (process-unit '(BB AH))))

(display "\n")



(main)
