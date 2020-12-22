#lang racket

(require racket/hash)

(define (parse-food line)
  (match-let ([(list ings allegs) (string-split line " (contains ")])
    `#hash((ings . ,(list->set (string-split ings)))
           (allegs . ,(list->set
                       (string-split
                        (first (string-split allegs ")"))
                        ", "))))))

(define (parse-file file-path)
  (for/vector ([line (file->lines file-path)])
    (parse-food line)))

(define (count-ing-alleg ing alleg foods)
  (length
   (for/list ([food foods]
              #:when
              (and
               (set-member? (hash-ref food 'ings) ing)
               (set-member? (hash-ref food 'allegs) alleg)))
     food)))

(define (potential-allegs-2 foods)
  (let ([uniq-allegs (for*/set ([food foods]
                                [alleg (hash-ref food 'allegs)])
                       alleg)])
    (for/vector ([alleg uniq-allegs])
      (cons alleg (for/set ([ing (let ([ings (for/list
                                                 ([food foods]
                                                  #:when
                                                  (set-member? (hash-ref food 'allegs) alleg))
                                               (hash-ref food 'ings))])
                                   (foldl set-intersect (car ings) (cdr ings)))])
                    ing)))))

(define (potential-allegs foods)
  (let ([uniq-allegs (for*/set ([food foods]
                                [alleg (hash-ref food 'allegs)])
                       alleg)])
    (for*/set ([alleg uniq-allegs]
               [ing (let ([ings (for/list
                                    ([food foods]
                                     #:when
                                     (set-member? (hash-ref food 'allegs) alleg))
                                  (hash-ref food 'ings))])
                      (foldl set-intersect (car ings) (cdr ings)))])
      ing)))

(define (not-allegs foods)
  (let ([uniq-ings (for*/set ([food foods]
                              [ing (hash-ref food 'ings)])
                     ing)])
    (set-subtract
     uniq-ings
     (potential-allegs foods))))

(define (part-1 foods)
  (length
   (for*/list ([not-alleg (not-allegs foods)]
               [food foods]
               [ing (hash-ref food 'ings)]
               #:when (equal? not-alleg ing))
     ing)))

(define (part-2 foods)
  (let* ([xs (vector-sort
              (potential-allegs-2 foods)
              #:key (lambda (x) (set-count (cdr x))) <)]
         [n (vector-length xs)])
    ;; (for ([i (in-range 0 (- n 1))])
    ;;   (for ([j (in-range (+ i 1) n)])
    ;;     (let ([x (car (set->list (vector-ref xs i)))])
    ;;       (vector-set! xs j
    ;;                    (set-remove (vector-ref xs j) x)))))
    xs)
  )

(define (solve-file file-path)
  (printf "Input file: ~a\n" file-path)
  (let ([foods (parse-file file-path)])
    (printf "Part 1: ~a\n" (part-1 foods))
    (printf "Part 2: ~a\n" (part-2 foods))))

(for ([arg (current-command-line-arguments)])
  (solve-file arg))

