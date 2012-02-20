#lang racket
(require openssl/sha1)

(provide main)

(define (get-directories directory)
  (for/list ([f (directory-list directory)]
             #:when (directory-exists? 
                     (string-append directory "/" (path->string f))))
    (string-append directory "/" (path->string f))))

(define (get-all-sub-directories directory)
  (define directories (get-directories directory))
  (append directories (append-map get-all-sub-directories directories)))

(define (find-duplicates directory)
  (define file-hash (make-hash))
  (for ([f (directory-list directory)])
    (define full-path (string-append directory "/" (path->string f)))
    (unless (directory-exists? full-path)
      (add-or-append! file-hash
                      (call-with-input-file full-path sha1)
                      (list full-path))))
  file-hash)

(define (print-duplicates file-hash)
  (for ([l (hash-values file-hash)])
    (when (> (length l) 1)
      (printf "duplicates:~n")
      (for ([p l])
        (printf "\t~a~n" p))
      (newline))))

(define (add-or-append! h k val-list)
  (if (hash-has-key? h k)
      (hash-set! h k (append (hash-ref h k) val-list))
      (hash-set! h k val-list)))

(define (combine-hash! h assocs)
  (for ([p assocs])
    (add-or-append! h (first p) (rest p))))

(define (make-worker-place)
  (place ch
    (define my-num (place-channel-get ch))
    (let loop ()
      (define directory (place-channel-get ch))
      (printf "\t~a: processing ~a~n" my-num directory)
      (define file-hash (find-duplicates directory))
      (define message (cons my-num (hash->list file-hash)))
      (place-channel-put ch message)
      (loop))))

(define (hash-from-list l)
  (define file-hash (make-hash))
  (for* ([assocs l]
         [p assocs])
    (add-or-append! file-hash (first p) (rest p)))
  file-hash)

(define (main)
  (define results-list empty)
  (define directory "/Users/heather/Pictures/Nick's Pictures")
  (define all-directories (cons directory (get-all-sub-directories directory)))
  (define total (length all-directories))
  (define done 0)
  ;(define num-workers 1)
  (define num-workers (* 2 (processor-count)))
  (define places (for/list ([i (in-range num-workers)]
                            [dir all-directories])
                   (define p (make-worker-place))
                   (place-channel-put p i)
                   (place-channel-put p dir)
                   p))
  (define places-evt (apply choice-evt places))
  (define remaining-directories
    (if (> total (length places))
        (list-tail all-directories (length places))
        empty))
  (for ([dir remaining-directories])
    (define result (sync places-evt))
    (place-channel-put (list-ref places (first result)) dir)
    (set! results-list (cons (rest result) results-list))
    (set! done (add1 done))
    (printf "~a% done~n" (real->decimal-string (* 100. (/ done total)))))
  ;get remaining stuff
  (for ([_ (length places)])
    (define result (sync places-evt))
    (set! results-list (cons (rest result) results-list))
    (set! done (add1 done))
    (printf "~a% done~n" (real->decimal-string (* 100. (/ done total)))))
  (printf "~n~nPROCESSING RESULTS...~n~n")
  (print-duplicates (hash-from-list results-list)))