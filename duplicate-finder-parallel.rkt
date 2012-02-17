#lang racket
(require file/md5)

(provide main)

(define (find-duplicates directory)
  (define file-hash (make-hash))
  (define place-list empty)
  (for ([f (directory-list directory)])
    (define full-path (string-append directory "/" (path->string f)))
    (if (directory-exists? full-path)
        (let ([p (place ch
                   (define directory (place-channel-get ch))
                   (define file-hash (find-duplicates directory))
                   (place-channel-put ch (hash->list file-hash)))])
          (place-channel-put p full-path)
          (set! place-list (cons p place-list)))
        (add-or-append! file-hash
                        (md5 (file->bytes full-path))
                        (list full-path))))
  (for ([p place-list])
    (define assocs (place-channel-get p))
    (combine-hash! file-hash assocs))
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
        

(define (main)
  (define p
    (place ch
      (define directory (place-channel-get ch))
      (define file-hash (find-duplicates directory))
      (place-channel-put ch (hash->list file-hash))))
  
  ;(place-channel-put p "/Users/heather/Pictures/Nick's Pictures/random pics")
  (place-channel-put p "/Users/heather/Pictures/Nick's Pictures/Nick's iPhoto Pictures")
  
  (print-duplicates (make-hash (place-channel-get p))))