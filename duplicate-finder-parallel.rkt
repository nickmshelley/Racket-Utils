#lang racket
(require file/md5)

(define (find-duplicates directory)
  (define file-hash (make-hash))
  (for ([f (directory-list directory)])
    (define full-path (string-append directory "/" (path->string f)))
    (if (directory-exists? full-path)
        (let ([p (place ch
                   (define directory (place-channel-get ch))
                   (define file-hash (find-duplicates directory))
                   (place-channel-put (hash->list file-hash)))])
          (place-channel-put p full-path)
          (combine-hash! file-hash (place-channel-get p)))
        (add-or-append! file-hash
                        (md5 (file->bytes full-path))
                        full-path)))
  file-hash)

(define (print-duplicates file-hash)
  (for ([l (hash-values file-hash)])
    (when (> (length l) 1)
      (printf "duplicates:~n")
      (for ([p l])
        (printf "\t~a~n" p))
      (newline))))

(define (add-or-append! h k v)
  (if (hash-has-key? h k)
      (hash-set! h k (append (hash-ref h k) (list v)))
      (hash-set! h k (list v))))

(define (combine-hash! h assocs)
  (for ([p assocs])
    (add-or-append! h (car assocs) (cdr assocs))))
        

(define (main)
  (define p
    (place ch
      (define directory (place-channel-get ch))
      (define file-hash (find-duplicates directory))
      (place-channel-put (hash->list file-hash))))
  
  (place-channel-put p "/Users/heather/Pictures/Nick's Pictures/random pics")
  
  (print-duplicates (make-hash (place-channel-get p))))

(main)