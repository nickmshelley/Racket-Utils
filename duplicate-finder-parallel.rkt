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
  (printf "Starting directory: ~a~n" directory)
  (define file-hash (make-hash))
  (for ([f (directory-list directory)])
    (define full-path (string-append directory "/" (path->string f)))
    (unless (directory-exists? full-path)
      (add-or-append! file-hash
                      (call-with-input-file full-path sha1)
                      (list full-path))))
  (printf "\tFinished directory: ~a~n" directory)
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
    (define directory (place-channel-get ch))
    (define directories (get-directories directory))
    (place-channel-put ch directories)
    (define file-hash (find-duplicates directory))
    (place-channel-put ch (hash->list file-hash))))

(define (main)
  (define file-hash (make-hash))
  (define directory "/Users/heather/Pictures/Nick's Pictures/random pics")
  (define all-directories (get-all-sub-directories directory))
  (define total (add1 (length all-directories)))
  (define done 0)
  (define places
    (let loop ([directories (list directory)]
               [places empty])
      (define p (make-worker-place))
      (place-channel-put p (first directories))
      (define new-directories (append (rest directories) (place-channel-get p)))
      (if (empty? new-directories)
          (append places (list p))
          (if (> (length places) (* 5 (processor-count)))
              (begin
                (printf "Waiting...~n")
                (combine-hash! file-hash (place-channel-get (first places)))
                (set! done (add1 done))
                (printf "~n~a% done~n" (real->decimal-string (* 100. (/ done total))))
                (loop new-directories (append (rest places) (list p))))
              (loop new-directories (append places (list p)))))))
  (for ([p places])
    (define assocs (place-channel-get p))
    (combine-hash! file-hash assocs)
    (set! done (add1 done))
    (printf "~n~a% done~n" (real->decimal-string (* 100. (/ done total)))))
  (print-duplicates file-hash))