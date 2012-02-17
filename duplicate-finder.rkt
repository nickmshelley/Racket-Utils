#lang racket
(require file/md5)

(define (find-duplicates file-hash directory)
  (for ([f (directory-list directory)])
    (define full-path (string-append directory "/" (path->string f)))
    (if (directory-exists? full-path)
        (find-duplicates file-hash full-path)
        (let ([h (md5 (file->bytes full-path))])
          (if (hash-has-key? file-hash h)
              (hash-set! file-hash h (append (hash-ref file-hash h) (list full-path)))
              (hash-set! file-hash h (list full-path))))))
  file-hash)

(define (print-duplicates file-hash)
  (for ([l (hash-values file-hash)])
    (when (> (length l) 1)
      (printf "duplicates:~n")
      (for ([p l])
        (printf "\t~a~n" p))
      (newline))))

(print-duplicates (find-duplicates (make-hash) "/Users/heather/Pictures/Nick's Pictures/random pics"))