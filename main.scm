#!/usr/bin/guile \
-e main -s
!#

(import (rnrs base)
        (rnrs io ports)
        (rnrs io simple)
        (ice-9 getopt-long))

(define home (getenv "HOME"))
(define (version) (display "0.0.1"))

(define dotsignore-filename ".dotsignore")

(define (dotsignore-exist? file)
  (file-exists? file))

(define (dotsignore root)
  (string-append root "/" dotsignore-filename))

(define (ignored-files root)
  (let* ((listed (string-split
                  (call-with-input-file (dotsignore root) get-string-all)
                  #\newline))
         (all (cons ".git" (cons ".dotsignore" listed))))
    (map (lambda (file)(string-append root "/" file)) all)))

(define (info root)
  (display (string-append "root: " root))
  (newline)
  (display (string-append
            "dotsignore: " (dotsignore root)
            " - found? " (if (dotsignore-exist? root) "yep" "nope"))))

(define (dryrun) (display 'dryrunning))
(define (symlink) (display 'symlinking))

(define (usage-options)
  (display "dots [options]
  -v, --version    Display version
  -s, --symlink    Deploy dotfiles symlinking
  -d, --dryrun     Mimic symlinking deployment
  -i, --info       Miscelleanous information
  -h, --help       Display this help"))

(define (cli-parser args root)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (symlink (single-char #\s) (value #f))
                        (info    (single-char #\i) (value #f))
                        (dryrun  (single-char #\d) (value #f))
                        (help    (single-char #\h) (value #f))))
         (options (getopt-long args option-spec)))
    (option-run options)))

(define (option-run options)
  (let ((option-wanted (lambda (option)
                         (option-ref options option #f))))
    (cond ((option-wanted 'version) (version))
          ((option-wanted 'help)    (usage-options))
          ((option-wanted 'symlink) (symlink))
          ((option-wanted 'dryrun)  (dryrun))
          ((option-wanted 'info)    (info root))
          (else                     (usage-options)))))

(define (main args)
  (let ((root (if (null? args)
                  (canonicalize-path (cadr args))
                  "")))
    (cli-parser args root)))
