#!/usr/bin/guile \
--no-auto-compile -e main -s
!#

;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements.  See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership.  The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License.  You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing,
;; software distributed under the License is distributed on an
;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;; KIND, either express or implied.  See the License for the
;; specific language governing permissions and limitations
;; under the License.

(import (rnrs base)
        (rnrs io ports)
        (rnrs io simple)
        (ice-9 getopt-long)
        (ice-9 ftw)
        (ice-9 format))

;; GLOBAL
;; -----------------------------------------------------------------------

(define home (getenv "HOME"))
(define version "0.0.1")

;; IGNORED FILE
;; -----------------------------------------------------------------------

(define ignored-filename ".dutignore")

;; ignore file is provided by user?
(define (ignored-file-exist? file)
  (file-exists? file))

;; TARGET ignored file absolute path
(define (ignored-file-path target)
  (string-append target "/" ignored-filename))

;; return all listed files to be ignored in .dutignore
(define (ignored-files-found target)
  (string-split (call-with-input-file (ignored-file-path target) get-string-all)
                #\newline))

;; default files to be ignored
(define ignored-files-default '(".git" ".dutignore"))

(define (ignored-files target)
  (let ((files (ignored-files-found target)))
    (map (lambda (i) (if (not (member i files))
                    (cons i files)))
         files)
    files))

;; MIDDLEWARE
;; -----------------------------------------------------------------------

;; Folder residing all dotfiles to link.
(define (target-get options)
  (canonicalize-path (cdr (assv 'from options))))

;; Destination folder, defaults to $HOME.
(define (destination-get options)
  (let ((dest (if (assq 'to options)
                  (cdr (assv 'to options))
                  home)))

    ;; create DEST if it does not exist yet.
    (unless (file-exists? dest)
      (mkdir dest))

    (canonicalize-path dest)))

;; return: string
;; remove target from current filename "/target/filename" -> "filename"
(define (target-remove filename target)
  (let* ((target-length (if (string-ci= target filename)
                            (string-length target) ;; if filename is target return without heading /
                            (+ 1 (string-length target)))))
    (string-replace filename "" 0 target-length)))

;; returns TARGET/.config/meh/FILENAME to $HOME/.config/meh/FILENAME
(define (target-to-home filename)
  (string-append home "/" filename))

;; is FILENAME listed in .dutignore?
(define (target-ignore? file target) ;; FIX: not ignore .git folder
  (member #t (map (lambda (ignore-file)
                    (unless (string-null? ignore-file)
                      ;; (newline) (display (format #f "i: ~a -> f: ~a ? ~a n: ~a" ignore-file file (string-prefix? ignore-file file) (string-null? ignore-file)))
                      (string-prefix? ignore-file file)))
                  (ignored-files target))))

;; walk recursively through the TARGET folder and run ACTION.
(define (walk target action)
  (ftw target
       (lambda (current-filename statinfo flag)
         (let* ((file (target-remove current-filename target)))
           (unless (target-ignore? file target)
             (action current-filename file))
           #t))))

;; ACTIONS
;; -----------------------------------------------------------------------

(define (create options)
  ;; link file or create directory.
  (define (link-process source link)
    (if (file-is-directory? source)
        (unless (file-exists? link)
          (begin (display (format #f "\nCreating directory: ~a" link))
                 (mkdir link)))
        (unless (file-exists? link)
          (begin
            (display (format #f "\nCreating link: ~a" link))
            (symlink source link)))))

  (walk (target-get options)
        (lambda (source link)
          (link-process source
                        (link-destined (destination-get options) link)))))

(define (remove options)
  (walk (target-get options)
        (lambda (source link)
          (let ((linked (link-destined (destination-get options) link)))
            (unless (file-is-directory? linked)
              (delete-file linked))))))

(define (pretend options)
  (display 'dryrunning))

(define (overwrite options)
  (display 'overwriting))

(define (info options)
  (let ((target (target-get options)))
    (display (format #f "target: ~a" target))
    (newline)
    (display (format #f "destination: ~a" (destination-get options)))
    (newline)
    (display (format #f "ignore: ~a" (string-join (ignored-files target) " ")))))

;; HELPERS
;; -----------------------------------------------------------------------

;; if -to flag is provided, append DESTINE to LINK.
(define (link-destined destine link)
  (if (string-null? destine)
        link
        (string-append destine "/" link)))

;; CLI PARSING
;; -----------------------------------------------------------------------

(define (cli-usage-banner)

  (display "dut [options]
  -t DIR, --to DIR      destination folder to deliver links
  -f DIR, --from DIR    target folder with dotfiles
  -c, --create          create links of dotfiles
  -r, --remove          remove links from target folder
  -p, --pretend         demonstrate files linking
  -o, --overwrite       overwrite existent links
  -i, --info            provide additional information
  -v, --version         display version
  -h, --help            display this help"))

(define cli-option-list '((version     (single-char #\v) (value #f))
                      (create    (single-char #\c) (value #f))
                      (remove    (single-char #\r) (value #f))
                      (pretend   (single-char #\p) (value #f))
                      (overwrite (single-char #\o) (value #f))
                      (info      (single-char #\i) (value #f))
                      (to        (single-char #\t) (value optional))
                      (from      (single-char #\f) (value #t))
                      (help      (single-char #\h) (value #f))))

(define (cli-parser args)
  (let* ((option-spec cli-option-list)
         (options (getopt-long args option-spec)))
    (cli-option-run options)))

(define (cli-option-run options)
  (let ((option-wanted (lambda (option) (option-ref options option #f))))
    (cond ((option-wanted 'version)   (display version))
          ((option-wanted 'create)    (create options))
          ((option-wanted 'remove)    (remove options))
          ((option-wanted 'pretend)   (pretend options))
          ((option-wanted 'overwrite) (overwrite options))
          ((option-wanted 'info)      (info options))
          ((option-wanted 'help)      (cli-usage-banner))
          (else                       (cli-usage-banner)))))

;; MAIN
;; -----------------------------------------------------------------------

(define (main args)
  (cli-parser args))
