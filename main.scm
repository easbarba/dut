#!/usr/bin/guile \
-e main -s
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
        (ice-9 ftw))

(define home (getenv "HOME"))
(define (version) (display "0.0.1"))

(define dutignore-filename ".dutignore")

(define (dutignore-exist? file)
  (file-exists? file))

(define (dutignore target)
  (string-append target "/" dutignore-filename))

(define (ignored-files target)
  (let* ((listed (string-split
                  (call-with-input-file (dutignore target) get-string-all)
                  #\newline))
         (all (cons ".git" (cons ".dutignore" listed))))
    all)) ;; (map (lambda (file)(string-append target "/" file)) all)

;; Folder residing all dotfiles to link.
(define (get-target options)
  (canonicalize-path (cdr (assv 'from options))))

;; Destination folder, defaults to $HOME.
(define (get-destination options)
  (if (assq 'to options)
      (canonicalize-path (cdr (assv 'to options)))
      home))

;; remove target string from current filename "/target/filename" -> "filename"
(define (remove-target filename options)
  (string-replace filename "" 0 (string-length (get-target options))))

;; ACTIONS

(define (create options)

  (ftw (get-target options) (lambda (filename statinfo flag)
                              (let ((filename-path (string-append (get-target options) filename)))
                                (if (not (member (string- filename) '(".git" ".svn" "CVS")))
                                  (begin (display (string-append (get-target options) filename))
                                         (newline))))
                              #t)))

(define (remove options) (display 'remove))

(define (pretend options) (display 'dryrunning))

(define (overwrite options) (display 'overwriting))

(define (info options)
  (display (string-append "target: " (get-target options)))
  (newline)
  (display (string-append "destination: " (get-destination options)))
  (newline)
  (display (string-append "dutignore: " (string-join (ignored-files (get-target options)) " "))))

;; CLI PARSING

(define (usage-banner)
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

(define option-list '((version     (single-char #\v) (value #f))
                        (create    (single-char #\c) (value #f))
                        (remove    (single-char #\r) (value #f))
                        (pretend   (single-char #\p) (value #f))
                        (overwrite (single-char #\o) (value #f))
                        (info      (single-char #\i) (value #f))
                        (to        (single-char #\t) (value optional))
                        (from      (single-char #\f) (value #t))
                        (help      (single-char #\h) (value #f))))

(define (cli-parser args target)
  (let* ((option-spec option-list)
         (options (getopt-long args option-spec)))
    (option-run options)))

(define (option-run options)
  (let ((option-wanted (lambda (option) (option-ref options option #f))))
    (cond ((option-wanted 'version)   (version))
          ((option-wanted 'create)    (create options))
          ((option-wanted 'remove)    (remove options))
          ((option-wanted 'pretend)   (pretend options))
          ((option-wanted 'overwrite) (overwrite options))
          ((option-wanted 'info)      (info options))
          ((option-wanted 'help)      (usage-banner))
          (else                       (usage-banner)))))

(define (main args)
  (let ((target (if (null? args)
                  (canonicalize-path (cadr args))
                  "")))
    (cli-parser args target)))
