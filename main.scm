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
        (ice-9 getopt-long))

(define home (getenv "HOME"))
(define (version) (display "0.0.1"))

(define dutignore-filename ".dutignore")

(define (dutignore-exist? file)
  (file-exists? file))

(define (dutignore root)
  (string-append root "/" dutignore-filename))

(define (ignored-files root)
  (let* ((listed (string-split
                  (call-with-input-file (dutignore root) get-string-all)
                  #\newline))
         (all (cons ".git" (cons ".dutignore" listed))))
    (map (lambda (file)(string-append root "/" file)) all)))

;; Folder residing all dotfiles to link.
(define (get-target options)
  (canonicalize-path (cdr (assv 'from options))))

;; Destination folder, defaults to $HOME.
(define (get-destination options)
  (if (assq 'to options)
      (canonicalize-path (cdr (assv 'to options)))
      home))

;; ACTIONS

(define (create) (display 'creating))

(define (remove) (display 'remove))

(define (pretend) (display 'dryrunning))

(define (overwrite) (display 'overwriting))

(define (info options)
  (display (string-append "target: " (get-target options)))
  (newline)
  (display (string-append "destination: " (get-destination options)))
  (newline)
  (display (string-append "dutignore: " (dutignore (get-target options)))))

;; CLI PARSING

(define (usage-options)
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

(define (cli-parser args root)
  (let* ((option-spec option-list)
         (options (getopt-long args option-spec)))
    (option-run options)))

(define (option-run options)
  (let ((option-wanted (lambda (option) (option-ref options option #f))))
    (cond ((option-wanted 'version)   (version))
          ((option-wanted 'help)      (usage-options))
          ((option-wanted 'create)    (create options))
          ((option-wanted 'remove)    (remove options))
          ((option-wanted 'pretend)   (pretend options))
          ((option-wanted 'overwrite) (overwrite options))
          ((option-wanted 'info)      (info options))
          (else                       (usage-options)))))

(define (main args)
  (let ((root (if (null? args)
                  (canonicalize-path (cadr args))
                  "")))
    (cli-parser args root)))
