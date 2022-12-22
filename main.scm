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
        (ice-9 ftw)
        (ice-9 format))

(define home (getenv "HOME"))
(define (version) (display "0.0.1"))

(define dutignore-filename ".dutignore")

(define (dutignore-exist? file)
  (file-exists? file))

(define (dutignore target)
  (string-append target "/" dutignore-filename))

;; return all listed files to be ignored in .dutignore
(define (ignored-files-found target)
  (string-split (call-with-input-file (dutignore target) get-string-all)
                #\newline))

;; default files to be ignored
(define ignored-files-default '(".git" ".dutignore"))

(define (ignored-files-final options)
  (let ((files (ignored-files-found (get-target options))))
    (map (lambda (i)
           (if (not (member i files))
               (cons i files)))
         files)
    files))

;; Folder residing all dotfiles to link.
(define (get-target options)
  (canonicalize-path (cdr (assv 'from options))))

;; Destination folder, defaults to $HOME.
(define (get-destination options)
  (if (assq 'to options)
      (canonicalize-path (cdr (assv 'to options)))
      home))

;; return: string
;; remove target from current filename "/target/filename" -> "filename"
(define (remove-target filename options)
  (let* ((target (get-target options))
         (target-length (if (string-ci= target filename)
                            (string-length target) ;; if filename is target return without heading /
                            (+ 1 (string-length target)))))
    (string-replace filename "" 0 target-length)))

;; returns TARGET/.config/meh/filename to $HOME/.config/meh/filename
(define (homey filename)
  (string-append home "/" filename))

;; ACTIONS

(define (create options)
  (ftw (get-target options)
       (lambda (filename statinfo flag)
         (let ((filename-wo-target (remove-target filename options)))
           (if (not (member #t (map (lambda (v) (string-prefix? filename-wo-target v))
                                    (ignored-files-found (get-target options)))))
               (begin (display filename-wo-target)
                      (newline)))
           #t))))
(define (create target link)
  (newline)
  (display (format #f "~a -> ~a" target link)))

(define (remove options) (display 'remove))

(define (pretend options) (display 'dryrunning))

(define (overwrite options) (display 'overwriting))

(define (info options)
  (display (string-append "target: " (get-target options)))
  (newline)
  (display (string-append "destination: " (get-destination options)))
  (newline)
  (display (string-append "dutignore: " (string-join (ignored-files-found (get-target options)) " "))))

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
