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

(define (info root)
  (display (string-append "root: " root))
  (newline)
  (display (string-append
            "dutignore: " (dutignore root)
            " - found? " (if (dutignore-exist? root) "yep" "nope"))))

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
