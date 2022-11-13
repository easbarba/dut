:;exec emacs -batch -l "$0" -f main "$@"

;;; main.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 EAS Barbosa
;;
;; Author: EAS Barbosa <easbarba@outlook.com>
;; Maintainer: EAS Barbosa <easbarba@outlook.com>
;; Created: November 13, 2022
;; Modified: November 13, 2022
;; Version: 0.0.1
;; Keywords: command-line dotfiles manager linux bsd
;; Homepage: https://github.com/easbarba/dut
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun main ()
  (print (version))
  (print (format "Options in %s" command-line-args-left)))

;; Local Variables:
;; mode: emacs-lisp
;; End:

(provide 'main)
;;; main.el ends here
