;;; browser-hist.el --- Search through the Browser history -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: November 02, 2022
;; Modified: November 02, 2022
;; Version: 0.0.1
;; Keywords: convenience hypermedia matching tools
;; Homepage: https://github.com/agzam/browser-hist.el
;; Package-Requires: ((emacs "27.1"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Search through the Browser history
;;
;;; Code:

(defgroup browser-hist nil
  "browser-hist group"
  :prefix "browser-hist-"
  :group 'applications)

(defcustom browser-hist-db-paths
  '((chrome . "$HOME/Library/Application Support/Google/Chrome/Default/History")
    (brave . "$HOME/Library/Application Support/BraveSoftware/Brave-Browser/Default/History")
    (firefox . "$HOME/Library/Application Support/Firefox/Profiles/rmgcr4hw.default-release/places.sqlite"))
  "Paths to sqlite DBs"
  :group 'browser-hist
  :type '(alist :key-type symbol :value string))

(defcustom browser-hist-default-browser 'brave
  "Default browser."
  :group 'browser-hist
  :type '(choice
          (const 'chrome)
          (const 'chrome)
          (const 'firefox)))

(defcustom browser-hist-ignore-query-params t
  "When not nil, ignore everything after ? in url."
  :group 'browser-hist
  :type 'boolean)

(defvar browser-hist--db-queries
  '((chrome . "select title, url from urls order by last_visit_time desc")
    (brave . "select title, url from urls order by last_visit_time desc")
    (firefox . "select title, url from moz_places order by last_visit_date desc")))

(defun browser-hist--make-db-copy (browser)
  "Copy browser's history db file to a temp dir.
Browser history file is usually locked, in order to connect to
db, we copy the file."
  (let* ((db-file (alist-get browser browser-hist-db-paths))
         (hist-db (substitute-in-file-name db-file))
         (new-fname (format "%sbhist-%s.sqlite"
                            (temporary-file-directory)
                            (symbol-name browser))))
    (copy-file hist-db new-fname :overwite)
    new-fname))

(defun browser-hist--query (browser)
  "Query db."
  (let* ((db (sqlite-open
              (browser-hist--make-db-copy browser)))
         (rows
          (thread-last
            (sqlite-select db (alist-get browser browser-hist--db-queries))
            (seq-remove
             (lambda (x) (or (null (car x)) (string-blank-p (car x)))))
            (seq-map
             (lambda (x)
               (cons (string-trim-right
                      (replace-regexp-in-string
                       (if browser-hist-ignore-query-params "\\?.*" "")
                       "" (cadr x)) "/")
                     (car x)))))))
    (seq-uniq rows (lambda (a b) (string= (car a) (car b))))))

(defun browser-hist--completing-fn (coll)
  "Filter COLL when passed to completing-read."
  (lambda (str _ flag)
    (pcase flag
      ('metadata
       `(metadata
         (annotation-function
          ,@(lambda (x)
              (format "\n\t%s" (alist-get x coll nil nil #'string=))))
         (display-sort-function ; keep rows sorted as they come from db
          ,@(lambda (xs) xs))
         (category . url)))
      ('t
       (if (< (length str) 2)
           (all-completions str (thread-last coll (take 20)))
         (thread-last
           coll
           (seq-filter
            (lambda (x)
              (unless (string-blank-p str)
                (or
                 (string-match-p str (cdr x))
                 (string-match-p str (car x))))))
           (seq-map #'car)))))))

(defun browser-hist-search ()
  (interactive)
  (let* ((rows (browser-hist--query browser-hist-default-browser))
         (selection (completing-read
                     "Browser history: "
                     (browser-hist--completing-fn rows))))
    selection))

(defun consult-browser-history ()
  (let* ((coll (seq-map
                (lambda (x)
                  (cons
                   (concat
                    (car x)
                    (propertize (cdr x) 'invisible t))
                   (cdr x)))
                (browser-hist--query browser-hist-default-browser))))
    (consult--read
     coll
     :prompt "Browser history: "
     :sort nil
     :annotate (lambda (x)
                 (format "\n\t%s" (alist-get x coll nil nil #'string=)))
     :category 'url)
    )
  )



;;; browser-hist.el ends here
