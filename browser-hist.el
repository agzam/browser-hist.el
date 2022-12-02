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
;; Package-Requires: ((emacs "28"))
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
  :type '(chrome brave firefox))

(defcustom browser-hist-ignore-query-params nil
  "When not nil, ignore everything after ? in url."
  :group 'browser-hist
  :type 'boolean)

(defvar browser-hist--db-queries
  '((chrome . "select distinct title, url from urls order by last_visit_time desc")
    (brave . "select distinct title, url from urls order by last_visit_time desc")
    (firefox . "select distinct title, url from moz_places order by last_visit_date desc")))

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
    rows))

(defun browser-hist--completing-fn (coll)
  "Filter COLL when passed to completing-read."
  (lambda (s _ flag)
    (pcase flag
      ('metadata
       `(metadata
         (annotation-function
          ,@(lambda (x)
              (concat
               "\n\t"
               (propertize
                (alist-get x coll nil nil #'string=)
                'face 'completions-annotations))))
         (display-sort-function ; keep rows sorted as they come from db
          ,@(lambda (xs) xs))
         (category . url)))
      ('t
       (all-completions s coll)))))

(defun browser-hist--url-trim (type target)
  "Remove title from TARGET url appended by `browser-hist-search'"
  `(,type .
    ,(replace-regexp-in-string "\t.*" "" target)))

(defun browser-hist-search ()
  "Search through browser history."
  (interactive)
  ;; temporarily adjust browse-url-handlers
  ;; so it properly handles URLs with the appended titles
  (setq prev-browse-url-handlers browse-url-handlers)
  (add-to-list
   'browse-url-handlers
   `(".*\t" . ,(lambda (x &rest _)
                 (browse-url
                  (replace-regexp-in-string "\t.*" "" x))
                 (setq browse-url-handlers prev-browse-url-handlers)
                 (makunbound prev-browse-url-handlers))))
  (when (boundp 'embark-transformer-alist)
    (unless (member '(url . browser-hist--url-trim)
                    embark-transformer-alist)
      (add-to-list
       'embark-transformer-alist
       '(url . browser-hist--url-trim))))

  (let* ((coll (seq-map
                (lambda (x)
                  (cons
                   (concat
                    (car x)
                    "\t"
                    (propertize (cdr x) 'invisible t))
                   (cdr x)))
                (browser-hist--query browser-hist-default-browser)))
         (selected (thread-last
                     (browser-hist--completing-fn coll)
                     (completing-read "Browser history: "))))
    (browse-url selected)))

;;; browser-hist.el ends here
