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

(require 'browse-url)

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

(defcustom browser-hist-default-browser 'chrome
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


(defvar browser-hist-use-cache nil
  "Cache results until next session or universal argument is passed.")

(defvar browser-hist--db-cached nil
  "Holds the current browser history cache.")

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

(defun browser-hist--url-transformer (type target)
  "Remove title from TARGET url appended by `browser-hist-search'"
  `(,type .
    ,(replace-regexp-in-string "\t.*" "" target)))

(defun browser-hist--url-handler (url &rest _)
  "Remove title from TARGET url appended by `browser-hist-search'"
  (browse-url (replace-regexp-in-string "\t.*" "" url)))

(defun browser-hist-search (&optional clear-cache)
  "Search through browser history.

When `browser-hist-use-cache' is not nil, CLEAR-CACHE can be
passed using universal argument to refresh the cache."
  (interactive "P")
  (unless (member '(".*\t" . browser-hist--url-handler)
                  browse-url-handlers)
    (add-to-list 'browse-url-handlers '(".*\t" . browser-hist--url-handler)))

  (when (boundp 'embark-transformer-alist)
    (unless (member '(url . browser-hist--url-transformer)
                    embark-transformer-alist)
      (add-to-list
       'embark-transformer-alist
       '(url . browser-hist--url-transformer))))

  (when (or clear-cache (not browser-hist--db-cached) (not browser-hist-use-cache))
      (setq browser-hist--db-cached (browser-hist--query browser-hist-default-browser)))

  (let* ((coll (seq-map
                (lambda (x)
                  (cons
                   (concat
                    (car x)
                    "\t"
                    (propertize (cdr x) 'invisible t))
                   (cdr x)))
                browser-hist--db-cached))
         (selected (completing-read "Browser history: " (browser-hist--completing-fn coll))))
    (browse-url selected)))

;;; browser-hist.el ends here
