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
;; Package-Requires: ((emacs "27"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Search through the Browser history
;;
;; Important!
;; for Emacs prior 29, install sqlite.el package
;;
;;; Code:

(require 'subr-x)
(require 'browse-url)

(defgroup browser-hist nil
  "browser-hist group"
  :prefix "browser-hist-"
  :group 'applications)

(defcustom browser-hist-db-paths
  (cond
   ((eq system-type 'darwin)
    '((chrome . "$HOME/Library/Application Support/Google/Chrome/Default/History")
      (brave . "$HOME/Library/Application Support/BraveSoftware/Brave-Browser/Default/History")
      (firefox . "$HOME/Library/Application Support/Firefox/Profiles/*.default-release/places.sqlite")
      (safari . "$HOME/Library/Safari/History.db")
      (chromium . "$HOME/Library/Application Support/Chromium/Default/History")))

   ((memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix))
    '((chrome . "$HOME/.config/google-chrome/Default/History")
      (brave . "$HOME/.config/BraveSoftware/Brave-Browser/Default/History")
      (firefox . "$HOME/.mozilla/firefox/*.default-release-*/places.sqlite")
      (chromium . "$HOME/.config//Chromium/Default/History")))

   ;; FIXME: have to figure out paths in Windows
   ((memq system-type '(cygwin windows-nt ms-dos))
    '((chrome . "C:\\Users\\*\\AppData\\Local\\Google\\Chrome\\User Data\\Default")
      (brave . "")
      (firefox . ""))))
  "Paths to sqlite DBs"
  :group 'browser-hist
  :type '(alist :key-type symbol :value string))

(defcustom browser-hist-default-browser 'chrome
  "Default browser."
  :group 'browser-hist
  :type '(chrome chromium brave firefox safari))

(defcustom browser-hist-ignore-query-params nil
  "When not nil, ignore everything after ? in url."
  :group 'browser-hist
  :type 'boolean)

(defvar browser-hist--db-queries
  '((chrome . "select distinct title, url from urls order by last_visit_time desc")
    (chromium . "select distinct title, url from urls order by last_visit_time desc")
    (brave . "select distinct title, url from urls order by last_visit_time desc")
    (firefox . "select distinct title, url from moz_places order by last_visit_date desc")
    (safari . "select distinct v.title, i.url from history_items i join history_visits v on i.id = v.history_item order by v.visit_time desc")))

(defun browser-hist--make-db-copy (browser)
  "Copy browser's history db file to a temp dir.
Browser history file is usually locked, in order to connect to
db, we copy the file."
  (let* ((db-file (alist-get browser browser-hist-db-paths))
         (hist-db (car (file-expand-wildcards
                        (substitute-in-file-name db-file))))
         (new-fname (format "%sbhist-%s.sqlite"
                            (temporary-file-directory)
                            (symbol-name browser))))
    (copy-file hist-db new-fname :overwite)
    new-fname))

(defun browser-hist--query (browser)
  "Query db."
  (let* ((built-in? (and (fboundp 'sqlite-available-p)
                         (sqlite-available-p)))
         (db-file (browser-hist--make-db-copy browser))
         (query (alist-get browser browser-hist--db-queries))
         (db (if built-in?
                 (sqlite-open db-file)
               (progn
                 (require 'sqlite)
                 (sqlite-init db-file))))
         (rows
          (thread-last
            (if built-in?
                (sqlite-select db query)
             (sqlite-query db query))
            (seq-remove
             (lambda (x) (or (null (car x)) (string-blank-p (car x)))))
            (seq-map
             (lambda (x)
               (when (and (car x) (cadr x))
                (cons (string-trim-right
                       (replace-regexp-in-string
                        (if browser-hist-ignore-query-params "\\?.*" "")
                        "" (cadr x)) "/")
                      (car x))))))))
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

(defun browser-hist-search ()
  "Search through browser history."
  (interactive)
  (unless (member '(".*\t" . browser-hist--url-handler)
                  browse-url-handlers)
    (add-to-list 'browse-url-handlers '(".*\t" . browser-hist--url-handler)))

  (when (boundp 'embark-transformer-alist)
    (unless (member '(url . browser-hist--url-transformer)
                    embark-transformer-alist)
      (add-to-list
       'embark-transformer-alist
       '(url . browser-hist--url-transformer))))

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
