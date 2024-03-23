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

(require 'browse-url)
(eval-when-compile (require 'cl-lib))

(eval-when-compile
  (if (and (fboundp 'sqlite-available-p)
           (sqlite-available-p))
      (progn (defalias 'browser-hist--sqlite-open 'sqlite-open)
             (defalias 'browser-hist--sqlite-select 'sqlite-select))
    (require 'sqlite)
    (defalias 'browser-hist--sqlite-open 'sqlite-init)
    (defalias 'browser-hist--sqlite-select 'sqlite-query)))

(defgroup browser-hist nil
  "browser-hist group"
  :prefix "browser-hist-"
  :group 'applications)

(defcustom browser-hist-minimum-query-length 3
  "Minimum length of the search term(s) to query the history database."
  :type 'natnum
  :group 'browser-hist)

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

(defvar browser-hist--db-fields
  '((chrome   "title"   "url"   "urls"          "ORDER BY last_visit_time desc")
    (chromium "title"   "url"   "urls"          "ORDER BY last_visit_time desc")
    (brave    "title"   "url"   "urls"          "ORDER BY last_visit_time desc")
    (firefox  "title"   "url"   "moz_places"    "ORDER BY last_visit_date desc")
    (safari   "v.title" "i.url" "history_items"
     "i JOIN history_visits v ON i.id = v.history_item ORDER BY v.visit_time desc")))

(defsubst browser-hist--db-copy-name (browser)
  (format "%sbhist-%s.sqlite"
          (temporary-file-directory)
          (symbol-name browser)))

(defun browser-hist--make-db-copy (browser &optional force-update)
  "Copy browser's history db file to a temp dir.
Browser history file is usually locked, in order to connect to
db, we copy the file."
  (let* ((db-file (alist-get browser browser-hist-db-paths))
         (hist-db (car (file-expand-wildcards
                        (substitute-in-file-name db-file))))
         (new-fname (browser-hist--db-copy-name browser)))
    (if (or force-update
            (not (file-exists-p new-fname))
            (file-newer-than-file-p hist-db new-fname))
        (copy-file hist-db new-fname :overwite :keep-time)
      new-fname)))

(defvar browser-hist--db-connection nil)

(defun browser-hist--send-query (strings)
  "Find database entries matching STRINGS.

If STRINGS is nil return the latest 100 entries."
  (let ((full-query
         (cl-loop with (title url table rest) =
                  (alist-get browser-hist-default-browser browser-hist--db-fields)
                  with emptyp = (or (not strings) (string-empty-p strings))
                  ;; collect WHERE queries if strings is non-empty
                  for s in (and (not emptyp) (split-string strings))
                  collect (format " ( %s LIKE '%%%s%%' OR %s LIKE '%%%s%%' ) "
                                  title s url s)
                  into queries
                  finally return        ;Construct full query
                  (concat
                   (format "SELECT DISTINCT %s, %s FROM %s " title url table)
                   (and (not emptyp) " WHERE ") ;Match strings
                   (mapconcat #'identity queries " AND ") rest
                   (and emptyp " LIMIT 100")))) ;No match, just return history
        (db (or browser-hist--db-connection
                (setq browser-hist--db-connection
                      (browser-hist--sqlite-open
                       (browser-hist--db-copy-name
                        browser-hist-default-browser))))))
    (cl-loop for (desc link) in (browser-hist--sqlite-select db full-query)
             unless (or (null desc) (string-blank-p desc))
             collect (cons (string-trim-right
                            (replace-regexp-in-string
                             (if browser-hist-ignore-query-params "\\?.*" "")
                             "" link))
                           desc))))

(defun browser-hist--completion-table (s _ flag)
  (let* ((rows-raw (if (>= (length (string-trim s))
                           browser-hist-minimum-query-length)
                       (browser-hist--send-query s)
                     (browser-hist--send-query nil))))
    (pcase flag
      ('metadata
       `(metadata
         (annotation-function
          ,@(lambda (x)
              (concat " "
               (if (> (length x) (floor (window-width) 2))
                   "\n\t" (propertize " " 'display '(space :align-to (- center 1))))
               (alist-get x rows-raw nil nil #'string=))))
         (display-sort-function ,@(lambda (xs) xs))
         (category . url)))
      ('nil (try-completion s rows-raw))
      ('t (mapcar #'car rows-raw)))))

(defun browser-hist--url-transformer (type target)
  "Remove title from TARGET url appended by `browser-hist-search'"
  `(,type . ,(replace-regexp-in-string "\t.*" "" target)))

(defun browser-hist--url-handler (url &rest _)
  "Remove title from TARGET url appended by `browser-hist-search'"
  (browse-url (replace-regexp-in-string "\t.*" "" url)))

;;;###autoload
(defun browser-hist-search (&optional force-update)
  "Search through browser history.

With prefix-arg FORCE-UPDATE, ensure that the history cache is
updated."
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

  (when force-update (message "Forcing browser history update"))
  (browser-hist--make-db-copy browser-hist-default-browser force-update)
  
  (unwind-protect
      (let*
          ((completion-styles '(basic partial-completion))
           (selected
            (completing-read "Browser history: "
                             #'browser-hist--completion-table)))
        (browse-url selected))
    (and browser-hist--db-connection
         (ignore-errors (sqlite-close browser-hist--db-connection))
         (setq browser-hist--db-connection nil))))

(provide 'browser-hist)
;;; browser-hist.el ends here
