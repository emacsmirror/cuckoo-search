;;; cuckoo-search.el --- Content-based search and saved-searches for Elfeed -*- lexical-binding: t; -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/cuckoo-search
;; Version: 0.2.4
;; Package-Requires: ((emacs "29.1") (elfeed "3.4.2"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; cuckoo-search.el is collection of hacks to allow for content-based
;; search in Elfeed. Requires ripgrep and the Elfeed package, the
;; latter must be loaded first - see the above URL for more info.
;;
;;
;;
;;; News
;;
;; 0.2.4
;; - Even more adjustments; added minor-mode `cuckoo-search-mode' and
;; global mode `cuckoo-search-global-mode'
;;
;; 0.2.3
;; - More adjustments; added GPL v3 license
;;
;; 0.2.2
;; - More polishing for intended Melpa release
;;
;; 0.2.1
;; - Now uses `elfeed-db-directory` to get value for data-folder and
;; index file; flycheck package hygiene (thanks @sarg for both
;; suggestions); removed package-lint issues
;;
;; 0.2
;; - Added `cuckoo-saved-searches'
;;
;; 0.1
;; - Initial release

;;; Code:

(require 'json)   ; For json-encode
(require 'elfeed) ; cuckoo-search does not work without Elfeed
(require 'elfeed-db)

(defvar cuckoo-search-content-id (make-hash-table :test 'equal) "Hashtable with the key content hash and the value id.")
(defvar cuckoo-search-elfeed-data-folder (expand-file-name "data" elfeed-db-directory))
(defvar cuckoo-search-elfeed-index-file (expand-file-name "index" elfeed-db-directory))
(defvar cuckoo-search-rg-cmd "rg -l -i -e")
(defvar cuckoo-search-saved-searches-config-file "~/.cuckoo-search-saved-searches")

(defgroup cuckoo-search ()
  "Content-based search and saved-searches for Elfeed."
  :group 'comm)

(define-minor-mode cuckoo-search-mode
  "Minor mode to better integrate `cuckoo-search' into `elfeed-search-mode'."
  :lighter " cuckoo-search-mode"
  (if cuckoo-search-mode
      (advice-add 'elfeed-search-clear-filter :after #'cuckoo-search-elfeed-restore-header)
    (advice-remove 'elfeed-search-clear-filter #'cuckoo-search-elfeed-restore-header)))

;;;###autoload 
(define-globalized-minor-mode cuckoo-search-global-mode
  cuckoo-search-mode
  (lambda ()
    (when (derived-mode-p 'elfeed-search-mode)
      (cuckoo-search-mode 1)))
  :group 'cuckoo-search
  :init-value t)

(defun cuckoo-search-read-index-file ()
  "Read the Elfeed index file and return only the real index (:version 4)."
  (with-temp-buffer
    (insert-file-contents cuckoo-search-elfeed-index-file)
    (goto-char (point-min))
    (let (real-index)
      (while (not (eobp))
        (condition-case nil
            (let ((data (read (current-buffer))))
              (when (and (plistp data) (= (plist-get data :version) 4))
                (setq real-index data)))
          (error nil)))
      real-index)))

(defun cuckoo-search-get-index-meta ()
  "Parse the index and fill the hashtable `cuckoo-search-content-id'."
  (let ((index (cuckoo-search-read-index-file))
        (entries nil))
    (when index
      (setq entries (plist-get index :entries))
      (if entries
          (maphash (lambda (_key value)
		      (let* ((entry-content (elfeed-entry-content value))
			     (entry-string (prin1-to-string entry-content))
			     (entry-content-hash
			      (if
				  (string-match "\"\\([a-f0-9]+\\)\"" entry-string)
				  (match-string 1 entry-string)
				nil)))
			(puthash entry-content-hash value cuckoo-search-content-id)))
		       entries)))))

(defun cuckoo-search (&optional search-string)
 "Content-based search for Elfeed. Accepts optional argument SEARCH-STRING."
 (interactive)
 (cuckoo-search-get-index-meta)
 (let* ((search (if (not search-string)
			 (read-from-minibuffer "Search for: ")
		       search-string))
	(cuckoo-search-findings-content-id (make-hash-table :test 'equal)))
   (with-temp-buffer
     (insert (shell-command-to-string (concat cuckoo-search-rg-cmd " \"" search "\" \"" (expand-file-name cuckoo-search-elfeed-data-folder) "\" --sort accessed")))
      (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (content lines)
	  (puthash (file-name-nondirectory content) (gethash (file-name-nondirectory content) cuckoo-search-content-id) cuckoo-search-findings-content-id))))
   (with-current-buffer "*elfeed-search*"
     (let* ((allowed-entries (hash-table-values cuckoo-search-findings-content-id))
	    (filtered-entries '()))
       (dolist (entry elfeed-search-entries)
	 (when (member entry allowed-entries)
	   (push entry filtered-entries)))
       (setq elfeed-search-entries (nreverse filtered-entries))
         (let ((inhibit-read-only t)
              (standard-output (current-buffer)))
           (erase-buffer)
           (dolist (entry elfeed-search-entries)
             (funcall elfeed-search-print-entry-function entry)
             (insert "\n"))
	   (setq header-line-format
		 (list (elfeed-search--header) " \"" search "\""))
           (setf elfeed-search-last-update (float-time)))))))

(defun cuckoo-search-elfeed-restore-header ()
 "Restore the old `header-line-format'."
(with-current-buffer "*elfeed-search*"
  (setq header-line-format (elfeed-search--header))))

(defun cuckoo-search-add-search ()
  "Add a new search combo to the list of saved-searches."
  (interactive)
  (let* ((cuckoo-search-list-searches (cuckoo-search-get-list-of-searches))
	 (elfeed-search-string (read-from-minibuffer "Enter the Elfeed-search-string to use (e.g. @6-months-ago +unread): "))
	 (cuckoo-search-string (read-from-minibuffer "Enter the cuckoo-search-string to use (e.g. -w China): "))
	 (search-name (read-from-minibuffer "Please provide a name for the new stream: "))
	 (search-name (replace-regexp-in-string "[\"'?:;\\\/]" "_" search-name)))
    (when (not cuckoo-search-list-searches)
      (setq cuckoo-search-list-searches (make-hash-table :test 'equal)))
    (puthash search-name (concat elfeed-search-string "::" cuckoo-search-string) cuckoo-search-list-searches)
    (with-temp-buffer
      (let* ((json-data (json-encode cuckoo-search-list-searches)))
	(insert json-data)
	(write-file cuckoo-search-saved-searches-config-file)))))

(defun cuckoo-search-get-list-of-searches ()
  "Return the hashtable cuckoo-search-name-search."
 (let ((cuckoo-search-file-exists (cuckoo-search-check-for-search-file)))
   (when cuckoo-search-file-exists
     (let ((cuckoo-search-list-searches (make-hash-table :test 'equal)))
       (with-temp-buffer
	 (insert-file-contents cuckoo-search-saved-searches-config-file)
	 (if (fboundp 'json-parse-buffer)
	     (setq cuckoo-search-list-searches (json-parse-buffer))))
cuckoo-search-list-searches))))

(defun cuckoo-search-check-for-search-file ()
  "Check for a search file in `cuckoo-search-saved-searches-config-file'."
  (let ((cuckoo-search-file-exists nil)
	(cuckoo-search-list-searches (make-hash-table :test 'equal))
	(length-of-list))
  (when (file-exists-p cuckoo-search-saved-searches-config-file)
    (with-temp-buffer
	 (insert-file-contents cuckoo-search-saved-searches-config-file)
	 (if (fboundp 'json-parse-buffer)
	     (setq cuckoo-search-list-searches (json-parse-buffer)))
	 (setq length-of-list (length (hash-table-values cuckoo-search-list-searches)))
	 (when (not (zerop length-of-list))
	   (setq cuckoo-search-file-exists t))))
  cuckoo-search-file-exists))

(defun cuckoo-search-saved-searches ()
  "Start a search from the list."
  (interactive)
  (let* ((cuckoo-search-list-searches (cuckoo-search-get-list-of-searches))
	 (searches (hash-table-keys cuckoo-search-list-searches))
	 (selection (completing-read "Select search: " searches))
	 (elfeed-string (cuckoo-search-get-elfeed-string selection))
	 (cuckoo-string (cuckoo-search-get-cuckoo-string selection)))
    (with-current-buffer "*elfeed-search*"
      (when (not (string= elfeed-string ""))
	(setq elfeed-search-filter elfeed-string)
	(elfeed-search-update--force))
	(cuckoo-search-elfeed-restore-header))
      (when (not (string= cuckoo-string ""))
	(cuckoo-search cuckoo-string))))

(defun cuckoo-search-get-elfeed-string (string)
  "Return the elfeed-search-string from STRING."
 (let* ((cuckoo-search-list-searches (cuckoo-search-get-list-of-searches))
       (elfeed-string (gethash string cuckoo-search-list-searches)))
   (string-match "\\(.*?\\)::\\(.*\\)" elfeed-string)
   (setq elfeed-string (match-string 1 elfeed-string))
   elfeed-string))

(defun cuckoo-search-get-cuckoo-string (string)
   "Return the cuckoo-search-string from STRING."
  (let* ((cuckoo-search-list-searches (cuckoo-search-get-list-of-searches))
	 (cuckoo-string (gethash string cuckoo-search-list-searches)))
   (string-match "\\(.*?\\)::\\(.*\\)" cuckoo-string)
   (setq cuckoo-string (match-string 2 cuckoo-string))
   cuckoo-string))
	 
(defun cuckoo-search-remove-search ()
  "Remove a search from the list."
  (interactive)
  (let* ((cuckoo-search-list-searches (cuckoo-search-get-list-of-searches))
	 (searches (hash-table-keys cuckoo-search-list-searches))
	 (json-data)
	 (selection))
    (sort searches 'string<)
    (setq selection
	  (completing-read "Which search should be removed? " searches))
    (if (not (member selection searches))
	(message "This search does not exist.")
      (if (yes-or-no-p (format "Are you sure you want to remove \"%s\" as a saved search? " selection))
	  (progn
	    (remhash selection cuckoo-search-list-searches)
	    (with-temp-buffer
	      (setq json-data (json-encode cuckoo-search-list-searches))
	      (insert json-data)
	      (write-file cuckoo-search-saved-searches-config-file)))))))

(cuckoo-search-global-mode)

;;; cuckoo-search.el ends here
