;;; cuckoo-search.el --- Content-based search hacks for elfeed -*- lexical-binding: t; -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/cuckoo-search
;; Version: 0.1
;; Package-Requires: ((emacs "27.2"))
;; Keywords: comm wp outlines

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
;; search in elfeed. Very early stage. Requires ripgrep.
;;
;;
;;
;;; News
;;

(defvar cuckoo-search-content-id (make-hash-table :test 'equal) "Hashtable with the key content hash and the value id.")
(defvar cuckoo-search-elfeed-data-folder "~/.elfeed/data/")
(defvar cuckoo-search-elfeed-index-file "~/.elfeed/index")
(defvar cuckoo-search-rg-cmd "rg -l -i -e")

(defun cuckoo-search-read-index-file ()
  "Reads the Elfeed index file and return only the real index (:version 4)."
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
  "Tests the read index function. This one works!"
  (let ((index (cuckoo-search-read-index-file)) 
        (entries nil))
    (when index 
      (setq entries (plist-get index :entries)) 
      (if entries  
          (maphash (lambda (key value)
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
 "Content-based search for Elfeed."
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

(advice-add 'elfeed-search-clear-filter :after #'cuckoo-search-elfeed-restore-header)

(defun cuckoo-search-elfeed-restore-header ()
 "Restores the old `header-line-format'."
(with-current-buffer "*elfeed-search*"
  (setq header-line-format (elfeed-search--header))))     

