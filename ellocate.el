;;; ellocate.el --- The locate command reimplemented in Emacs Lisp -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/ellocate
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (s "1.12.0") (f "0.20.0"))
;; Keywords: matching

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a re-implementation of locate written in Emacs Lisp.
;; It recursively searches directories using the find command and then stores the
;; results in the database file specified in `ellocate-scan-dir'.

;; The function `ellocate' scans the directory as specified by `ellocate-scan-dirs'
;; and creates a database file to store the results in.
;; `ellocate-clear' clears all stored databases.
;; This forces `ellocate' to refresh its search entries. Do this if your
;; file-system has changed and you want that reflected in `ellocate'.

;;; Code:
(require 's)
(require 'f)
(require 'cl-lib)

(defgroup ellocate nil
  "The locate command reimplemented in Emacs Lisp."
  :prefix "ellocate-"
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/walseb/ellocate"))

(defcustom ellocate-scan-dirs '(("~/" "~/ellocate-home-db")
				("/mnt/" nil))
  "A list of lists in this format:
'((path database-location) (path2 database-location2)).
If the database field is nil, ellocate will not save the database to
disk, but just store it in ram."
  :type 'sexp
  :group 'ellocate)

(defvar ellocate-gc-mem 80000000
  "GC limit to set before running ellocate.
Set before running ellocate to avoid multiple garbage collections. If nil
don't modify `gc-cons-threshold'")

(defvar ellocate-scan-cache '()
  "The variable where caches are stored inside Emacs for quick access.
Used to display the ellocate prompt quickly.")

(defvar ellocate-database-coding-system 'utf-8
  "The coding system used by the database file.")

;; This PWD might be a problem https://stackoverflow.com/questions/246215/how-can-i-generate-a-list-of-files-with-their-absolute-path-in-linux
(defvar ellocate-find-command "find \"$PWD\" -name '.git' -prune -o -type f -print"
  "Find command used to build the cache.")

;;;###autoload
(defun ellocate-clear-all ()
  "Cleans all caches defined in `ellocate-scan-dirs'.
Next time you run `ellocate' you will need to rebuild the cache.
Run this if your file system has changed and you want ellocate to find your new files."
  (interactive)
  (setq ellocate-scan-cache nil)
  (mapc (lambda (list)
	  (when (nth 1 list)
	    (delete-file (nth 1 list))))
	ellocate-scan-dirs))

;;;###autoload
(defun ellocate-clear (&optional ignore-scope)
  "Clears the cache corresponding to your current directory.
If IGNORE-SCOPE is non-nil, clear all caches instead of just the cache
corresponding to your current directory.

Next time you run `ellocate' in this directory it will need to
rebuild the cache.
Run this function if your file system has changed and you want `ellocate' to find your new files."
  (interactive "P")
  (if ignore-scope
      (ellocate-clear-all)
    ;; Clear cache
    (setq ellocate-scan-cache
	  (seq-remove (lambda (list)
			(let* ((path-name (nth 0 list))
			       (db-name (nth 1 list)))
			  (if (and db-name (file-in-directory-p default-directory path-name))
			      t
			    nil)))
		      ellocate-scan-cache))

    ;; Clear databases on disk
    (mapc (lambda (list)
	    (let* ((path-name (nth 0 list))
		   (db-name (nth 1 list)))
	      (when (and db-name (file-in-directory-p default-directory path-name))
		(message (concat "Deleting database: " db-name))
		(delete-file db-name))))
	  ellocate-scan-dirs)))

(defun ellocate-cache-dir (list)
  "Caches directory in LIST to database also in LIST."
  (let* ((default-directory (expand-file-name (nth 0 list)))
	 (cache (ignore-errors (expand-file-name (nth 1 list))))
	 (candidates (if (and cache (file-exists-p cache))
			 (f-read cache)
		       (shell-command-to-string ellocate-find-command))))

    ;; Write cache to file
    (when (and cache (not (file-exists-p cache)))
      (let ((coding-system-for-write (or coding-system-for-write
                                         ellocate-database-coding-system)))
	(write-region candidates nil cache)))

    ;; Store the cache in Emacs as a variable
    (add-to-list 'ellocate-scan-cache `(,default-directory
					 ,(split-string
					   candidates
					   "\n")))))

;;;###autoload
(defun ellocate (&optional ignore-scope)
  "Displays any files below the current dir.
If IGNORE-SCOPE is non-nil, search the entire database instead of just every
file under the current directory."
  (interactive "P")
  (let* ((gc-cons-threshold (or ellocate-gc-mem gc-cons-threshold))
	 ;; Load data from cached search corresponding to this default-directory
	 (search
	  (nth 1 (cl-find-if (lambda (list)
			       (file-in-directory-p
				default-directory (nth 0 list)))
			     ellocate-scan-cache))))
    (if search
	(let ((dir (expand-file-name default-directory)))
	  (find-file
	   (ellocate-completing-read dir search ignore-scope)))

      (let ((found-dir (cl-find-if
			(lambda (list)
			  (file-in-directory-p (expand-file-name default-directory) (nth 0 list)))
			ellocate-scan-dirs)))
	(if found-dir
	    (ellocate-cache-dir found-dir)
	  (message "Could not search: the current directory is outside of any directories listed in ellocate-scan-dirs")))
      ;; Re-run ellocate on the newly found files
      (ellocate))))

;;;###autoload
(defun ellocate-all ()
  "Find all files in current database.
This can also be done by running ellocate with a universal argument
but I figured it would be good to make it more explicit."
  (interactive)
  (ellocate t))

(defun ellocate-completing-read (dir candidates ignore-scope)
  "Run completing read on CANDIDATES without sorting.
Only candidates inside DIR are shown if IGNORE-SCOPE is nil."
  (let* ((presorted-completions
	  (if ignore-scope
	      candidates
	    (seq-filter (lambda (string) (s-starts-with-p dir string)) candidates)))
	 (completion-table
	  ;; Don't give up here! What this lambda does is described here:
	  ;; https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting?rq=1
	  (lambda (string pred action)
	    (if (eq action 'metadata)
		'(metadata (display-sort-function . identity)
			   (cycle-sort-function . identity))
	      (complete-with-action
	       action presorted-completions string pred)))))
    (completing-read "Find: " completion-table)))

(provide 'ellocate)

;;; ellocate.el ends here
