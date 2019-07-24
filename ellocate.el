;;; ellocate.el --- The locate command reimplemented in Emacs Lisp -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/ellocate
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (s "1.12.0") (ivy "0.11.0"))
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
(require 'ivy)

(defcustom ellocate-scan-dirs '(("~/" "~/ellocate-home-db"))
  "An list of elements in this format: '(path database-location).
If the database field is nil, ellocate will not save the database to
disk, but just store it in ram."
  :type 'list
  :group 'ellocate)

(defvar ellocate-gc-mem 80000000
  "The amount of memory ellocate sets before running a search to avoid
multiple garbage collections. If nil don't modify `gc-cons-threshold'")

(defvar ellocate-scan-cache '()
  "The variable where caches are stored inside Emacs for quick access.
Used to display the ellocate prompt quickly.")

(defvar ellocate-database-coding-system 'utf-8
  "The coding system used by the database file.")

;; This PWD might be a problem https://stackoverflow.com/questions/246215/how-can-i-generate-a-list-of-files-with-their-absolute-path-in-linux
(defvar ellocate-find-command "find \"$PWD\" -name '.git' -prune -o -type f -print"
  "Find command used to build the cache.")

;;;###autoload
(defun ellocate-clear ()
  "Cleans all caches defined in `ellocate-scan-dirs'.
Next time you run ellocate you will need to rebuild the cache.
Run this if your file system has changed and you want ellocate to find your new files."
  (interactive)
  (setq ellocate-scan-cache nil)
  (mapc (lambda (list)
	  (when (nth 1 list)
	    (delete-file (nth 1 list))))
	ellocate-scan-dirs))

(defun ellocate-cache-dir (list)
  "Caches directory in LIST to database also in LIST."
  (let* ((default-directory (expand-file-name (nth 0 list)))
	 (cache (ignore-errors (expand-file-name (nth 1 list))))
	 (candidates (if (and cache (file-exists-p cache))
			 (f-read cache)
		       (shell-command-to-string ellocate-find-command))))

    ;; Write cache to file
    (when (and cache (not (file-exists-p cache)))
      (let ((coding-system-for-write (if coding-system-for-write
					 coding-system-for-write
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
  (let* ((gc-cons-threshold (if ellocate-gc-mem
				ellocate-gc-mem
			      gc-cons-threshold))
	 ;; Load data from cached search corresponding to this default-directory
	 (search
	  (nth 1 (cl-find-if (lambda (list)
			       (file-in-directory-p
				default-directory (nth 0 list)))
			     ellocate-scan-cache))))
    (if search
	(let* ((dir (expand-file-name default-directory))
	       (dir-length (length dir)))
	  (find-file (ivy-read
		      "Find: "
		      search
		      ;; Predicate is slightly faster than using seq-filter over the candidates somehow
		      :predicate (lambda (string) (if ignore-scope
						 t
					       (s-starts-with-p dir string)))
		      ;; Don't sort for better performance, find should already have sorted them anyway
		      :sort nil)))

      (let ((found-dir (cl-find-if
			(lambda (list)
			  (file-in-directory-p (expand-file-name default-directory) (nth 0 list)))
			ellocate-scan-dirs)))
	(if found-dir
	    (ellocate-cache-dir found-dir)
	  (message "Could not search: the current directory is outside of any directories listed in ellocate-scan-dirs")))
      (ellocate))))

(provide 'ellocate)

;;; ellocate.el ends here
