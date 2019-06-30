;;; ellocate.el --- Find a file using emacs with the speed of locate -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/ellocate
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (s "1.12.0") (ivy "0.11.0"))
;; Keywords: extensions

;;; Commentary:

;; Todo

;;; Code:
(require 's)

(defcustom ellocate-scan-dirs '(("~/find-home" "~/"))
  "An list of elements (path database-location)."
  :type 'list
  :group 'exwm-edit)

(defvar ellocate-gc-mem 80000000
  "The amount of memory ellocate sets before running a search to avoid
multiple garbage collections. If nil don't modify `gc-cons-threshold'")

(defvar ellocate-scan-cache '()
  "The variable where the cache is stored.")

(defvar ellocate-databas-coding-system 'utf-8
  "The coding system used by the database file.")

;; This PWD might be a problem https://stackoverflow.com/questions/246215/how-can-i-generate-a-list-of-files-with-their-absolute-path-in-linux
(defvar ellocate-find-command "find \"$PWD\" -name '.git' -prune -o -type f -print"
  "Find command used to build the cache.")

(defun ellocate-reset ()
  "Cleans all caches so that next time you run ellocate you will need to rebuild the cache."
  (interactive)
  (setq ellocate-scan-cache nil)
  (mapc (lambda (list) (delete-file (nth 1 list))) ellocate-scan-dirs))

;; TODO: Add option to compress the files saved with tar, this reduces the file size by like 99% and doesn't take any time (and only has to be done while loading the file into the elisp list)
(defun ellocate-cache-dir (list)
  "Caches directory in LIST to database also in LIST."
  (let* ((default-directory (expand-file-name (nth 0 list)))
	 (cache (expand-file-name (nth 1 list)))
	 (results (if (file-exists-p cache)
		      (f-read cache)
		    (shell-command-to-string ellocate-find-command))))

    (when (not (file-exists-p cache))
      (let ((coding-system-for-write (if coding-system-for-write
					 coding-system-for-write
				       ellocate-databas-coding-system)))
	(write-region results nil cache)))

    ;; Cache
    (add-to-list 'ellocate-scan-cache `(,default-directory
					 ,(split-string
					   results
					   "\n")))))

;; TODO: Error if user is outside scope defined by ellocate-scan-dirs
;; TODO: Add command to search entire scan dir, i.e. ignore predicate here
(defun ellocate ()
  "Displays any files below the current dir."
  (interactive)
  (let* ((gc-cons-threshold
	  (if ellocate-gc-mem
	      ellocate-gc-mem
	    gc-cons-threshold))
	 ;; Load data from cached search corresponding to this default-directory
	 (search
	  (nth 1
	       (cl-find-if (lambda (list)
			     (file-in-directory-p
			      default-directory (nth 0 list)))
			   ellocate-scan-cache))))

    (if search
	(let* ((dir (expand-file-name default-directory))
	       (dir-length (length dir)))
	  (find-file (ivy-read
		      "Find: "
		      search

		      ;; Predicate is slightly faster than using seq-filter somehow
		      :predicate (lambda (string) (s-starts-with-p dir string))
		      ;; We can't have initial input because if the path contains capital letters, we have to use capital letters in the search. Also if we just lower case the search we will get a bad path if we choose to use capital letters in the search
		      ;;:initial-input (concat "^" (expand-file-name default-directory))
		      ;; Don't sort for better performance, find should already have sorted them anyway
		      :sort nil)))

      (ellocate-cache-dir
       (cl-find-if (lambda (list)
		     (file-in-directory-p (expand-file-name default-directory) (nth 0 list))) ellocate-scan-dirs))
      (ellocate))))

(provide 'ellocate)

;;; ellocate.el ends here
