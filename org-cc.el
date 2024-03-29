;;; org-cc.el --- Custom completions for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 orgtre

;; Author: orgtre
;; URL: https://github.com/orgtre/org-cc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package makes it easy to create rich custom completion interfaces
;; to find specific Org headings based on their (meta)data.

;;; Code:

;; * Setup

(require 'org)

(defgroup org-cc nil
  "Custom completions for Org."
  :group 'text)

(defcustom org-cc nil
  "Alist defining your custom completions.

Each key will be used as suffix to define a command `org-cc-key'.
Each value should in turn be an alist with the following keys
and values:

  format (list of one string or alist)
  This is used to format the data-field contents returned by
  get-data-function into completion candidates passed on to
  `completing-read'. The two ways to specify the format are:

  1. As a list of one string, where the string is of the same
     form as the column specification for Org column view. See
     the Info node `(org)Column attributes'. Only the width (INT)
     and property (NAME) attributes are used, and they both have
     to be specified for each data field like this:
     %[INT1]NAME1 %[INT2]NAME2 %[INT3]NAME3 ...
     Each NAME should match one data-field name returned
     by get-data-function below.

  2. An alist between the names of all the data fields and
     their format specification alist.

     Each entry of this format alist should look like this:
     (NAME (first . INT)(sep . STR)(last . INT)(end . STR))
     where NAME is the data-field name as in get-data-function
     below and the values are:

       first (must be specified)
       The number of characters to display from the beginning of
       the data field.

       sep (default: `org-cc-field-part-separator')
       The separator between the first and last parts of a field.

       last (default: 0)
       The number of characters to display from the end of the
       data field.

       end (default: `org-cc-field-separator')
       The separator between two different fields.

  get-data-function (function)
  A function with one &rest argument which returns an alist with
  the data-field names specified by format as keys and their
  contents as values. It should work when called at the beginning
  of an org entry as it will be used with `org-map-entries'.

  get-data-config (alist, optional)
  An alist of configuration options passed to get-data-function.

  match, scope, skip (optional)
  See `org-map-entries', to which they are passed.

  prompt (string, optional)
  The prompt passed on to `completing-read'.

  sort-function (function, optional)
  Function used to sort completion candidates.

  action (function, optional)
  Defines the action to take with the choosen completion candidate.
  Defaults to `org-cc--goto'.

See the example custom completion commands."
  :type 'list)

(defcustom org-cc-default-adjust-for-invisible-chars t
  "Whether to adjust the completion string for invisible chars.
Invisible characters throw off the column alignment
but checking for them is slow."
  :type 'bool)

(defcustom org-cc-default-sort-function nil
  "Default function used to sort completion candidates."
  :type 'function)

(defcustom org-cc-field-separator "  "
  "Separator end between two different fields.
This value is added to the end of a field to separate it from the next.
It is only used if end is not explicitly specified in the `org-cc'
format."
  :type 'string)

(defcustom org-cc-field-part-separator ".."
  "Separator sep between the parts of a field.
If the middle part of a field is omitted, this value is used to
separate the first and last parts. This variable is only used
when sep is not explicitly specified in the `org-cc' format."
  :type 'string)


;; * Example custom completion commands

(add-to-list
 'org-cc
 `(heading
   (format (heading (first . 40)(sep . "..")(last . 10)(end . "  "))
	   (tags (first . 20)(sep . "")(last . 0)(end . "")))
   (get-data-function org-cc-heading-get-data)
   (prompt "Headings: ")
   (sort-function identity)))

(defun org-cc-heading-get-data (&rest config)
  "Function used by `org-cc-heading' to get entry data."
  (let-alist config
    `((heading . ,(org-cc--get-heading t nil t))
      (tags . ,(propertize
		(mapconcat 'identity (org-get-tags) " ")
		'face 'org-tag)))))


;; * Main

;;;###autoload
(defun org-cc-create-commands ()
  "Create the completion commands defined via `org-cc'."
  (interactive)
  (dolist (var org-cc)
    (eval `(org-cc--create-command ,(symbol-name (car var))))))

(defmacro org-cc--create-command (name)
  "Creates a command called org-cc-NAME.
The commands body is made up of `org-cc--read'."
  `(defun ,(intern (format "org-cc-%s" name)) ()
     (interactive)
     (org-cc--read ,name)))

(defun org-cc--read (name)
  "Generic read routine used to built all completion commands.
It uses the data found in the cdr of the entry with car NAME in `org-cc',
and forms the body of a command created by `org-cc-create-commands'."
  (let (choice position)
    (let-alist (alist-get (intern name) org-cc)
      (when (stringp (car .format))
        (setq .format (org-cc--convert-columns-format (car .format))))
      (org-cc--fill-format-spec .format)
      (let-alist (org-cc--get-data .format
				   (car .get-data-function)
				   (car .get-data-config)
				   (car .match)
				   (car .scope)
				   (car .skip))
        (let* ((name-config (alist-get (intern name) org-cc))
	       (sort-fun (or (nth 1 (assq 'sort-function name-config))
			     org-cc-default-sort-function)))
	  (setq choice
                (completing-read
	         (nth 1 (assq 'prompt (alist-get (intern name) org-cc)))
	         (org-cc--create-collection .choice-list sort-fun)))
	  (setq position (gethash choice .hash-table))))
      (funcall (or (car .action) #'org-cc--goto) choice position))))

(defun org-cc--convert-columns-format (fmt)
  "Convert Org column view FMT into `org-cc' format alist.
FMT is a string as described in Info node `(org)Column attributes'."
  (let ((start 0)
        (regex (concat "%\\([0-9]+\\)?\\([[:alnum:]_-]+\\)\\(?:(\\(?:[^)]+\\)"
                       ")\\)?\\(?:{\\(?:[^}]+\\)}\\)?\\s-*"))
        cc-fmt)
    (while (string-match regex fmt start)
      (setq start (match-end 0))
      (let* ((width (and (match-end 1)
                         (string-to-number (match-string 1 fmt))))
	     (prop (match-string-no-properties 2 fmt)))
        (push (cons (intern prop) (list (cons 'first width))) cc-fmt)))
    (nreverse cc-fmt)))

(defun org-cc--fill-format-spec (format)
  "Fill in the missing values in FORMAT.
FORMAT should be as in `org-cc'. It is modified directly."
  (unless (assq 'end (cdr (car (last format))))
    (push (cons 'end "") (cdr (car (last format)))))
  (dolist (field-fmt format)
    (let-alist (cdr field-fmt)
      (unless .first
        (user-error "Format in `org=cc' needs to contain `first'."))
      (if (and .last (> .last 0))
          (unless .sep (push (cons 'sep org-cc-field-part-separator)
                             (cdr field-fmt)))
        (if .sep
            (setcdr (assq 'sep (cdr field-fmt)) "")
          (push (cons 'sep "") (cdr field-fmt))))
      (unless .last (push (cons 'last 0)
                          (cdr field-fmt)))
      (unless .end (push (cons 'end org-cc-field-separator)
                         (cdr field-fmt))))))

(defun org-cc--get-data (format fun &optional config match scope skip)
  "Returns an alist with entries `choice-list' and `hash-table'.
`choice-list' contains the choice alternatives (completion strings).
`hash-table' contains the choice alternatives as keys and the position
of the corresponding headings as values. FORMAT, FUN, CONFIG, MATCH,
SCOPE, and SKIP are as in `org-cc'."
  (let* ((hash-table (make-hash-table :test 'equal))
	 (choice-list (org-map-entries 
		       (lambda ()
			 (let ((choice-string (org-cc--build-completion-string 
					       (funcall fun config)
					       format)))
			   (puthash choice-string (org-cc--get-position)
				    hash-table)
			   choice-string))
		       match scope skip)))
    (list (cons 'choice-list choice-list)
	  (cons 'hash-table hash-table))))

(defun org-cc--get-position ()
  "Return alist holding current buffer position and filename."
  ;; could use (point-marker) instead, but this would require keeping all
  ;; files we collect positions from open, which we might not want in future
  (list
   (cons 
    'startpos
    (point))
   (cons 
    'file
    (buffer-file-name))))

(defun org-cc--build-completion-string (metalist format)
  "Build completing-read string based on alist METALIST.
METALIST is an alist of data returned by the commands
get-data-function; the data-field names are the keys
and the field contents the values.
FORMAT is as specified in `org-cc'."
  (string-trim-right
   (mapconcat
    (lambda (x) (org-cc--build-completion-string-sub x metalist))
    format
    "")))

(defun org-cc--build-completion-string-sub (field metalist)
  "Construct the part of the completion string corresponding to FIELD.
FIELD is an entry of the format alist specified in `org-cc'; it has
the data-field name in car and a format alist in cdr. METALIST is
as in `org-cc--build-completion-string'."
  (let-alist (cdr field)
    (let* ((field-name (car field))
	   (field-prefix (propertize
			  (concat "^" (symbol-name field-name) ":")
			  'invisible t))
	   (field-content (alist-get field-name metalist))
	   (field-suffix (propertize "$" 'invisible t))
	   (field-length (length field-content))
	   (target-length (+ .first (length .sep) .last))
	   comp-string)
      (when (not org-cc-default-adjust-for-invisible-chars)
	(if (<= field-length target-length)
	    (setq comp-string (concat field-prefix field-content field-suffix
				      (make-string 
				       (- target-length field-length) ? )
				      .end))
	  (setq comp-string
		(concat
		 field-prefix
		 (substring field-content 0 .first)
		 (propertize (substring field-content .first field-length)
			     'invisible t)
		 field-suffix
		 .sep
		 (substring field-content (- field-length .last) field-length)
		 .end))))
      (when org-cc-default-adjust-for-invisible-chars
	(let ((total-invisible (org-cc--count-invisible-chars field-content)))
	  (if (<= (- field-length total-invisible) target-length)
	      (setq comp-string (concat field-prefix field-content field-suffix
					(make-string 
					 (- target-length
					    (- field-length total-invisible))
					 ? )
					.end))
	    (setq comp-string
		  (let ((first-n-visible (org-cc--get-first-n-visible-chars
					  field-content .first)))
		    (concat
		     field-prefix
		     first-n-visible
		     (propertize (substring field-content
                                            (length first-n-visible)
					    field-length)
				 'invisible t)
		     field-suffix
		     .sep
		     (org-cc--get-last-n-visible-chars field-content .last)
		     .end))))))
      comp-string)))

(defun org-cc--count-invisible-chars (string)
  "Count the number of invisible characters in STRING."
  (let ((i 0)
	(count 0))
    (while (< i (length string))
      (when (get-text-property i 'invisible string)
	(setq count (+ count 1)))
      (setq i (+ i 1)))
    count))

(defun org-cc--get-first-n-visible-chars (string n)
  "Return the smallest prefix of STRING with N visible chars."
  (let ((itot 0)
	(ivis 0))
    (while (<= ivis n)
      (when (not (get-text-property itot 'invisible string))
	(setq ivis (+ ivis 1)))
      (setq itot (+ itot 1)))
    (substring string 0 (- itot 1))))

(defun org-cc--get-last-n-visible-chars (string n)
  "Return the smallest suffix of STRING with N visible chars."
  ;; reverse discards properties so can't use it
  (let ((itot (length string))
	(ivis 0))
    (while (<= ivis n)
      (when (not (get-text-property (- itot 1) 'invisible string))
	(setq ivis (+ ivis 1)))
      (setq itot (- itot 1)))
    (substring string (+ itot 1) (length string))))

(defun org-cc--create-collection (completions sort-fun)
  "Add SORT-fun as display-sort-function to list COMPLETIONS.
This ensures the completion data is sorted according to SORT-FUN."
  ;; source: https://emacs.stackexchange.com/a/8177
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,sort-fun))
      (complete-with-action action completions string pred))))

(defun org-cc--get-heading (&optional respect-indent no-properties
				      &rest exclusions)
  "Get formatted heading of org entry at point.
With RESPECT-INDENT include the indent as part of the heading.
With NO-PROPERTIES exclude text properties. EXCLUSIONS are
passed as arguments to `org-get-heading'."
  (let ((heading (apply #'org-get-heading exclusions))
	prefix-length)
    (if no-properties
	(setq heading (substring-no-properties heading))
      (when respect-indent
	(setq prefix-length
	      (length (get-text-property 0 'line-prefix heading)))
	(setq heading
	      (concat (make-string prefix-length ? ) heading)))
      ;; fix misalignment caused by org-indent-mode's line-prefix
      (remove-text-properties 0 1 '(line-prefix) heading))
    heading))

(defun org-cc--entry-contents-string (&optional no-properties
                                                no-linebreaks trim)
  "Return the contents of the entry at point as a string."
  (save-excursion
    (let ((s (if no-properties
                 (buffer-substring-no-properties
                  (progn (org-end-of-meta-data t) (point))
                  (if (org-at-heading-p) (point) (org-end-of-subtree t)))
               (buffer-substring
                (progn (org-end-of-meta-data t) (point))
                (if (org-at-heading-p) (point) (org-end-of-subtree t))))))
      (when no-linebreaks
        (setq s (string-replace "\n" " " s)))
      (when trim
        (setq s (string-trim s)))
      s)))

(defun org-cc--goto (_choice position)
  "Goto POSITION and show entry with children.
Default action performed with selected completion candidate."
  (let-alist position
    (find-file .file)
    (org-content 1)
    (goto-char .startpos)
    (when (or (org-invisible-p) (org-invisible-p2))
      (org-fold-show-set-visibility t))
    (org-fold-show-entry)
    (org-fold-show-children)))


(provide 'org-cc.el)
;;; org-cc.el ends here
