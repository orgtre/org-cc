;;; org-cc.el --- Custom completion for Org -*- lexical-binding: t; -*-

;;; Commentary:

;; This package make it easy to create rich custom completion interfaces
;; to find specific org-mode headings based on their metadata.

;;; Code:

(defgroup org-cc nil
  "Custom completion for Org."
  :group 'text)

(defcustom org-cc-get-metadata-function 'org-cc-get-metadata
  "Function used to build metadata associated with an org entry.
It should return an alist with the same keys as
`org-cc-metadata-format'."
  :type 'symbol)

(defcustom org-cc-metadata-format
  '((heading (first . 40)(sep . "...")(last . 10)(end . "   "))
    (tags (first . 20)(sep . "")(last . 0)(end . "")))
  "List of metadata available for an entry."
  :type 'list)

(defcustom org-cc-adjust-for-invisible-chars t
  "Whether to adjust the completion string for invisible chars.
Invisible characters throw off the column alignment
but checking for them is slow."
  :type 'bool)

(defcustom org-cc-respect-org-indent nil
  "Whether to respect indent set by `org-indent-mode'."
  :type 'bool)

(defcustom org-cc-sort-function #'identity
  "Function used to sort completition candidates."
  :type 'function)


;;;###autoload
(defun org-cc-goto ()
  (interactive)
  (let-alist (org-cc--get-data)
    (let* ((choice
            (completing-read
	     "Headings: "
	     (org-cc--create-collection-function .choice-list)))
	   (position (gethash choice .hash-table)))
      (print position)
      (let-alist position	
	(find-file .file)
	(org-content 1)
	(goto-char .startpos)
	(when (or (org-invisible-p) (org-invisible-p2))
	  (org-show-set-visibility t))
	(org-show-entry)
	(org-show-children)
	))))

(defun org-cc--get-data ()
  (let* ((hash-table (make-hash-table :test 'equal))
	 (choice-list (org-map-entries 
		       (lambda ()
			 (let ((choice-string (org-cc--build-completion-string 
					       (org-cc-get-metadata))))
			   (puthash choice-string (org-cc--get-position)
				    hash-table)
			   choice-string
			   )))))
    (list (cons 'choice-list choice-list)
	  (cons 'hash-table hash-table))))

(defun org-cc--get-position ()
  (list
   (cons 
    'startpos
    (point))
   (cons 
    'file
    (buffer-file-name))))

(defun org-cc-get-metadata ()
  "Build an alist of metadata for entry."
  (let ((heading
	 ;;(substring-no-properties (org-get-heading))
	 (org-get-heading t))
	(tags
	 (mapconcat (lambda (x) x) (org-get-tags) " ")))
    ;; fix misalignment caused by org-indent-mode's line-prefix
    (when org-cc-respect-org-indent
      (let ((prefix-length
	     (length (get-text-property 0 'line-prefix heading))))
	(setq heading
	      (concat (make-string prefix-length ? ) heading))))
    (remove-text-properties 0 1 '(line-prefix) heading)
    `((heading . ,heading)(tags . ,tags))))

(defun org-cc--build-completion-string (metalist)
  "Build completing-read string based on alist METALIST."
  (mapconcat
   (lambda (x) (org-cc--build-completion-string-sub x metalist))
   org-cc-metadata-format
   ""))

(defun org-cc--build-completion-string-sub (field metalist)
  
  (let-alist (cdr field)
    (let* ((field-name (car field))
	   (field-content (alist-get field-name metalist))
	   (field-length (length field-content))
	   (target-length (+ .first (length .sep) .last))
	   comp-string)
      (when (not org-cc-adjust-for-invisible-chars)
	(if (<= field-length target-length)
	    (setq comp-string (concat field-content
				      (make-string 
				       (- target-length field-length) ? )
				      .end))
	  (setq comp-string
		(concat 
		 (substring field-content 0 .first)
		 .sep
		 (substring field-content (- field-length .last) field-length)
		 .end))))
      (when org-cc-adjust-for-invisible-chars
	(let ((total-invisible (org-cc--count-invisible-chars field-content)))
	  (if (<= (- field-length total-invisible) target-length)
	      (setq comp-string (concat field-content
					(make-string 
					 (- target-length field-length) ? )
					.end))
	    (setq comp-string
		  (concat
		   (org-cc--get-first-n-visible-chars field-content .first)
		   .sep
		   (org-cc--get-last-n-visible-chars field-content .last)
		   .end)))))
      (set-text-properties 0 (length field-content)
			   '(invisible t) field-content)
      (concat comp-string field-content))))

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
  (let ((itot 0)
	(ivis 0))
    (while (<= ivis n)
      (when (not (get-text-property itot 'invisible string))
	(setq ivis (+ ivis 1)))
      (setq itot (+ itot 1)))
    (substring string 0 (- itot 1))))

(defun org-cc--get-last-n-visible-chars (string n)
  ;; reverse discards properties so can't use it
  (let ((itot (length string))
	(ivis 0))
    (while (<= ivis n)
      (when (not (get-text-property (- itot 1) 'invisible string))
	(setq ivis (+ ivis 1)))
      (setq itot (- itot 1)))
    (substring string (+ itot 1) (length string))))

(defun org-cc--create-collection-function (completions)
  ;; source: https://emacs.stackexchange.com/a/8177
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,org-cc-sort-function))
      (complete-with-action action completions string pred))))


(provide 'org-cc.el)
;;; org-cc.el ends here

