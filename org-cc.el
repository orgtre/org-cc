;;; org-cc.el --- Custom completions for Org -*- lexical-binding: t; -*-

;;; Commentary:

;; This package makes it easy to create rich custom completion interfaces
;; to find specific Org headings based on their (meta)data.

;;; Code:

;; * Setup

(defgroup org-cc nil
  "Custom completions for Org."
  :group 'text)

(defcustom org-cc
  `((heading
     (prompt "Headings: ")
     (data-format (heading (first . 40)(sep . "...")(last . 10)(end . "   "))
		  (tags (first . 20)(sep . "")(last . 0)(end . "")))
     (get-data-function org-cc-heading-get-data)
     (get-data-config (respect-org-indent t)))
    (heading2
     (prompt "Headings: ")
     (data-format (heading (first . 20)(sep . "...")(last . 10)(end . "   "))
		  (tags (first . 20)(sep . "")(last . 0)(end . "")))
     (get-data-function org-cc-heading-get-data)
     (get-data-config )))
  "Alist defining your custom completions.

Each key will be used as suffix to define a command 'org-cc-key'.
Each value should in turn be an alist with the following keys:

  prompt
  The prompt used by `completing-read'.

  data-format
  An alist specifying the names of all data fields as keys
  and an alist of their format specification as values.

  get-data-function
  A function with one &rest argument which returns an alist with
  the data-field names specified by data-format as keys and their
  contents as values.

  get-data-config
  An alist of configuration options passed to get-data-function."
  :type 'list)

(defun org-cc-heading-get-data (&rest config)
  "Build an alist of (meta)data for entry."
  (let-alist config 
    (let ((heading
	   ;;(substring-no-properties (org-get-heading))
	   (org-get-heading t))
	  (tags
	   (mapconcat (lambda (x) x) (org-get-tags) " ")))
      ;; fix misalignment caused by org-indent-mode's line-prefix
      (when .respect-org-indent
	(let ((prefix-length
	       (length (get-text-property 0 'line-prefix heading))))
	  (setq heading
		(concat (make-string prefix-length ? ) heading))))
      (remove-text-properties 0 1 '(line-prefix) heading)
      `((heading . ,heading)(tags . ,tags)))))

(defvar org-cc-heading-get-data-config
  `((respect-org-indent nil))
  "Alist of options passed on to `org-cc-heading-get-data'.

A short description of each key:

  respect-org-indent (bool)
  Whether to respect indent set by `org-indent-mode'.")

(defcustom org-cc-default-adjust-for-invisible-chars t
  "Whether to adjust the completion string for invisible chars.
Invisible characters throw off the column alignment
but checking for them is slow."
  :type 'bool)

(defcustom org-cc-default-sort-function #'identity
  "Function used to sort completition candidates."
  :type 'function)


;; * Main

;;;###autoload
(defun org-cc-create-commands ()
  "Create the completion commands defined via `org-cc'."
  (dolist (var org-cc)
    (eval `(org-cc--create-command ,(symbol-name (car var))))))

(defmacro org-cc--create-command (name)
  `(defun ,(intern (format "org-cc-%s" name)) ()
     (interactive)
     (org-cc--goto ,name)))

(defun org-cc--goto (name)
  (let-alist (alist-get (intern name) org-cc)
    (let-alist (org-cc--get-data (car .get-data-function) .get-data-config .data-format)
      (let* ((choice
              (completing-read
	       (car (alist-get 'prompt (alist-get (intern name) org-cc)))
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
	  )))))

(defun org-cc--get-data (fun config data-format)
  (let* ((hash-table (make-hash-table :test 'equal))
	 (choice-list (org-map-entries 
		       (lambda ()
			 (let ((choice-string (org-cc--build-completion-string 
					       (funcall fun config)
					       data-format)))
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

(defun org-cc--build-completion-string (metalist data-format)
  "Build completing-read string based on alist METALIST."
  (mapconcat
   (lambda (x) (org-cc--build-completion-string-sub x metalist))
   data-format
   ""))

(defun org-cc--build-completion-string-sub (field metalist)
  (let-alist (cdr field)
    (let* ((field-name (car field))
	   (field-content (alist-get field-name metalist))
	   (field-length (length field-content))
	   (target-length (+ .first (length .sep) .last))
	   comp-string)
      (when (not org-cc-default-adjust-for-invisible-chars)
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
      (when org-cc-default-adjust-for-invisible-chars
	(let ((total-invisible (org-cc--count-invisible-chars field-content)))
	  (if (<= (- field-length total-invisible) target-length)
	      (setq comp-string (concat field-content
					(make-string 
					 (- target-length
					    (- field-length total-invisible))
					 ? )
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
        `(metadata (display-sort-function . ,org-cc-default-sort-function))
      (complete-with-action action completions string pred))))


(provide 'org-cc.el)
;;; org-cc.el ends here
