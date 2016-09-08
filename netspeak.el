;;; netspeak.el --- Emacs interface to Netspeak

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/emacs-netspeak
;; Version: 0.1.0
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 
;; 
;; 

;;; Code:

(defvar netspeak-base-url "http://api.netspeak.org/netspeak3/search"
  "Netspeak API base URL.")

(defun netspeak--trim-string (string)
  "Remove whitespaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ .\t\n\r]*" ""
                            (replace-regexp-in-string "[ .\t\n\r]*\\'" "" string)))

(defun netspeak--http-response-body (url)
  "Retrieve URL and return the response body as a string."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward (format "\n\n"))
    (delete-region (point-min) (point))
    (prog1 
        (buffer-string)
      (kill-buffer))))

(defun netspeak--format-query-string (query-params)
  "Format QUERY-PARAMS as a query string.

QUERY-PARAMS must be an alist of field-value pairs."
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             query-params "&"))

(defun netspeak--format-request-url (query-params)
  "Format QUERY-PARAMS as a Google Translate HTTP request URL.

QUERY-PARAMS must be an alist of field-value pairs."
  (concat netspeak-base-url
          "?"
          (netspeak--format-query-string query-params)))

(defun netspeak--parse-text-response (response)
  "Parse response assuming that the format of it is a plain
text. Return parsed suggestions."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (let ((current-point (point))
          (rate nil)
          (rates (list))
          (total 0)
          (longest-line 0)
          (first-persent-pos nil))
      (while (re-search-forward "\t" nil t 1)
        (kill-region current-point (point))
        (setq current-point (point))
        (re-search-forward "\t" nil t)
        (setq rate
              (string-to-int
               (netspeak--trim-string
                (buffer-substring-no-properties current-point (point)))))
        (setq rates (add-to-list 'rates rate t))
        (kill-region current-point (point))
        (end-of-line)
        (when (> (- (line-end-position) (line-beginning-position)) longest-line)
          (setq longest-line (- (line-end-position) (line-beginning-position))))
        (message "Longest line: %s" longest-line)
        (beginning-of-line 2)
        (setq current-point (point)))
      (beginning-of-buffer)
      (setq total (apply '+ rates))
      (while rates
        (setq rate (pop rates))
        (end-of-line)
        (setq spaces (- longest-line (- (line-end-position) (line-beginning-position))))
        (insert (format "  %s%s" (make-string spaces ? ) (netspeak--commify rate)))
        (insert "  ")
        (when (null first-persent-pos)
          (setq first-persent-pos (- (line-end-position) (line-beginning-position))))
        (while (< (- (line-end-position) (line-beginning-position)) first-persent-pos)
          (insert " "))
        (insert (format "%s%%" (/ (* rate 100) total)))
        (end-of-line 2)))
    (buffer-string)))    

(defun netspeak--request (query)
  "Make request to Netspeak with QUERY.Return suggestions as a
plain text."
  (netspeak--parse-text-response
   (netspeak--http-response-body
    (netspeak--format-request-url
     `(("query" . ,(netspeak--prepare-query query)))))))

(defun netspeak--prepare-query (query)
  (netspeak--trim-string
   ;; query must be lower cased due to issue in API
   (downcase query)))

(defun netspeak--read-from-minibuffer ()
  "Read string from minibuffer. If region is active then use it
as default value. Otherwise takes as default value sentence at
point."
  (let* ((prompt "Netspeak query: ")
         (query (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (sentence-at-point)))
         (query (netspeak--prepare-query query)))
    (read-from-minibuffer prompt query
                          nil nil nil query t)))

(defun netspeak-query ()
  "Make the query and show a buffer with the suggestions."
  (interactive)
  (let* ((query (netspeak--read-from-minibuffer))
         (response (netspeak--request query)))
    (netspeak--show-response response)))

(defun netspeak-query-line ()
  "Take the current line, make query and show the buffer with
suggestions."
  (interactive)
  (let* ((line-bos (line-beginning-position))
         (line-eos (line-end-position))
         (query (buffer-substring-no-properties line-bos line-eos))
         (response (netspeak--request query)))
    (netspeak--show-response response)))

(defun netspeak--show-response (response)
  "Output response in the buffer."
  (let ((buffer-name "*Netspeak*"))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (insert (format "Netspeak's suggestions for \"%s\":\n\n" query))
      (if (string= response "")
          (insert "(no suggestions found)")
        (insert response)))))

(defun netspeak--commify (n &optional comma-char)
  (unless comma-char (setq comma-char ","))
  (with-temp-buffer
    (insert (format "%s" n))
    (while (> (- (point)
                 (line-beginning-position))
              (if (>= n 0) 3 4))
      (backward-char 3)
      (insert comma-char)
      (backward-char 1))
    (buffer-string)))


(provide 'netspeak)

;;; netspeak.el ends here
