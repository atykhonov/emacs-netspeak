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

(require 'popup)

(defvar netspeak-base-url "http://api.netspeak.org/netspeak3/search"
  "Netspeak API base URL.")

(defvar netspeak-menu nil
  "Popup menu for suggestions.")

(defvar netspeak-completing-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'ac-expand)
    (define-key map [tab] 'ac-expand)
    (define-key map "\r" 'ac-complete)
    (define-key map [return] 'ac-complete)
    (define-key map (kbd "M-TAB") 'auto-complete)
    ;; (define-key map "\C-s" 'ac-isearch)

    (define-key map "\M-n" 'netspeak-next-candidate)
    (define-key map "\M-p" 'netspeak-previous-candidate)
    (define-key map [down] 'netspeak-next-candidate)
    (define-key map [up] ''netspeak-previous-candidate)

    ;; (define-key map [f1] 'ac-help)
    ;; (define-key map [M-f1] 'ac-persist-help)
    ;; (define-key map (kbd "C-?") 'ac-help)
    ;; (define-key map (kbd "C-M-?") 'ac-persist-help)

    ;; (define-key map [C-down] 'ac-quick-help-scroll-down)
    ;; (define-key map [C-up] 'ac-quick-help-scroll-up)
    ;; (define-key map "\C-\M-n" 'ac-quick-help-scroll-down)
    ;; (define-key map "\C-\M-p" 'ac-quick-help-scroll-up)

    ;; (dotimes (i 9)
    ;;   (let ((symbol (intern (format "ac-complete-%d" (1+ i)))))
    ;;     (fset symbol
    ;;           `(lambda ()
    ;;              (interactive)
    ;;              (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
    ;;                (ac-complete))))
    ;;     (define-key map (read-kbd-macro (format "M-%s" (1+ i))) symbol)))

    map)
  "Keymap for completion.")

(defvar netspeak-candidates-list '()
  "Candidate list for auto completion.")

(defvar ac-source-netspeak nil
  ;; '((candidates . netspeak-candidates)
  ;;   (requires . 0))
  "auto-complete source.")

(defun netspeak-candidates ()
  netspeak-candidates-list)

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
    (let ((current-point (point)))
      (while (re-search-forward "\t" nil t 2)
        (kill-region current-point (point))
        (beginning-of-line 2)
        (setq current-point (point))))
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
  "Queries the query and shows a buffer with suggestions."
  (interactive)
  (let* ((buffer-name "*Netspeak*")
         (query (netspeak--read-from-minibuffer))
         (response (netspeak--request query)))
    (with-output-to-temp-buffer buffer-name
      (set-buffer buffer-name)
      (insert (format "Netspeak's suggestions for \"%s\":\n\n" query))
      (if (string= response "")
          (insert "(no suggestions found)")
        (insert response)))))        

(defun netspeak-popup ()
  (interactive)
  (let* ((region-beg (region-beginning))
         (region-end (region-end))
         (region-text (when (use-region-p)
                        (buffer-substring-no-properties region-beg
                                                        region-end)))
         (symbol-pos nil)
         (menu nil)
         (menu2 nil)
         (menu3 nil)
         (prefix "")
         (text-with-pattern "")
         (candidates '())
         (response nil))
    (goto-char region-beg)
    (setq symbol-pos (search-forward "?" region-end t))
    (when (null symbol-pos)
      (goto-char region-beg)
      (setq symbol-pos (search-forward "*" region-end t)))

    (deactivate-mark)
    (goto-char symbol-pos)

    (setq text-with-pattern
          (with-temp-buffer
            (insert region-text)
            (goto-char (point-min))
            (while (search-forward "." nil t)
              (replace-match ""))
            (goto-char (point-min))
            (while (search-forward "?" nil t)               
              (delete-backward-char 1)
              (insert "\\(.+\\)"))
            (goto-char (point-min))
            (when (search-forward "*" nil t)               
              (delete-backward-char 1)
              (insert "\\(.+\\)"))
            (goto-char (point-min))
            (while (search-forward "..." nil t)
              (insert "\\(.+\\)"))
            (goto-char (point-min))
            (while (search-forward "[" nil t)
              (let ((open-point (- (point) 1)))
                (when (search-forward "]" nil t)
                  (delete-region open-point (point))
                  (insert "\\(.+\\)"))))
            (goto-char (point-min))
            (while (search-forward "{" nil t)
              (let ((open-point (- (point) 1)))
                (when (search-forward "}" nil t)
                  (delete-region open-point (point))
                  (insert "\\(.+\\)"))))
              (buffer-string)))

          (setq netspeak-candidates-list '())
          ;; (setq response (netspeak--request region-text))
          (setq candidates
                (split-string
                 (netspeak--request region-text) "\n"))

          (dolist (candidate candidates)
            (with-temp-buffer
              (erase-buffer)
              (insert candidate)
              (goto-char (point-min))
              (when (re-search-forward text-with-pattern nil t)
                (add-to-list 'netspeak-candidates-list (match-string 1)))))

          (delete-backward-char 1)

          (setq prefix
                (concat (buffer-substring-no-properties (- (point) 5) (- (point) 1))
                        "\\(.*\\)"))

           (setq ac-source-netspeak
                `((candidates . netspeak-candidates-list)
                  ;; (prefix . ,prefix)
                  (requires . 0)))

          (setq ac-sources '(ac-source-netspeak))
          (setq ac-trigger-key "SPC")

          ;; (delete-backward-char 1)
          ;; (execute-kbd-macro " ")
          (ac-trigger-key-command t)
          ;; (execute-kbd-macro " ")
          ))


(defun netspeak--menu-create (point width height)
  (setq netspeak-menu
        (popup-create point width height
                      :around t
                      ;; :face 'ac-candidate-face
                      ;; :mouse-face 'ac-candidate-mouse-face
                      ;; :selection-face 'ac-selection-face
                      :symbol t
                      :scroll-bar t
                      :margin-left 1
                      :keymap netspeak-completing-map ; for mouse bindings
                      )))

(defun netspeak-next-candidate ()
  "Select next candidate."
  (interactive)
  (when (netspeak-menu-live-p)
    (popup-next netspeak-menu)
    (setq netspeak-show-menu t)))
    ;; (if (eq this-command 'netspeak-next-candidate)
    ;;     (setq ac-dwim-enable t))))

(defun netspeak-previous-candidate ()
  "Select previous candidate."
  (interactive)
  (when (netspeak-menu-live-p)
    (popup-previous netspeak-menu)
    (setq netspeak-show-menu t)))
    ;; (if (eq this-command 'netspeak-previous-candidate)
    ;;     (setq ac-dwim-enable t))))

(defun netspeak-update-candidates (cursor scroll-top)
  "Update candidates of menu to `ac-candidates' and redraw it."
  (setf (popup-cursor ac-menu) cursor
        (popup-scroll-top ac-menu) scroll-top)
  (setq ac-dwim-enable (= (length ac-candidates) 1))
  (if ac-candidates
      (progn
        (setq ac-completing t)
        (ac-activate-completing-map))
    (setq ac-completing nil)
    (ac-deactivate-completing-map))
  (ac-inline-update)
  (popup-set-list ac-menu ac-candidates)
  (if (and (not ac-fuzzy-enable)
           (<= (length ac-candidates) 1))
      (popup-hide ac-menu)
    (if ac-show-menu
        (popup-draw ac-menu))))

(defun netspeak-delete-menu ()
  (interactive)
  (popup-delete netspeak-menu))

(provide 'netspeak)

;;; netspeak.el ends here
