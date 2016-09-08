Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum. 

(defun ac-netspeak-candidates ()
                              '("said the city"
                               "stated that the city"
                               "city"
                               "was in the city"
                               "noted that the city"
                               "left the city"
                               "moved to the city"
                               "said that the city"
                               "stated the city"
                               "asked the city"
                               "was a city"))

(defvar ac-source-mysource1
  '((candidates . ac-netspeak-candidates)
    (prefix . "")
    ;; (prefix . "^To: \\(.*\\)")
    (requires . 0)))

(setq ac-sources '(ac-source-mysource1))

To: 
To: sh
To: said that the city
To: was in the city

(setq ac-trigger-key "SPC")

she was in the city

(defun netspeak-popup ()
  (interactive)
  (let* ((region-beg (region-beginning))
         (region-end (region-end))
         (region-text (when (use-region-p)
                        (buffer-substring-no-properties region-beg region-end)))
         (symbol-pos nil)
         (menu nil)
         (menu2 nil)
         (menu3 nil)
         (response nil))
    (goto-char region-beg)
    (setq symbol-pos (search-forward "?" region-end t))
    (deactivate-mark)
    (goto-char symbol-pos)
    ;; (setq menu (popup-menu* (list "Please wait...")))
    ;; (popup-delete menu)
    ;; (setq menu2 (popup-menu* (list "More...")))
    (message "Region text: %s" region-text)
    (setq response (netspeak--request region-text))
    (print (format "Response: %s" response))
    (setq response (split-string (netspeak--request region-text) "\n"))

    (setq netspeak-candidates-list '())
    ;; (setq response (netspeak--request region-text))
    (setq candidates
          (split-string
           (netspeak--request region-text) "\n"))

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
    
    (dolist (candidate candidates)
      (with-temp-buffer
        (erase-buffer)
        (insert candidate)
        (goto-char (point-min))
        (when (re-search-forward text-with-pattern nil t)
          (add-to-list 'netspeak-candidates-list (match-string 1)))))
    
    (delete-backward-char 1)
    (insert (popup-menu* netspeak-candidates-list))))

(defun netspeak-insert ()
  (interactive)
  (message "OK"))

(defvar netspeak-keymap  
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "C-g") 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'netspeak-insert)
    (define-key map (kbd "C-j") 'iregister-insert-text)
    (define-key map (kbd "n") 'iregister-next-text)
    (define-key map (kbd "p") 'iregister-previous-text)
    (define-key map (kbd "l") 'iregister-latest-text)
    (define-key map (kbd "a") 'iregister-append-text)
    (define-key map (kbd "A") 'iregister-prepend-text)
    (define-key map (kbd "d") 'iregister-delete-text-register)
    (define-key map (kbd "s") 'iregister-save-text-to-register)
    map)
  "Keymap for minibuffer when display a text register.")
