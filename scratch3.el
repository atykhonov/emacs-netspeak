Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum. 

(defun ac-netspeak-candidates ()
                              '("she said the city"
                               "she stated that the city"
                               "she city"
                               "she was in the city"
                               "she noted that the city"
                               "she left the city"
                               "she moved to the city"
                               "she said that the city"
                               "she stated the city"
                               "she asked the city"
                               "she was a city"))

(setq ac-netspeak-dictionary
                               (list "she said the city"
                               "she stated that the city"
                               "she city"
                               "she was in the city"
                               "she noted that the city"
                               "she left the city"
                               "she moved to the city"
                               "she said that the city"
                               "she stated the city"
                               "she asked the city"
                               "she was a city"))

(add-to-list 'ac-sources '("she said the city"
                               "she stated that the city"
                               "she city"
                               "she was in the city"
                               "she noted that the city"
                               "she left the city"
                               "she moved to the city"
                               "she said that the city"
                               "she stated the city"
                               "she asked the city"
                               "she was a city"))

sh

(defvar ac-source-mysource1
  '((candidates . ,ac-netspeak-dictionary)))

(defvar ac-source-mysource1
  '((candidates . ac-netspeak-candidates)))

(setq ac-sources '(ac-source-mysource1))
x
Lo

(setq ac-sources '(ac-source-netspeak ac-source-words-in-same-mode-buffers))
(setq ac-sources '(ac-source-netspeak))

(setq-default ac-sources '(ac-source-netspeak ac-source-words-in-same-mode-buffers))



(popup-menu ac-netspeak-dictionary)
(popup-menu* ac-netspeak-dictionary :point 1535)

(message (symbol-name this-command))

"eval-print-last-sexp"






(defun popup-hook ()
  (interactive)
  (when (equal (symbol-name this-command)
               "self-insert-command")
    (when (equal (buffer-substring-no-properties (- (point) 1) (point))
                 ".")
      (let ((sentence nil))
        (save-excursion
          (backward-char 3)
          (setq sentence (sentence-at-point)))
        (when
            (with-temp-buffer
              (insert sentence)
              (goto-char (point-min))
              (search-forward "?" nil t))
          (when (search-backward "?" nil t)
            (popup-menu* ac-netspeak-dictionary :point (point))))))))

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
    ;; (print (format "Response: %s" response))
    ;; (popup-delete menu2)
    (popup-menu* response :point symbol-pos :nowait t)
    (delete-backward-char 1)))
    

(add-hook 'post-self-insert-hook 'popup-hook)
(remove-hook 'post-self-insert-hook 'popup-hook)
