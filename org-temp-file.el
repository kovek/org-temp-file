;;; package -- Summary
;;; Code
;;; Summary
;;; Commentary:

;;; Code:

(defun my-momentarily-display-after-buffer-end (string &optional timeout)
  "Foo.
STRING TIMEOUT"
  (let ((ov (make-overlay (point)
                          (point))))
    (overlay-put ov 'after-string
                 (concat (propertize " " 'display
                                     '(space :align-to (+ left-fringe 10)))
                         (propertize string 'display
                                     '(raise -1))
                         "\n\n"))

    ))

(defvar all-temporary-overlays)
(setq all-temporary-overlays '())

(defun my-inserter (string)
  "Foo.
STRING: the string to insert."
  (let ((str-length (length string)))
    ;; (setq all-temporary-overlays (append all-temporary-overlays '(1)))
                                        ; (end-of-line)
                                        ; (newline)
    (insert string)
    (defvar temporary-data)
    (setq temporary-data (make-overlay (point) (- (point) str-length)))
    (setq all-temporary-overlays (append
                                  all-temporary-overlays
                                  (list temporary-data)))
    ))

(defun my-remover ()
  "Foo."
  ;; (setq all-temporary-overlays (append all-temporary-overlays '(1)))
  (interactive)
  (defvar my-head)
  (setq my-head (car all-temporary-overlays))
  (print my-head)
  (setq all-temporary-overlays (cdr all-temporary-overlays))
  (delete-region (overlay-start my-head) (overlay-end my-head))
  (delete-overlay my-head)
  )


;; (global-set-key (kbd "C-c C-a") (lambda () (interactive)
;;                                   (my-inserter ";;; this is a test")))
;; (global-set-key (kbd "C-c C-b") 'my-remover)

;; (defun my-insert-list (my-list)
;;   "Inserst list as org-headings.
;; MY-LIST: List of strings to insert"
;;   (defvar my-point (point))
;;   (org-set-property "org-runner-generated-content" "true")
;;   (org-metaright)
;;   (dolist (elt my-list)
;;     (end-of-visual-line)
;;     (org-insert-heading)
;;     (my-inserter elt))
;;   (goto-char my-point)
;;   (org-metaleft))

(defvar all-beg-end-pos nil)
(defun my-clean-generated-content ()
  "Deletes the headlines comming off of the current headline.
Note: Prone to getting bugs if we modify the buffer, which messes with the
org-element caching mechanism!"
  (defvar my-current-position (point))
  (with-current-buffer (clone-indirect-buffer "foo" nil)
    (goto-char my-current-position)
    ;; (print (buffer-substring my-current-position (+ my-current-position 10)))
    (org-narrow-to-subtree)
    (setq all-beg-end-pos (my-get-children-headlines))
    (print all-beg-end-pos)
    (setq all-beg-end-pos
          (sort all-beg-end-pos
                (lambda (left right)
                  (> (first left) (first right)) )))
    (dolist (beg-end-pos all-beg-end-pos)
            (delete-region (first beg-end-pos) (second beg-end-pos)))
    ))

(defun my-get-children-headlines ()
  "Gets all children under the current headline."
  (interactive)
  (defvar my-current-headline (org-element-at-point))
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (if (not (string=
                (org-element-property :raw-value headline)
                (org-element-property :raw-value my-current-headline)))
          (list (org-element-property :begin headline) (org-element-property :end headline)))
      ;; (org-element-put-property headline :foo "ach")
      ;; (print (org-element-property :foo headline))
      ;; (print (org-element-property :raw-value headline))
      )))
;; (print (point))))

(defun my-test ()
  "Gets all children under the current headline."
  (interactive)
  (edebug)
  (org-element-extract-element
   (org-element-map (org-element-parse-buffer) 'headline
     (lambda (headline)
       (if (string=
            (org-element-property :raw-value headline)
            "foo")
           headline)
       ;; (org-element-put-property headline :foo "ach")
       ;; (print (org-element-property :foo headline))
       ;; (print (org-element-property :raw-value headline))
       ))))
;; (print (point))))

(defvar after-headline)
(defvar before-next-headline)
(defvar output-contains-content) 
(defun contains-content ()
  "Check if current headline has content inside of it."
  (setq after-headline (point))
  ;; (org-end-of-line)
  (org-forward-heading-same-level 1)
  (org-beginning-of-line)
  (setq before-next-headline (point))
  (goto-char after-headline)
  (org-end-of-line)
  (skip-chars-forward "[ ]\n")
  (setq output-contains-content (not (= (point) before-next-headline)))
  (goto-char after-headline)
  output-contains-content
  )

(defvar before-star)
(defvar after-star)
(defvar additional-stars)
(defun get-current-org-level ()
  "Get current number of stars at current level."
  (org-beginning-of-line)
  (setq before-star (point))
  (search-forward " ")
  (setq after-star (point))

  (setq additional-stars
        (- after-star before-star 1))
  additional-stars
  )

(defvar old-buffer)
(defvar current-headline)
(defvar file-path)
(defvar current-headline-string)
(defvar current-line)
(defvar next-line)
(defvar file-contents)
(defvar char-limit)
(defvar last-window-start)
(defvar before-inserting-contents)
(defun my-load-org-file ()
  "Load the org-file pointed to by the link at.
point as a tree of headlines under the current headline.

Now, we need before save hook so that we could properly manage this file."
  (org-beginning-of-line)
  (forward-word)
  (setq last-window-start (window-start))
  (setq current-line (point))
  (outline-next-visible-heading 1)
  (setq next-line (point))
  (goto-char current-line)
  (setq current-headline (org-element-at-point))
  (setq current-headline-string (org-element-property :raw-value current-headline))

  (when (contains-content)
    (my-unload-org-file)
    )
  
  (setq additional-stars
        (get-current-org-level))
  (when (is-file-heading current-headline) 
    (org-narrow-to-subtree)
    (setq file-path (get-file-path-at-current-heading))
    (org-end-of-line)
    (org-insert-heading)
    (org-beginning-of-line)
    (kill-line 1)
    ;; (insert-file-contents (org-element-property :raw-link file-path))

    (setq file-contents (with-temp-buffer
                          (insert-file-contents (org-element-property :raw-link file-path))
                          (buffer-string)))

    (setq before-inserting-contents (point))
    (insert file-contents)

    (setq char-limit (point))
    


    (goto-char current-line)
    ;; (org-forward-heading-same-level 1)

    (while  (re-search-forward "^\* " nil t)
      (dotimes (n additional-stars nil)
        (org-demote-subtree)))
    (goto-char current-line)
    (widen)
    (set-window-start (selected-window) last-window-start)

    ;; (while (not (eq (what-line) current-line))
    ;;   (outline-previous-visible-heading 1)
    ;;   )

    ;; (org-element-map (org-element-parse-buffer) 'headline
    ;;   (lambda (headline)
    ;;     (goto-char (org-element-property :begin headline))
    ;;     (insert additional-stars)
    ;;     ))))
    ))

(defun get-file-path-at-current-heading ()
  "Get file path at current heading."
  (setq current-headline (org-element-at-point))
  (car (org-element-map (org-element-parse-buffer) 'link
         (lambda (some-link)
           (if (eq (org-element-property :parent some-link) current-headline)
               org-element-property :path some-link)
           ))))

(defvar current-headline-raw-value)
(defun is-file-heading (current-headline)
  "Return TRUE if the provided headline is a pointer to a file.
CURRENT-HEADLINE is an org-element"
  (setq current-headline-raw-value (org-element-property :raw-value current-headline))
  (when current-headline-raw-value
    (string-match
     "\\[FILE\\]"
     current-headline-raw-value)))

(defvar current-pos)
(defvar end-of-line)
(defvar end-of-content)
(defun get-content-bounds ()
  "Get the current headline content bounds."
  (setq current-pos (point))
  (org-end-of-line)
  (setq end-of-line (point))
  (org-forward-heading-same-level 1)    
  (org-end-of-line)
  (when (equal end-of-line (point))
    (goto-char (point-max)))
  (forward-line -1)
  (org-end-of-line)
  (setq end-of-content (point))
  (goto-char current-pos)
  (list end-of-line end-of-content)
  )

(defvar content-bounds)
(defun my-unload-org-file ()
  "Unload the org-file at the current heading."
  (when (is-file-heading (org-element-at-point))
    (setq content-bounds (get-content-bounds))
    (list (first content-bounds)
          (delete-and-extract-region (first content-bounds) (second content-bounds)))))

(defun testing ()
  (interactive)
  (defvar foo)
  (setq foo "bar")
  (print "A")
  (edebug foo)
  (print "b")
  )

(defvar hidden-files '())
(defvar num-hidden-files)
(defun hide-files ()
  "Hides all loaded files before saving.
Pollutes the undo tree!"
  (when (equal major-mode 'org-mode)
    (setq num-hidden-files 0)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (when (is-file-heading headline)
          (incf num-hidden-files)
                                        ; (save-file-content-to-buffer)
          (goto-char (org-element-property :begin headline))
          (push (my-unload-org-file) hidden-files))))))

(defvar current-headline)
(defun hide-files-not-current ()
  (setq num-hidden-files 0)
  (setq current-headline (org-element-at-point))  
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (when (and (is-file-heading headline)
                 (not (equal (org-element-property :begin headline)
                             (org-element-property :begin current-headline))))
        (incf num-hidden-files)
                                        ; (save-file-content-to-buffer)
        (goto-char (org-element-property :begin headline))
        (push (my-unload-org-file) hidden-files)))))


(defun send-save ()
  ""

  )

(defun show-files ()
  "Shows all files back."
  (when (not (equal num-hidden-files 0))
    (undo-tree-undo)))

(defvar prev-char)
(defvar temp-hidden-file)
(defun show-saved-files ()
  "Show all files that were pushed to hidden-files back."
  (when (equal major-mode 'org-mode)
    (setq prev-char (point))
    (while hidden-files
      (setq temp-hidden-file (pop hidden-files))
      (when temp-hidden-file
        (goto-char (first temp-hidden-file))
        (insert (second temp-hidden-file))))
    (goto-char prev-char)))

(add-hook 'before-save-hook 'hide-files)
(add-hook 'after-save-hook 'show-saved-files)

(defvar current-position)
(defun reload-and-save ()
  (reload-files)
  (save-files))

(defun reload-files ()

  )

(defun goto-first-file-up ()
  "Go to the first file heading traveling up if there is one."
  (while (and
          (not (is-file-heading (org-element-at-point)))
          (or (not (= (get-current-org-level) 1))
              (not (= (what-line) 1))))
    (outline-up-heading 1)))


(defvar current-file-headline-level)
(defvar current-file-headline-begin)
(defun save-current-file ()
  "Save current file."
  (setq current-position (point))
  (goto-first-file-up)
  (when (not (= (point) 1))
    (setq file-path (org-element-property :raw-link (get-file-path-at-current-heading)))
    (org-narrow-to-subtree)
    (setq current-file-headline-level (get-current-org-level))
    (org-beginning-of-line)
    (setq current-file-headline-begin (point))
    ;; (debug)
    (hide-files-not-current)
    ;; Undecrement by number of stars.
    (goto-char (point-min))
    (forward-line)
    ;; (set-mark-command)
    ;; (while (not (equal
    ;;              (get-current-org-level)
    ;;              (+ 1 current-file-headline-level)))
    ;;   (outline-up-heading))
    ;; (while (not (equal (point) current-file-headline-begin))
    ;;   (dotimes (n current-file-headline-level nil)
    ;;     (org-promote-subtree))
    ;;   (outline-backward-heading-same-level 1))

    (while  (re-search-forward "^\*" nil t)
      (dotimes (n additional-stars nil)
        (org-do-promote)
        (forward-line)))
    (setq content-bounds (get-content-bounds))

    (goto-char (point-min))
    (forward-line)
    (setq content-bounds (list (point) (point-max)))

    ;; (debug content-bounds)
    (write-region (first content-bounds) (second content-bounds) file-path)
    (goto-char (point-min))
    (forward-line)

    (while  (re-search-forward "^\*" nil t)
      (dotimes (n additional-stars nil)
        (org-metaright)
        (forward-line)))
    (show-saved-files)
    (goto-char (point-min))
    (widen)
    ))

;; (defvar all-generated-contents-temporary-backup)
;; (defvar file-path)
(add-hook 'after-save-hook 'save-files)
;; (lambda ()
;;   ;; Hide all loaded files in current file.
;;   (org-element-map
;;       (org-element-parse-buffer)
;;       'headline
;;     (lambda (headline)
;;       (if (is-file-heading headline)
;;           (goto-char (org-element-property :begin headline))
;;         (end-of-line)
;;         (delete-region
;;          (+ (point) 1)
;;          (org-element-property :end headline))
;;         (push (my-clean-generated-content) all-generated-contents-temporary-backup)
;;         )))))

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (dolist (generated-contents all-generated-contents-temporary-backup)
;;               (goto-char (first generated-contents))
;;               (insert (second generated-contents))
;;             ))



;;   ;; (insert-file-contents)                
;;   (setq old-buffer (current-buffer))
;;   (with-temp-buffer (load-file-as-buffer
;;                      (org-element-property :link (org-element-at-point))
;;                      (setq full-buffer-data (get-full-buffer-data))
;;                      (insert into old-buffer full-buffer-data))
;;                     ; Connect a region in two buffers together. 
;; )

;; (defvar my-current-headline (org-element-at-point))
;; (with-temp-buffer (current-buffer)
;;                   (goto-char my-current-position)
;;                   (print (buffer-substring (point) (+ (point) 100)))
;;                   ;; (org-narrow-to-subtree)
;;                   ;; (delete-region 1 10)
;;                   ))
;;   ;; (delete-region (point) (+ (point) 5))))

(defun google-for (query-string)
  "Googles for query.
QUERY-STRING: The query to send to google.
Displays a list of results on the next line."
  (my-inserter "test"))
;; (with-current-buffer (url-retrieve-synchronously
;;                       (concatenate 'string "http://google.com/search?q=" query-string))
;;   (defvar my-foo)
;;   (setq my-foo (prog1
;;                    (buffer-string)
;;                  (kill-buffer))))
;; (my-inserter my-foo)
;; )

(provide 'org-temp-file)

;;; org-temp-file.el ends here
