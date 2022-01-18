;;; extends/rails-i18n-search/rails-i18n-search.el -*- lexical-binding: t; -*-

(defun rails-i18n-search-replace-region(begin end file-path option)
  (let ((region (buffer-substring begin end))
        (rp nil)
        (findup nil))
    ;;(rails-i18n-search-show-translation-with-open-buffer region)
    (with-current-buffer
        (find-file-noselect file-path)
      (beginning-of-buffer)
      (let ((rg
             (concat "\\([ ]+\\)\\([a-zA-Z0-9_]+\\)\\(: "
                     region
                     "$\\)")))
        (when (search-forward-regexp rg nil t)
          ;; can base on level, and collect as one?
          ;; yes we can!
          (setq findup (match-string 2))
          ;; get level
          (let* ((space (match-string 1))
                 (countor (- (length space) 2))
                 (stack nil))
            (while (> countor 0)
              (when (search-backward-regexp
                     (concat "\\(^"
                             (substring space (- countor))
                             "\\)"
                             "\\([a-zA-Z0-9_]*\\)"
                             "\\(:\\)")
                     nil t)
                (setq findup (concat (match-string 2) "." findup)))
              (setq countor (- countor 2)))))

        (unless findup
          (setq findup (rails-i18n-insert-new-var region)))))
    ;; go back
    (save-excursion
      (delete-region begin end)
      (cond (option (insert (format "{{ $t('%s') }}" findup)))
            (t (insert (format "$t('%s')" findup)))))))

(defun rails-i18n-search-dwim (opt)
  (unless (boundp 'rails-i18n-search--data-file)
    (rails-i18n-search-set-local-data-file))
  (cond ((region-active-p)
         (rails-i18n-search-replace-region
          (region-beginning)
          (region-end)
          rails-i18n-search--data-file opt))))

(defun rails-i18n-search-set-local-data-file()
  (interactive)
  (setq-local
   rails-i18n-search--data-file
   (car (find-file-read-args
         "Find yml file: "
         (confirm-nonexistent-file-or-buffer)))))

(defun rails-i18n-search-show-translation-with-open-buffer (region)
  (unless (boundp 'workflow--i18n-dictionary-file)
    (rails-i18n-search-set-local-dictionary-file))
  (with-current-buffer (find-file-noselect rails-i18n-search--dictionary-file)
    (beginning-of-buffer)
    (when (search-forward-regexp region nil t))))

(defun rails-i18n-search-set-local-dictionary-file()
  (interactive)
  (setq-local
   rails-i18n-search--dictionary-file
   (car (find-file-read-args
         "Find dictionary: "
         (confirm-nonexistent-file-or-buffer)))))

(defun rails-i18n-insert-new-var (region)
  (save-excursion
    (save-restriction
      (let ((space "")
            (keep-read t)
            (var "")
            (prompt (concat "type : to finish, i18n key " region " -> ")))
        (while keep-read
          (let((candidate nil)
               (input "")
               (new-tab t))
            (setq space (concat space "  "))
            (end-of-buffer)
            (while (search-backward-regexp
                    (concat "\\(^" space "\\)"
                            "\\([a-zA-Z_]*\\)"
                            "\\(:[ ]*\\)") nil t)
              (push (cons (match-string 2) (point))
                    candidate))
            (setq input
                  (completing-read
                   (concat prompt var)
                   (mapcar (lambda (c) (car c)) candidate)))
            (when (string-match ":$" input)
              (end-of-buffer)
              (setq var (concat var (substring input 0 -1)))
              (insert (concat space input " " region "\n"))
              (setq keep-read nil))
            (if (string-match "\\.$" input)
                (setq input (substring input 0 -1)))
            (let ((select nil)
                  (break nil))
              (while (and keep-read (not break) candidate)
                (setq select (pop candidate))
                (when (string= (car select) input)
                  (setq var (concat var input "."))
                  (narrow-to-region
                   (cdr select)
                   (if candidate
                       (cdr (pop candidate))
                     (point-max)))
                  (setq break t new-tab nil))))
            (when (and new-tab keep-read)
              (end-of-buffer)
              (insert (concat space input ":\n"))
              (setq var (concat var input ".")))))
        var))))

(defun rails-i18n-search-dwim-html-style ()
  (interactive)
  (rails-i18n-search-dwim t))

(defun rails-i18n-search-dwim-variable-style ()
  (interactive)
  (rails-i18n-search-dwim nil))

;;; new meet

(defun rails-i18n-search-replace-region-format(format-seed begin end file-path)
  (let ((region (buffer-substring begin end))
        (rp nil)
        (findup nil))
    ;;(rails-i18n-search-show-translation-with-open-buffer region)
    (with-current-buffer
        (find-file-noselect file-path)
      (beginning-of-buffer)
      (let ((rg
             (concat "\\([ ]+\\)\\([a-zA-Z0-9_]+\\)\\(: "
                     region
                     "$\\)")))
        (when (search-forward-regexp rg nil t)
          ;; can base on level, and collect as one?
          ;; yes we can!
          (setq findup (match-string 2))
          ;; get level
          (let* ((space (match-string 1))
                 (countor (- (length space) 2))
                 (stack nil))
            (while (> countor 0)
              (when (search-backward-regexp
                     (concat "\\(^"
                             (substring space (- countor))
                             "\\)"
                             "\\([a-zA-Z0-9_]*\\)"
                             "\\(:\\)")
                     nil t)
                (setq findup (concat (match-string 2) "." findup)))
              (setq countor (- countor 2)))))))
    ;; go back
    (if findup
        (save-excursion
          (delete-region begin end)
          (insert (format format-seed findup))))
    findup))

(defvar rails-i18n-search-default-yml nil)
(defvar rails-i18n-search-extend-yml nil)

(defun rails-i18n-search-replace-regex-select-style ()
  (interactive)
  (when (region-active-p)
    (let* ((style-list '(("html-plain-text" . "{{ $t('%s') }}")
                         ("html-in-tage   " . "$t('%s')")
                         ("js-style       " . "this.$t('%s')")))
           (select (completing-read "check your code style: "
                                    (mapcar (lambda (c) (concat (car c) "\t" (cdr c))) style-list)))
           (style nil))
      (dolist (s style-list)
        (if (string-match (car s) select) (setq style (cdr s))))
      (unless (rails-i18n-search-replace-region-format
               style
               (region-beginning)
               (region-end) rails-i18n-search-default-yml)
        (unless (rails-i18n-search-replace-region-format
         style
         (region-beginning)
         (region-end) rails-i18n-search-extend-yml)
          (message "No match i18n value, update yml file?"))))))

(provide 'rails-i18n-search)
