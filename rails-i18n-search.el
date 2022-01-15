;;; extends/rails-i18n-search/rails-i18n-search.el -*- lexical-binding: t; -*-

(defun rails-i18n-search-replace-region(begin end file-path option)
  (let ((region (buffer-substring begin end))
        (rp nil)
        (finded nil))
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
          (setq finded (match-string 2))
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
                (setq finded (concat (match-string 2) "." finded)))
              (setq countor (- countor 2)))))

        (unless finded
          (setq finded (rails-i18n-search-new-var region)))))
    ;; go back
    (save-excursion
      (delete-region begin end)
      (cond (option (insert (format "{{ $t('%s') }}" finded)))
            (t (insert (format "$t('%s')" finded)))))))

(defun rails-i18n-search-dwim (opt)
  (unless (boundp 'workflow--i18n-data-file)
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

(defun rails-i18n-search-new-var (region)
  (let ((space "")
        (input "")
        (finish nil)
        (prompt (concat "type : to finish, i18n key " region " -> "))
        (var ""))
    (save-excursion
      (save-restriction
        (while (not finish)
          (end-of-buffer)
          (setq space (concat space "  "))
          (let ((condidate nil))
            (while (search-backward-regexp
                    (concat "\\(^" space "\\)"
                            "\\([a-zA-Z_]*\\)"
                            "\\(:[ ]*\\)") nil t)
              (push (cons (match-string 2) (point))
                    condidate))
            ;;(beginning-of-buffer)
            (setq input
                  (completing-read
                   (concat prompt var)
                   (mapcar (lambda (c) (car c)) condidate)))
            (let ((break nil)
                  (select nil)
                  (new-tab t))
              (while (and (not break) condidate)
                (setq select (pop condidate))
                (cond ((string= input (car select))
                       (setq var (concat var (car select) "."))
                       (narrow-to-region
                        (cdr select)
                        (if condidate
                            (cdr (pop condidate))
                          (point-max)))
                       (setq new-tab nil)
                       (setq break t))
                      (t (setq new-tab t))))
              (while (and (not finish) new-tab)
                (goto-char (point-max))
                (cond
                 ((string-match ":$" input)
                  (setq var (concat var (substring input 0 -1)))
                  (setq finish t)
                  (insert (concat space input " " region "\n")))
                 ((string-match "\\.$" input)
                  (setq var (concat var input))
                  (insert (concat space (substring input 0 -1) ":\n"))
                  (setq space (concat space "  "))
                  (setq input
                        (read-string (concat prompt var))))
                 (t
                  (insert (concat space input ":\n"))
                  (setq var (concat var input "."))
                  (setq space (concat space "  "))
                  (setq input
                        (completing-read (concat prompt var) nil)))
                 ))
              )
            )) var))))

(defun rails-i18n-search-dwim-html-style ()
  (interactive)
  (rails-i18n-search-dwim t))

(defun rails-i18n-search-dwim-variable-style ()
  (interactive)
  (rails-i18n-search-dwim nil))

(provide 'rails-i18n-search)
