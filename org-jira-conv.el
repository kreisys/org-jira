;;; .local/straight/repos/org-jira/org-jira-conv.el -*- lexical-binding: t; -*-

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (file-name-directory this-file)))

  (cl-labels
      ((| (body cmd &rest args)
          (with-temp-buffer
            (insert body)
            (apply 'call-process-region (point-min) (point-max) cmd t t nil args)
            (buffer-substring-no-properties (point-min) (point-max))))

       (jira->md (body)
                 (-> body
                     (| "node"
                        (expand-file-name "to-md.js" this-dir))))

       (md->org (body)
                (-> body
                    (| "pandoc"
                       "-f" "gfm"
                       "-t" "org")))

       (jira->org (body) (-> body jira->md md->org))

       (md->jira (body)
                 (-> body
                     (| "node"
                        (expand-file-name "to-jr.js" this-dir))))

       (org->jira (body)
                  (let ((org-export-show-temporary-export-buffer nil))
                    (with-temp-buffer
                      (insert body)
                      (with-current-buffer
                          (org-confluence-export-as-confluence nil nil t t)
                        (buffer-substring-no-properties (point-min) (point-max))))))

       ;; (org->jira (body) (-> body org->md md->jira))
       )

    (defalias 'org-jira-conv-org-to-md #'org->md)
    (defalias 'org-jira-conv-jira-to-org #'jira->org
      "Convert `BODY' from JIRA markup to org-mode.")

    (defalias 'org-jira-conv-org-to-jira #'org->jira
      "Convert `BODY' from org-mode to JIRA markup.")))

;; (let (
;;       (txt "
;; #+BEGIN_SRC bash
;; echo hi
;; #+END_SRC
;; "))

;;   (org->jira txt))

;; (defun org->jira (body)
;;   (let ((org-export-show-temporary-export-buffer nil))
;;     (with-temp-buffer
;;       (insert body)
;;       (with-current-buffer
;;           (org-confluence-export-as-confluence)
;;         (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'org-jira-conv)
