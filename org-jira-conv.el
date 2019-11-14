;;; .local/straight/repos/org-jira/org-jira-conv.el -*- lexical-binding: t; -*-

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (file-name-directory this-file)))

  (defun | (body cmd &rest args)
    (with-temp-buffer
      (insert body)
      (apply 'call-process-region (point-min) (point-max) cmd t t nil args)
      (buffer-substring-no-properties (point-min) (point-max))))

  (defun jira->md (body)
    (-> body
        (| "node"
           (expand-file-name "to-md.js" this-dir))))

  (defun md->org (body)
    (-> body
        (| "pandoc"
           "-f" "gfm"
           "-t" "org")))

  (defun jira->org (body) (-> body jira->md md->org))

  (defun md->jira (body)
    (-> body
        (| "node"
           (expand-file-name "to-jr.js" this-dir))))

  (defun org->md (body)
    (-> body
        (| "pandoc"
           "-f" "org"
           "-t" "md")))

  (defun org->jira (body) (-> body org->md md->jira))

  )

(defalias 'org-jira-conv-jira-to-org #'jira->org
  "Convert `BODY' from JIRA markup to org-mode.")

(defalias 'org-jira-conv-org-to-jira #'org->jira
  "Convert `BODY' from org-mode to JIRA markup.")

(provide 'org-jira-conv)
