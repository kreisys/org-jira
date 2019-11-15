;;; .local/straight/repos/org-jira/org-jira-conv.el -*- lexical-binding: t; -*-

(require 'ox-jira-monkeypatch)

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (file-name-directory this-file)))

  (cl-labels
      ((| (body cmd &rest args)
          (with-temp-buffer
            (insert body)
            (apply 'call-process-region (point-min) (point-max) cmd t t nil args)
            (buffer-substring-no-properties (point-min) (point-max))))

       (jira->gfm (body)
                  (-> body
                      (| "node"
                         (expand-file-name "jira-to-gfm.js" this-dir))))

       (gfm->org (body)
                 (-> body
                     (| "pandoc"
                        "-f" "gfm"
                        "-t" "org")))

       (jira->org (body) (-> body jira->gfm gfm->org))

       (org->jira (body)
                  (let ((org-export-show-temporary-export-buffer nil))
                    (with-temp-buffer
                      (insert body)
                      (with-current-buffer
                          (ox-jira-export-as-jira)
                        (buffer-substring-no-properties (point-min) (point-max))))))

       ;; (org->jira (body) (-> body org->md md->jira))
       )

    (defalias 'org-jira-conv-org-to-md #'org->md)
    (defalias 'org-jira-conv-jira-to-org #'jira->org
      "Convert `BODY' from JIRA markup to org-mode.")

    (defalias 'org-jira-conv-org-to-jira #'org->jira
      "Convert `BODY' from org-mode to JIRA markup.")))

(provide 'org-jira-conv)
