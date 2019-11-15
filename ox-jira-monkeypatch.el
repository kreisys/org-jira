;;; .local/straight/repos/org-jira/ox-jira-monkeypatch.el -*- lexical-binding: t; -*-

(defun ox-jira-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Jira.
CONTENTS holds the contents of the src-block.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((title (apply #'concat (org-export-get-caption src-block)))
           (lang (org-element-property :language src-block))
           (code (org-export-format-code-default src-block info))
           (collapse (if (< (plist-get info :src-collapse-threshold)
                            (org-count-lines code))
                         "true" "false")))
      (concat
       ;; (format "{code:title=%s|language=%s|collapse=%s}" title lang collapse)
       (format "{code:%s}\n" lang)
       code
       "{code}"))))
