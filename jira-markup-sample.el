;;; .local/straight/repos/org-jira/jira-markup-sample.el -*- lexical-binding: t; -*-

(setq jira-markup-sample "
h1. Biggest heading

h2. Bigger heading

h1. Biggest heading
h2. Bigger heading
h3. Big heading
h4. Normal heading
h5. Small heading
h6. Smallest heading

*strong*
_emphasis_
{{monospaced}}
??citation??
-deleted-
+inserted+
^superscript^
~subscript~

{code:javascript}
var hello = 'world';
{code}

[http://google.com]
[Google|http://google.com]

GitHub Flavor
-deleted-

{noformat}
  preformatted piece of text
  so *no* further _formatting_ is done here
{noformat}
")
