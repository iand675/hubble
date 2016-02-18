module GitHub.Miscellaneous.Gitignore where

templates :: m (Vector TemplateName)
templates = get (v3 json) "/gitignore/templates" ""

template :: TemplateName -> m Template
template (TemplateName n) = get (v3 json) ("/gitignore/templates/" <> encode n) ""

