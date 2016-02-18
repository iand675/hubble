module GitHub.GitData.Tags where

getTag :: Owner -> Repo -> SHA -> m Tag

createTag :: Owner -> Repo -> NewTag -> m Tag

