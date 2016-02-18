module GitHub.GitData.Trees where

data TreeOptions
  = NonRecursive
  | Recursive

tree :: Owner -> Repo -> SHA -> TreeOptions -> m Tree

createTree :: Owner -> Repo -> NewTree -> m Tree
