module GitHub.GitData.Blobs where

getBlob :: Owner -> Repo -> SHA -> m Blob

createBlob :: Owner -> Repo -> NewBlob -> m CreatedBlob

