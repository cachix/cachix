module Cachix.API.Types.MultipartUpload where

import Data.Text (Text)
import Data.UUID (UUID)

data CreateMultipartUpload = CreateMultipartUpload
  { key :: UUID,
    uploadId :: Text
  }
