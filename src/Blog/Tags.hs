module Blog.Tags where

import Data.List (intersperse)
import Hakyll.Core.Compiler (Compiler, makeItem)
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Item (Item(..))
import Hakyll.Core.Metadata (MonadMetadata)
import Hakyll.Web.Tags (getTags)
import Hakyll.Web.Template (loadAndApplyTemplate)
import Hakyll.Web.Template.Context (Context, field, listFieldWith)

-- Only add a tags field if one is present in the metadata.
loadAndApplyTemplateWithTags :: Identifier -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplateWithTags tid ctx item = hasTags item >>= withTags
  where
    hasTags = (fmap (not . null)) . (getTags . itemIdentifier)

    withTags False = loadAndApplyTemplate tid ctx item
    withTags True  = loadAndApplyTemplate tid (tagsListField <> ctx) item

tagsListField :: Context a
tagsListField =
    listFieldWith "tags" (field "tag" tag) tags
  where
    tag = pure . itemBody
    tags item = (getNormalizedTags . itemIdentifier) item >>= (sequence . fmap makeItem)

getNormalizedTags :: MonadMetadata m => Identifier -> m [String]
getNormalizedTags id = do
    tags <- getTags id
    pure $ normalizeTag <$> tags
  where
    normalizeTag = concat . intersperse "-" . words
