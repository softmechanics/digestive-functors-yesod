-- | Module providing a yesod backend for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, TypeSynonymInstances #-}
module Text.Digestive.Forms.Yesod
    ( YesodForm
    , yesodEnvironment
    , eitherYesodForm
    ) where

import Control.Monad (liftM)
import Control.Applicative
import "transformers" Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LB
import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), Environment (..), viewForm, eitherForm)
import Yesod.Request
import Network.Wai (requestMethod)
import qualified Text.Hamlet as H
import Text.Blaze.Renderer.String (renderHtml)
import Text.Digestive.Blaze.Html5 (BlazeFormHtml, renderFormHtml)
import Data.Maybe (fromMaybe)

-- | Form input type.  String is for most types, lazy ByteString for file uploads
newtype Input = Input { fromInput :: Either ParamValue FileInfo }

instance FormInput Input (String, LB.ByteString) where
    getInputString = either Just (const Nothing) . fromInput
    getInputFile = either (const Nothing) (\(FileInfo n _ v) -> Just (n,v)) . fromInput

-- | Simplification of the `Form` type, instantiated to Yesod
--
type YesodForm m e v a = Form m Input e v a

instance H.ToHtml BlazeFormHtml where
  toHtml = H.preEscapedString . renderHtml . fst . renderFormHtml

-- | Environment that will fetch input from the parameters parsed by Yesod
--
yesodEnvironment :: (MonadIO m, RequestReader m) => Environment m Input
yesodEnvironment = Environment lookupFormId
  where lookupFormId fid = do
          let key = show fid
          rr <- getRequest
          (sFields, fFields) <- liftIO $ reqRequestBody rr
          return $ fromMaybe (Input . Right <$> lookup key fFields) (Just. Input . Left <$> lookup key sFields)

-- | Run a Yesod form
--
-- * When we are responding to a GET request, you will simply receive the form
-- as a view
--
-- * When we are responding to another request method, the form data will be
-- used. When errors occur, you will receive the form as a view, otherwise,
-- you will get the actual result
--
eitherYesodForm :: (MonadIO m, Functor m, RequestReader m)
                => YesodForm m e v a -- ^ Form
                -> String -- ^ Form name
                -> m (Either v a) -- ^ Result
eitherYesodForm form name = do
  wai <- waiRequest 
  if requestMethod wai == "GET"
    then liftM Left $ viewForm form name
    else eitherForm form name yesodEnvironment 

