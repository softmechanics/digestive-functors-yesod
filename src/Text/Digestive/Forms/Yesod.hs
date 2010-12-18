-- | Module providing a yesod backend for the digestive-functors library
--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports, TypeSynonymInstances #-}
module Text.Digestive.Forms.Yesod
    ( YesodForm
    , eitherYesodForm
    , runFormGet
    , runFormPost
    ) where

import Control.Monad (liftM)
import "transformers" Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LB
import Text.Digestive.Forms (FormInput (..))
import Text.Digestive.Types (Form (..), View (..), Environment (..), viewForm, runForm, eitherForm)
import Text.Digestive.Result (Result (..))
import Yesod.Request
import Network.Wai (requestMethod)
import qualified Text.Hamlet as H
import Text.Blaze.Renderer.String (renderHtml)
import Text.Digestive.Blaze.Html5 (BlazeFormHtml, renderFormHtml)

-- | Form input type.  String is for most types, lazy ByteString for file uploads
type Input = Either ParamValue FileInfo 

instance FormInput Input (String, LB.ByteString) where
    getInputString = either Just (const Nothing)
    getInputFile = either (const Nothing) (\(FileInfo n _ v) -> Just (n,v))

-- | Simplification of the `Form` type, instantiated to Yesod
--
type YesodForm m e v a = Form m Input e v a

instance H.ToHtml BlazeFormHtml where
  toHtml = H.preEscapedString . renderHtml . fst . renderFormHtml

-- | Environment that will fetch input from the parameters parsed by Yesod
--
yesodEnvironmentPost :: (MonadIO m, RequestReader m) => m (Environment m Input)
yesodEnvironmentPost = do
  rr <- getRequest
  (sps, fps) <- liftIO $ reqRequestBody rr
  return $ environment (stringParams sps ++ fileParams fps) 

yesodEnvironmentGet :: (MonadIO m, RequestReader m) => m (Environment m Input)
yesodEnvironmentGet = do
  rr <- getRequest
  return . environment . stringParams $ reqGetParams rr

environment :: Monad m => [(ParamName, Input)] -> Environment m Input
environment inputs = Environment go
  where go fid = return $ lookup (show fid) inputs 

stringParams :: [(ParamName, ParamValue)] -> [(ParamName, Input)]
stringParams = map (mapSnd Left)

fileParams :: [(ParamName, FileInfo)] -> [(ParamName, Input)]
fileParams = map (mapSnd Right)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a,b) = (a, f b)

-- | Run a Yesod form using GET fields
runFormGet :: (MonadIO m, RequestReader m) => YesodForm m e v a -> String -> m (View e v, Result e a)
runFormGet form name = yesodEnvironmentGet >>= runForm form name

-- | Run a Yesod form using POST fields
runFormPost :: (MonadIO m, RequestReader m) => YesodForm m e v a -> String -> m (View e v, Result e a)
runFormPost form name = yesodEnvironmentPost >>= runForm form name

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
    else yesodEnvironmentPost >>= eitherForm form name 

