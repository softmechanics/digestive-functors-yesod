{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}

import Text.Digestive.Forms.Yesod
import Control.Applicative
import Yesod hiding (Html, renderHtml)
import Text.Digestive
import Text.Digestive.Blaze.Html5 (BlazeFormHtml, inputText, inputFile, renderFormHtml, FormHtml)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Network.Wai.Handler.SimpleServer
import qualified Text.Hamlet as H


data Test = Test

type Handler = GHandler Test Test

mkYesod "Test" [parseRoutes|
/ UploadR GET POST
|]

instance Yesod Test where
  approot _ = ""

data Upload = Upload LB.ByteString String
  deriving (Show)

uploadForm :: YesodForm Handler BlazeFormHtml BlazeFormHtml Upload
uploadForm = Upload
  <$> fmap (fromMaybe "Unknown" . fmap snd) inputFile 
  <*> inputText Nothing

myRenderForm :: BlazeFormHtml -> GWidget Test Test ()
myRenderForm form = 
  addHamlet [$hamlet|
    %form!method=post!enctype=multipart/form-data
      $formHtml$
      %input!type=submit
    |]
  where formHtml = H.toHtml form

getUploadR :: Handler RepHtml
getUploadR = do
  form <- viewForm uploadForm "upload"
  defaultLayout $ myRenderForm form
  
postUploadR :: Handler RepHtml
postUploadR = do
  result <- eitherYesodForm uploadForm "upload"
  defaultLayout $
    case result of
         Left v -> myRenderForm v
         Right u -> addHtml $ H.toHtml $ show u

main = basicHandler 3000 Test 

