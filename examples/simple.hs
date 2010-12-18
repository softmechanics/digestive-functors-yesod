{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, OverloadedStrings #-}

import Text.Digestive.Forms.Yesod
import Control.Applicative
import Yesod hiding (Html, renderHtml, runFormPost)
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Blaze.Html5 (Html)
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

resultView :: View e v -> Result e ok -> v
resultView v = unView v . resultErrors

resultErrors (Error e) = e
resultErrors _ = []

uploadForm :: YesodForm Handler Html BlazeFormHtml Upload
uploadForm = Upload
  <$> fmap (fromMaybe "Unknown" . fmap snd) inputFile 
  <*> (<++ errors) (inputTextRead "Required" Nothing) 

uploadFormWidget :: BlazeFormHtml -> GWidget Test Test ()
uploadFormWidget form = 
  addHamlet [$hamlet|
    %form!method=post!enctype=multipart/form-data
      $formHtml$
      %input!type=submit
    |]
  where formHtml = H.toHtml form

getUploadR :: Handler RepHtml
getUploadR = do
  form <- viewForm uploadForm "upload"
  defaultLayout $ uploadFormWidget form
  
postUploadR :: Handler RepHtml
postUploadR = do
  (view, res) <- runFormPost uploadForm "upload"
  defaultLayout $ do
    case res of
         Error _ -> return ()
         Ok up    -> addHtml . H.toHtml $ show up
    uploadFormWidget $ resultView view res

main = basicHandler 3000 Test 

