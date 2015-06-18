{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
-- | Helper functions for creating forms when using <http://materializecss.com/ Materializ>.
--

module Yesod.Form.Materialize.Fields
       (matIntField
       ,matPasswordField
       ,matTextField
       ,matPasswordField
       ,matTextareaField
       ,matHiddenField
       ,matIntField
       ,matDayField
       ,matTimeFieldTypeTime
       ,matTimeFieldTypeText
       ,matHtmlField
       ,matEmailField
       ,matMultiEmailField
       ,matUrlField
       ,matDoubleField
       ,matBoolField
       ,matCheckBoxField
       ,matFileField
       ,matSelectField
       ,matSelectFieldList
       ,matRadioField
       ,matRadioFieldList
       ,matCheckboxesField
       ,matCheckboxesFieldList
       ,matMultiSelectField
       ,matMultiSelectFieldList
       ,ColSize(..)
       ) where

import Yesod.Form.Types
import Yesod.Form.Fields
import Yesod.Form.I18n.English
import Yesod.Form.Functions (parseHelper)
import Yesod.Core
import Text.Hamlet
import Text.Blaze (Markup, ToMarkup (toMarkup), unsafeByteString)
#define ToHtml ToMarkup
#define toHtml toMarkup
#define preEscapedText preEscapedToMarkup
import Text.Cassius
import Data.Time (Day, TimeOfDay(..))
import qualified Text.Email.Validate as Email
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI (parseURI)
import Database.Persist.Sql (PersistField, PersistFieldSql (..))
import Database.Persist (Entity (..), SqlType (SqlString), PersistEntityBackend)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Control.Monad (when, unless)
import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Blaze.ByteString.Builder.Html.Utf8 as B
import Blaze.ByteString.Builder (writeByteString, toLazyByteString)
import Blaze.ByteString.Builder.Internal.Write (fromWriteList)

import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text as T ( Text, append, concat, cons, head
                      , intercalate, isPrefixOf, null, unpack, pack, splitOn
                      )
import qualified Data.Text as T (drop, dropWhile)  
import qualified Data.Text.Read

import qualified Data.Map as Map
import Yesod.Persist (selectList, runDB, Filter, SelectOpt, Key, YesodPersist, PersistEntity, PersistQuery)
import Control.Arrow ((&&&))

import Control.Applicative ((<$>), (<|>))

import Data.Attoparsec.Text (Parser, char, string, digit, skipSpace, endOfInput, parseOnly)

import Yesod.Persist.Core

data ColSize = ColS1 | ColS2 | ColS3 | ColS4 | ColS5 | ColS6 | ColS7 | ColS8 | ColS9 | ColS10 | ColS11 | ColS12
             | ColM1 | ColM2 | ColM3 | ColM4 | ColM5 | ColM6 | ColM7 | ColM8 | ColM9 | ColM10 | ColM11 | ColM12
             | ColL1 | ColL2 | ColL3 | ColL4 | ColL5 | ColL6 | ColL7 | ColL8 | ColL9 | ColL10 | ColL11 | ColL12

colClass :: ColSize -> Markup
colClass ColS1  = "col s1"
colClass ColS2  = "col s2"
colClass ColS3  = "col s3"
colClass ColS4  = "col s4"
colClass ColS5  = "col s5"
colClass ColS6  = "col s6"
colClass ColS7  = "col s7"
colClass ColS8  = "col s8"
colClass ColS9  = "col s9"
colClass ColS10 = "col s10"
colClass ColS11 = "col s11"
colClass ColS12 = "col s12"
colClass ColM1  = "col m1"
colClass ColM2  = "col m2"
colClass ColM3  = "col m3"
colClass ColM4  = "col m4"
colClass ColM5  = "col m5"
colClass ColM6  = "col m6"
colClass ColM7  = "col m7"
colClass ColM8  = "col m8"
colClass ColM9  = "col m9"
colClass ColM10 = "col m10"
colClass ColM11 = "col m11"
colClass ColM12 = "col m12"
colClass ColL1  = "col l1"
colClass ColL2  = "col l2"
colClass ColL3  = "col l3"
colClass ColL4  = "col l4"
colClass ColL5  = "col l5"
colClass ColL6  = "col l6"
colClass ColL7  = "col l7"
colClass ColL8  = "col l8"
colClass ColL9  = "col l9"
colClass ColL10 = "col l10"
colClass ColL11 = "col l11"
colClass ColL12 = "col l12"

-- | Creates a input with @type="number"@ and @step=1@.
matIntField :: (Monad m, Integral i, RenderMessage (HandlerSite m) FormMessage) => Markup -> ColSize -> Field m i
matIntField l col = intField { fieldView = view }
  where showVal = either id (pack . showI)
        showI x = show (fromIntegral x :: Integer)
        view theId name attrs val isReq = toWidget [hamlet|\
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="number" step=1 :isReq:required="" value="#{showVal val}">
  <label for="#{theId}">#{l}
|]

-- | Creates a input with @type="number"@ and @step=any@.
matDoubleField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> ColSize -> Field m Double
matDoubleField l col =  doubleField {fieldView = view}
  where showVal = either id (pack . show)
        view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="number" step=any :isReq:required="" value="#{showVal val}">
  <label for="#{theId}">#{l}
|]

-- | Creates an input with @type="date"@, validating the input using the 'parseDate' function.
--
-- Add the @time@ package and import the "Data.Time.Calendar" module to use this function.
matDayField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> ColSize -> Field m Day
matDayField l col = dayField { fieldView = view }
  where showVal = either id (pack . show)
        view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="date" :isReq:required="" value="#{showVal val}">
  <label for="#{theId}">#{l}
|]

-- | Creates an input with @type="time"@. <http://caniuse.com/#search=time%20input%20type Browsers not supporting this type> will fallback to a text field, and Yesod will parse the time as described in 'timeFieldTypeText'.
-- 
-- Add the @time@ package and import the "Data.Time.LocalTime" module to use this function.
--
-- Since 1.4.2
matTimeFieldTypeTime :: Monad m
                     => RenderMessage (HandlerSite m) FormMessage
                     => Markup -> ColSize -> Field m TimeOfDay
matTimeFieldTypeTime = matTimeFieldOfType "time"

-- | Creates an input with @type="text"@, parsing the time from an [H]H:MM[:SS] format, with an optional AM or PM (if not given, AM is assumed for compatibility with the 24 hour clock system).
-- 
-- Add the @time@ package and import the "Data.Time.LocalTime" module to use this function.
--
-- Since 1.4.2
matTimeFieldTypeText :: Monad m
                     => RenderMessage (HandlerSite m) FormMessage
                     => Markup -> ColSize -> Field m TimeOfDay
matTimeFieldTypeText = matTimeFieldOfType "text"

matTimeFieldOfType :: Monad m
                   => RenderMessage (HandlerSite m) FormMessage
                   => Text -> Markup -> ColSize -> Field m TimeOfDay
matTimeFieldOfType inputType l col = Field
    { fieldParse = parseHelper parseTime
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="#{inputType}" :isReq:required="" value="#{showVal val}">
  <label for="#{theId}">#{l}
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . show . roundFullSeconds)
    roundFullSeconds tod =
        TimeOfDay (todHour tod) (todMin tod) fullSec
      where
        fullSec = fromInteger $ floor $ todSec tod


-- | Creates a @\<textarea>@ tag whose input is sanitized to prevent XSS attacks and is validated for having balanced tags.
matHtmlField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> ColSize -> Field m Html
matHtmlField l col = htmlField { fieldView = view }
  where showVal = either id (pack . renderHtml)
        view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <textarea.materialize-textarea :isReq:required="" id="#{theId}" name="#{name}" *{attrs}>#{showVal val}
  <label for="#{theId}">#{l}
|]

-- | Creates a @\<textarea>@ tag whose returned value is wrapped in a 'Textarea'; see 'Textarea' for details.
matTextareaField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> ColSize -> Field m Textarea
matTextareaField l col = textareaField { fieldView = view }
  where view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <textarea.materialize-textarea id="#{theId}" name="#{name}" :isReq:required="" *{attrs}>#{either id unTextarea val}
  <label for="#{theId}">#{l}
|]

-- | Creates an input with @type="hidden"@; you can use this to store information in a form that users shouldn't see (for example, Yesod stores CSRF tokens in a hidden field).
matHiddenField :: (Monad m, PathPiece p, RenderMessage (HandlerSite m) FormMessage)
               => Field m p
matHiddenField = hiddenField

-- | Creates a input with @type="text"@.
matTextField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> ColSize -> Field m Text
matTextField l col = textField { fieldView = view }
  where view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either id id val}">
  <label for="#{theId}">#{l}
|]
        
-- | Creates an input with @type="password"@.
matPasswordField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> ColSize -> Field m Text
matPasswordField l col = passwordField { fieldView = view }
  where view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="password" :isReq:required="" value="#{either id id val}">
  <label for="#{theId}">#{l}
|]


-- | Creates an input with @type="email"@. Yesod will validate the email's correctness according to RFC5322 and canonicalize it by removing comments and whitespace (see "Text.Email.Validate").
matEmailField :: Monad m
              => RenderMessage (HandlerSite m) FormMessage
              => Markup -> ColSize -> Field m Text
matEmailField l col = emailField { fieldView = view }
  where view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="email" :isReq:required="" value="#{either id id val}">
  <label for="#{theId}">#{l}
|]

-- | Creates an input with @type="email"@ with the <http://www.w3.org/html/wg/drafts/html/master/forms.html#the-multiple-attribute multiple> attribute; browsers might implement this as taking a comma separated list of emails. Each email address is validated as described in 'emailField'.
--
-- Since 1.3.7
matMultiEmailField :: Monad m
                   => RenderMessage (HandlerSite m) FormMessage
                   => Markup -> ColSize -> Field m [Text]
matMultiEmailField l col = multiEmailField {fieldView = view}
  where -- report offending address along with error
        validate a = case Email.validate $ encodeUtf8 a of
                        Left e -> Left $ T.concat [a, " (",  pack e, ")"]
                        Right r -> Right $ emailToText r
        cat = intercalate ", "
        emailToText = decodeUtf8With lenientDecode . Email.toByteString
        view theId name attrs val isReq = toWidget [hamlet|
$newline never
<div class="input-field #{colClass col}">
  <input id="#{theId}" name="#{name}" *{attrs} type="email" multiple :isReq:required="" value="#{either id cat val}">
  <label for="#{theId}">#{l}
|]

-- | Creates an input with @type="url"@, validating the URL according to RFC3986.
matUrlField :: Monad m
            => RenderMessage (HandlerSite m) FormMessage
            => Markup -> ColSize -> Field m Text
matUrlField l col = urlField { fieldView = view }
  where view theId name attrs val isReq = toWidget [hamlet|
<div class="input-field #{colClass col}">
  <input ##{theId} name=#{name} *{attrs} type=url :isReq:required value=#{either id id val}>
  <label for="#{theId}">#{l}
|]




-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectFieldList [("Value 1" :: Text, "value1"),("Value 2", "value2")]) "Which value?" Nothing
matSelectFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                   => [(msg, a)] -> Markup -> ColSize -> Field (HandlerT site IO) a
matSelectFieldList opts = matSelectField (optionsPairs opts)

-- | Creates a @\<select>@ tag for selecting one option. Example usage:
--
-- > areq (selectField $ optionsPairs [(MsgValue1, "value1"),(MsgValue2, "value2")]) "Which value?" Nothing
matSelectField :: (Eq a, RenderMessage site FormMessage)
               => HandlerT site IO (OptionList a) -> Markup -> ColSize -> Field (HandlerT site IO) a
matSelectField handler l col = selectFieldHelper
    (\theId name attrs inside -> [whamlet|
$newline never
<div class="#{colClass col} input-field-margin">
  <label for="#{theId}">#{l}
  <select.browser-default ##{theId} name=#{name} *{attrs}>^{inside}
|]) -- outside
    (\_theId _name isSel -> [whamlet|
$newline never
<option value=none :isSel:selected>_{MsgSelectNone}
|]) -- onOpt
    (\_theId _name _attrs value isSel text -> [whamlet|
$newline never
<option value=#{value} :isSel:selected>#{text}
|]) -- inside
    handler

-- | Creates a @\<select>@ tag for selecting multiple options.
matMultiSelectFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                        => [(msg, a)] -> Markup -> ColSize -> Field (HandlerT site IO) [a]
matMultiSelectFieldList opts = matMultiSelectField (optionsPairs opts)

-- | Creates a @\<select>@ tag for selecting multiple options.
matMultiSelectField :: (Eq a, RenderMessage site FormMessage)
                    => HandlerT site IO (OptionList a) -> Markup -> ColSize -> Field (HandlerT site IO) [a]
matMultiSelectField ioptlist l col = (multiSelectField ioptlist) { fieldView = view }
  where view theId name attrs val isReq =
            do opts <- fmap olOptions $ handlerToWidget ioptlist
               let selOpts = map (id &&& (optselected val)) opts
               [whamlet|
                 <div class="#{colClass col} input-field-margin">
                   <label for="#{theId}">#{l}
                   <select.browser-default ##{theId} name=#{name} :isReq:required multiple *{attrs}>
                       $forall (opt, optsel) <- selOpts
                           <option value=#{optionExternalValue opt} :optsel:selected>#{optionDisplay opt}
                       |]
        optselected (Left _) _ = False
        optselected (Right vals) opt = (optionInternalValue opt) `elem` vals

-- | Creates an input with @type="radio"@ for selecting one option.
matRadioFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                  => [(msg, a)] -> Markup -> Field (HandlerT site IO) a
matRadioFieldList opts l = matRadioField (optionsPairs opts)

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
matCheckboxesFieldList :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                       => [(msg, a)] -> Field (HandlerT site IO) [a]
matCheckboxesFieldList = matCheckboxesField . optionsPairs

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
matCheckboxesField :: (Eq a, RenderMessage site FormMessage)
                   => HandlerT site IO (OptionList a) -> Field (HandlerT site IO) [a]
matCheckboxesField ioptlist =
    (multiSelectField ioptlist) { fieldView = view }
  where view theId name attrs val isReq =
            do opts <- fmap olOptions $ handlerToWidget ioptlist
               let optselected (Left _) _ = False
                   optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
               [whamlet|
$forall opt <- opts
  <p>
    <input id="#{theId}" type=checkbox name=#{name} value=#{optionExternalValue opt} *{attrs} :optselected val opt:checked>
    <label for="#{theId}">#{optionDisplay opt}
|]
        
-- | Creates an input with @type="radio"@ for selecting one option.
matRadioField :: (Eq a, RenderMessage site FormMessage)
               => HandlerT site IO (OptionList a) -> Field (HandlerT site IO) a
matRadioField = selectFieldHelper
    (\theId _name _attrs inside -> [whamlet|
$newline never
<div ##{theId}>^{inside}
|])
    (\theId name isSel -> [whamlet|
$newline never
<p>
  <input.with-gap id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
  <label.radio for="#{theId}-none">_{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
$newline never
<p>
  <input.with-gap id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
  <label.radio for=#{theId}-#{value}>\#{text}
|])

-- | Creates a group of radio buttons to answer the question given in the message. Radio buttons are used to allow differentiating between an empty response (@Nothing@) and a no response (@Just False@). Consider using the simpler 'checkBoxField' if you don't need to make this distinction.
--
-- If this field is optional, the first radio button is labeled "\<None>", the second \"Yes" and the third \"No".
--
-- If this field is required, the first radio button is labeled \"Yes" and the second \"No". 
--
-- (Exact label titles will depend on localization).
matBoolField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Bool
matBoolField = boolField { fieldView = view }
  where showVal = either (const False)
        view theId name attrs val isReq = [whamlet|
$newline never
$if isReq
  <div.switch>
    <label>
      Nein
      <input type="checkbox" *{attrs} value=yes name=#{name} :showVal id val:checked>
      <span.lever />
      Ja
$else
  <p> 
    <input id=#{theId}-none *{attrs} type=radio name=#{name} value=none checked>
    <label for=#{theId}-none>_{MsgSelectNone}
  <p>
    <input id=#{theId}-yes *{attrs} type=radio name=#{name} value=yes :showVal id val:checked>
    <label for=#{theId}-yes>_{MsgBoolYes}
  <p>
    <input id=#{theId}-no *{attrs} type=radio name=#{name} value=no :showVal not val:checked>
    <label for=#{theId}-no>_{MsgBoolNo}
|]

-- | Creates an input with @type="checkbox"@. 
--   While the default @'boolField'@ implements a radio button so you
--   can differentiate between an empty response (@Nothing@) and a no
--   response (@Just False@), this simpler checkbox field returns an empty
--   response as @Just False@.
--
--   Note that this makes the field always optional.
--
matCheckBoxField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Markup -> Field m Bool
matCheckBoxField l = checkBoxField {fieldView = view}
  where showVal = either (const False)
        view theId name attrs val isReq = [whamlet|
$newline never
<p>
  <input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
  <label for=#{theId}>#{l}
|]


-- | Creates an input with @type="file"@.
matFileField :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
             => Markup -> ColSize -> Field m FileInfo
matFileField l col = fileField { fieldView = view }
  where view theId name attrs val isReq = [whamlet|

<div class="file-field input-field #{colClass col}">
  <input class="file-path validate" type="text" />
  <div class="btn">
    <span>#{l}
    <input id=#{theId} name=#{name} *{attrs} type=file :isReq:required>
|]


selectFieldHelper
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetT site IO () -> WidgetT site IO ())
        -> (Text -> Text -> Bool -> WidgetT site IO ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetT site IO ())
        -> HandlerT site IO (OptionList a)
        -> Field (HandlerT site IO) a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ render opts val `notElem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
                (optionDisplay opt)
    , fieldEnctype = UrlEncoded
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y
