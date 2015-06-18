{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Materialize
       (renderMaterialize
       ) where

import Yesod.Core
import Yesod.Form.Types
import Yesod.Form.Functions


renderMaterialize :: Monad m => FormRender m a
renderMaterialize aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
        widget = [whamlet|
            $newline never
            #{fragment}
            $if any (has . fvErrors) views
              <div.row>
                <div.col.s12>
                  <span.alert.alert-error>Einige Felder wurden nicht korrekt ausgefüllt.
                  <ul.form-errors>
                    $forall view <- views
                      $maybe err <- fvErrors view
                        <li>#{fvLabel view}: #{err}
            <div.flex-row>
              $forall view <- views
                ^{fvInput view}
                |]
    return (res, widget)
