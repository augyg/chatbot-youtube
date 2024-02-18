

module EventListener where




import Reflex.Dom.Core
import Language.Javascript.JSaddle

import Control.Monad.IO.Class (liftIO)
import Control.Lens 
import Control.Monad (void)
import Data.Text as T

import qualified GHCJS.DOM.Types as GHCJS

--jsObj ^. js ("x" ::

type Trigger a = (a -> IO ())
type HTMLRef = JSVal
type HTMLEventType = T.Text
type HTMLEvent = JSVal

--jsf "someFunc" (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)

addHTMLEventListener''
  :: ( TriggerEvent t m
     , MonadJSM m
     , DomBuilder t m
     )
  => Element er GhcjsDomSpace t
  -> HTMLEventType
  -> (HTMLEvent -> Trigger a -> JSM ())
  -> m (Event t a) 
addHTMLEventListener'' elementReflex htmlEventType actn = do
  (event, trigger) <- newTriggerEvent
  let raw = GHCJS.unElement . _element_raw $ elementReflex

  -- jsObj.addEventListener(htmlEventType, () => ()) 
  liftJSM $ raw ^. js2 "addEventListener" htmlEventType
    ( fun $ \_ _ [e] -> do 
        actn e trigger
        pure () 
    ) 

  pure event 

