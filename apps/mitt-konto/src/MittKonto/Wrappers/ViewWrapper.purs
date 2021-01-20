module MittKonto.Wrappers.ViewWrapper where

import Prelude

import Data.Foldable (foldMap)
import Effect (Effect)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Grid as Grid
import MittKonto.Wrappers.Elements
import React.Basic.Classic (JSX, element, make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Router as Router
import Record as Record

-- TODO: the `closeType` prop is there because it would be good to control the way we close the view,
-- as now separate views have separate implementations, but they could be unified
-- through this component, which would need expansion.
-- Something to think about.
type Props p =
  { content :: p
  , wrapperType :: WrapperType
  , closeType :: CloseType
  }

type BaseViewWrapperState =
  ( closeable :: Boolean
  , titleText :: String
  , renderedContent :: JSX
  , onCancel :: Effect Unit
  )

type ViewWrapperStateBasic = Record BaseViewWrapperState

type ViewWrapperStateAsync =
  { asyncWrapperState :: AsyncWrapper.Progress JSX
  , onTryAgain :: Effect Unit
  | BaseViewWrapperState
  }

type SetStateBasic = (ViewWrapperStateBasic -> ViewWrapperStateBasic) -> Effect Unit
type SetStateAsync = (ViewWrapperStateAsync -> ViewWrapperStateAsync) -> Effect Unit

data SetViewWrapperState = SetViewWrapperStateBasic SetStateBasic | SetViewWrapperStateAsync SetStateAsync

class ViewWrapperContent p where
  instantiate :: p -> SetViewWrapperState -> Effect JSX

component :: forall p. React.Component (Props p)
component = React.createComponent "ViewWrapper"

viewWrapper :: forall p. (ViewWrapperContent p) => (Props p) -> JSX
viewWrapper props@{ wrapperType } = case wrapperType of
  Basic -> viewWrapperBasic props
  Async -> viewWrapperAsync props
  where
    viewWrapperBasic :: (Props p) -> JSX
    viewWrapperBasic = make component
      { initialState: initialStateBasic
      , didMount: didMountBasic
      , render: renderBasic
      }

    initialStateBasic =
      { closeable: true
      , titleText: mempty
      , renderedContent: mempty
      , onCancel: pure unit
      }

    didMountBasic :: React.Self (Props p) ViewWrapperStateBasic -> Effect Unit
    didMountBasic self@{ props: { content }, setState } = do
      renderedContent <- instantiate content $ SetViewWrapperStateBasic setState
      setState \s -> s { renderedContent = renderedContent }

    viewWrapperAsync :: (Props p) -> JSX
    viewWrapperAsync = make component
      { initialState: initialStateAsync
      , didMount: didMountAsync
      , render: renderAsync
      }

    initialStateAsync = Record.merge initialStateBasic
      { asyncWrapperState: AsyncWrapper.Ready
      , onTryAgain: pure unit
      }

    didMountAsync :: React.Self (Props p) ViewWrapperStateAsync -> Effect Unit
    didMountAsync self@{ props: { content }, setState } = do
      renderedContent <- instantiate content $ SetViewWrapperStateAsync setState
      setState \s -> s { renderedContent = renderedContent }


renderBasic :: forall p. (ViewWrapperContent p) => React.Self (Props p) ViewWrapperStateBasic -> JSX
renderBasic self@{ state: { closeable, titleText, onCancel, renderedContent }, setState } =
  render self.state renderedContent

renderAsync :: forall p. (ViewWrapperContent p) => React.Self (Props p) ViewWrapperStateAsync -> JSX
renderAsync self@{ props: { content, closeType, wrapperType }, state: { closeable, titleText, onCancel, onTryAgain, renderedContent }, setState } =
  render
    { closeable
    , titleText
    , onCancel
    , renderedContent
    }
    asyncWrapper
  where
    asyncWrapper :: JSX
    asyncWrapper = AsyncWrapper.asyncWrapper
      { wrapperState: self.state.asyncWrapperState
      , readyView: renderedContent
      , editingView: identity
      , successView: \msg -> close closeType <> successWrapper msg
      , errorView: \err -> errorWrapper onTryAgain err
      , loadingView: identity
      }

render :: ViewWrapperStateBasic -> JSX -> JSX
render state@{ closeable, titleText, onCancel, renderedContent } content =
  DOM.div_
  [ header
  , content ]
  where
    header :: JSX
    header = Grid.row_
      [ DOM.div
          { className: "col col-11"
          , children: [ title ]
          }
      , if closeable then
          DOM.div
            { className: "col-1 flex credit-card-choice--close-icon"
            , children: [ Router.link
                            { to: { pathname: "/", state: {} }
                            , children: [ ]
                            , className: "close-icon"
                            }
                        ]
            , onClick: handler_ onCancel
            }
        else
          mempty
      ]

    title :: JSX
    title = DOM.h3_ [ DOM.text titleText ]

close :: CloseType -> JSX
close Countdown = Router.delayedRedirect
  { to: { pathname: "/"
        , state: {}
        }
  , from: "/kortt/uppdatera"
  , push: true
  , delay: 2000.0
  }
close _ = mempty