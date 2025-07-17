module Main exposing (..)

import GraphicSVG exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp, onResize, onAnimationFrame)
import Browser.Navigation exposing (Key)
import List exposing (range)
import String
import Char
import Array
import Json.Decode as D
import Task
import Time exposing (..)
import Html
import Svg
import Svg.Attributes as SA
import SawToothBugCrawl

-- use these variables for your collage size
collageWidth = 192
collageHeight = 240
appTitle = "My App"
-- depending on the state you can turn on and off typing
allowTyping model = model.state /= NotTyping 
-- depending on state you can turn on and off animation (the Tick message)
isAnimating model = False 

-- CONSTANTS

intervalWidth : Float
intervalWidth = 10 

lightBlue : Color
lightBlue = rgb 51 210 255

blue : Color
blue = rgb 0 159 204

lightPink : Color 
lightPink = rgb 245 171 229

-- FUNCTIONS 

sawtooth : Float -> Float
sawtooth t = (t / (2 * pi)) - toFloat (floor (t / (2 * pi))) 

triangleWave : Float -> Float
triangleWave t =
  let 
    period = 4 * pi
    tt = ((t - 1)/2) - toFloat (floor ((t-1)/2))
  in
    abs (2 * sawtooth (t - 0.25 * period) - 1)

squareWave : Float -> Float
squareWave t = toFloat (modBy 2 (floor (t / pi)))

-- SINUSOIDAL TYPE 

type alias Sinusoidal =
  { amplitude : Float
  , period : Float
  , phaseShift : Float
  , verticalShift : Float
  }

waveToString : Sinusoidal -> String
waveToString wave =
  let 
    { amplitude , period , phaseShift , verticalShift } = wave
  in
    (String.fromFloat amplitude) ++ " * f (" ++ (String.fromFloat period) ++ " * model.time + pi * " ++ (String.fromFloat phaseShift) ++ ") + " ++ (String.fromFloat verticalShift)

codeString : Sinusoidal -> AnimationProperty -> String
codeString wave animationProperty =
  let
    { amplitude , period , phaseShift , verticalShift } = wave
    
    animationCode = 
         "( model.time " 
      ++ " |> (*) " ++ (String.fromFloat period) 
      ++ " |> (+) (pi * " ++ (String.fromFloat phaseShift) ++ ")"
      ++ " |> function "
      ++ " |> (*) " ++ (String.fromFloat amplitude) 
      ++ " |> (+) " ++ (String.fromFloat verticalShift) ++ ")"
  in
    case animationProperty of 
      Scale -> "|> scale " ++ animationCode
      Rotate -> "|> rotate (degrees " ++ animationCode ++ ")"
      Move Horizontal -> "|> move (" ++ animationCode ++ ", 0)"
      Move Vertical -> "|> move (0, " ++ animationCode ++ ")"

 
-- PROPERTIES
type Function =
    Sin 
  | Sawtooth
  | Triangle
  | Square 
 
type WaveProperty = 
    Amplitude
  | Period
  | PhaseShift
  | VerticalShift
  
type AnimationProperty = 
    Scale
  | Rotate
  | Move Direction
  
type Direction = 
    Vertical
  | Horizontal

-- SHAPE BORDER BOX

type alias ShapeBB =
  { w : Float
  , h : Float
  , sh : Shape Msg 
  }
  
addBB : Float -> Float -> Float -> ShapeBB -> ShapeBB
addBB r g b shape = 
  let 
    { w, h, sh } = shape 
  in
    { shape |
        sh = [ rect w h 
                 |> filled (rgba r g b 0.35)
             , rect w h
                 |> outlined (dotted 0.5) (rgba r g b 0.75)
             , sh
             ]
             |> group
    }
    
moveBB : (Float, Float) -> ShapeBB -> ShapeBB
moveBB (x, y) shape = 
  { shape | sh = shape.sh |> move (x, y) }
  
scaleBB : Float -> ShapeBB -> ShapeBB
scaleBB factor shape =
  { shape | sh = shape.sh |> scale factor }

notifyTapBB : Msg -> ShapeBB -> ShapeBB
notifyTapBB msg shape =
  { shape | sh = shape.sh |> notifyTap msg }

beside : ShapeBB -> ShapeBB -> ShapeBB
beside a b =
  { w = a.w + b.w
  , h = max a.h b.h
  , sh = [ a.sh
         , b.sh |> move (0.5 * (a.w + b.w), 0)
         ]
         |> group
         |> move (-0.5 * b.w, 0)
  }
  
stack : ShapeBB -> ShapeBB -> ShapeBB
stack a b =
  { w = max a.w b.w
  , h = a.h + b.h
  , sh = [ b.sh
         , a.sh |> move (0, 0.5 * (a.h + b.h))
         ]
         |> group
         |> move (0, -0.5 * a.h)
  }

layer : ShapeBB -> ShapeBB -> ShapeBB
layer a b =
  { w = max a.w b.w
  , h = max a.h b.h
  , sh = [ a.sh
         , b.sh
         ]
         |> group
  }

addPaddingBelowBB : Float -> ShapeBB -> ShapeBB
addPaddingBelowBB height shape  =
  stack 
    shape
    { w = shape.w
    , h = height
    , sh = group []
    }

recursiveBeside : List ShapeBB -> ShapeBB
recursiveBeside shapes =
  case shapes of
    shape :: rest -> beside shape (recursiveBeside rest)
    [] -> { w = 0
          , h = 0
          , sh = group []
          }

recursiveStack : List ShapeBB -> ShapeBB
recursiveStack shapes =
  case shapes of
    shape :: rest -> stack shape (recursiveStack rest)
    [] -> { w = 0
          , h = 0
          , sh = group []
          }

-- UI ELEMENTS 

type alias ButtonColours =
    { fill : Color
    , shadow : Color
    , border : Color
    , highlight : Color
    , pic : Shape Msg
    }

buttonColoUrsFor : String -> ButtonColours
buttonColoUrsFor name =
    case String.toLower name of
        "sawtooth" ->
            { fill = rgb 12 235 235, 
            shadow = rgb 107 125 120, 
            border = rgb 85 122 168,
            highlight = rgb 222 249 242,
            pic = sawtoothPic } 
        "sin" ->
            { fill = rgb 255 229 0, 
            shadow = rgb 167 167 112,
            border = rgb 198 200 89,
            highlight = rgb 255 243 193,
            pic = sinPic}  
        "triangle" ->
            { fill = rgb 255 51 51, 
            shadow = rgb 164 120 125,
            border = rgb 196 87 97,
            highlight = rgb 255 225 225,
            pic = trianglePic}
        "square" ->
            { fill = rgb 126 217 87, 
            shadow = rgb 110 125 107,
            border = rgb 99 168 85,
            highlight = rgb 229 255 225,
            pic = squarePic}
        _ ->
            { fill = rgb 200 200 200, 
            shadow = rgb 150 150 150,
            border = rgb 204 0 0,
            highlight = rgb 204 0 0,
            pic = sawtoothPic}

buttonGraphic fill shadow border highlight name pic = 
    let 
      wordLength = String.length name |> toFloat
    in 
      group
      [ 
        roundedRect 40 12 2
        |> filled highlight
        ,
          polygon [(30,2),(30,-9),(-8,-9)]
        |> filled shadow
        |> move (-9.8, 3.4)
        ,
        roundedRect 40 12 2
        |> outlined(solid 1.2) border
        ,
        roundedRect 35 9 1
        |> filled fill
        ,
        text (String.toUpper name)
        |> filled black
        |> scale 0.35
        |> move (-15,-1)
        , 
        pic
        |> scale 0.4
        |> move(wordLength * 1.5 , 0.4)
      ]


sawtoothPic : Shape msg
sawtoothPic =  
    let
        top = 6
        bottom = -6
        width = 6
        points =
            [ (-width, 0)         
            , (0, top)         
            , (0, bottom)       
            , (width, 0)        
            ]
    in
    openPolygon points |> outlined (solid 0.5) black

sinPic : Shape msg
sinPic =
    let
        step = 0.5
        xs = List.range -6 6
        points =
            List.map (\x -> 
                let
                    fx = toFloat x * step
                    y = 6 * sin fx
                in
                (fx *5, y)
            ) xs
    in
    openPolygon points
        |> outlined (solid 0.5) black

triangleFunc : Float -> Float
triangleFunc t =
    let
        period = 4
        offset = 1
        tt = ((t - offset) / period) - toFloat (floor ((t - offset) / period))
    in
    (abs (tt - 0.5)) * 8 - 2


trianglePic : Shape msg
trianglePic =
    let
        step = 0.5
        xs = List.range -6 6
        points =
            List.map (\x ->
                let
                    fx = toFloat x * step
                    y = 2* (triangleFunc fx)
                in
                (fx * 3, y)
            ) xs
    in
    openPolygon points
        |> outlined (solid 0.5) black
        |> scale 1.1

squareWave2 t = 
    toFloat (modBy 2 (floor t))
    
squarePic : Shape msg
squarePic =
    let
        step = 0.2
        xs = List.range -8 8
        points =
            List.map (\x ->
                let
                    fx = toFloat x * step
                    y = 10* (squareWave2 fx)
                in
                (fx * 5, y)
            ) xs
    in
    openPolygon points
        |> outlined (solid 0.5) black
        |> scale 1.2
        |> move (0, -5)

-- INTERACTIVE COMPONENTS (CLICKABLE ELEMENTS)

waveButton : (Float -> Float) -> String -> ShapeBB
waveButton function functionName =
  let
    colours = buttonColoUrsFor functionName
  in
    { w = 44
    , h = 12
    , sh = 
        [ buttonGraphic colours.fill colours.shadow colours.border colours.highlight functionName colours.pic
        ]
        |> group
    }
  |> notifyTapBB (SelectFunction function)

propertyValue : Float -> WaveProperty -> Bool -> ShapeBB
propertyValue value waveProperty isSelected =
  let
    width = 60
    height = 9
    
    panelSpace = 30 -- space between value and panel; length of triangle 
    
    valueText v = 
      text (String.fromFloat v)
        |> fixedwidth
        |> size 5
        |> underline
        |> (if isSelected then bold else identity)
        |> filled lightPink
        |> move (-2, -2)

    valueButton v =
      { w = width / 5
      , h = height
      , sh = valueText v
      }
      |> addBB 255 255 255
      |> notifyTapBB (if isSelected then DeselectProperty else SelectProperty waveProperty)
  
    buttonLabels =
      case waveProperty of
        Amplitude ->      [ ("taller", (Increment waveProperty))
                          , ("shorter", (Decrement waveProperty)) ]
        Period ->         [ ("shorter", (Increment waveProperty)) 
                          , ("longer", (Decrement waveProperty)) ]
        PhaseShift ->     [ ("left", (Increment waveProperty))
                          , ("right", (Decrement waveProperty)) ]
        VerticalShift ->  [ ("up", (Increment waveProperty)) 
                          , ("down", (Decrement waveProperty)) ]

  
    -- button to modify value
    button : String -> Msg -> ShapeBB
    button label msg =
      { w = 20
      , h = 8
      , sh = 
          [ roundedRect 18 7 2
              |> filled lightPink
          , text label
              |> size 3.5
              |> sansserif
              |> centered
              |> filled white
              |> move (0, -1.25)
          ]
          |> group
      }
      |> addPaddingBelowBB 2.5
      |> notifyTapBB msg

    -- panel containing multiple buttons
    panel : List (String, Msg) -> ShapeBB
    panel buttons =
      { w = 0 * 20
      , h = 0 * 8.5 * (toFloat (List.length buttons)) 
      , sh = (layer
                (background (List.length buttons))
                (recursiveStack (List.map (\(string, msg) -> button string msg) buttons))
             ).sh
             |> move (panelSpace, 0)
      }
      |> moveBB (0, -2.5)
      
    -- background for panel of buttons
    background : Int -> ShapeBB
    background n =
      layer
        { w = 30
        , h = 10
        , sh = 
            polygon [ (-(panelSpace + 1.5), 0), (0,  2.5), (0, -2.5) ]
              |> filled lightPink
              |> makeTransparent 0.6
        }
        { w = 20
        , h = 8.5 * (toFloat n)
        , sh = 
            [ roundedRect 20 (8.5 * (toFloat n) + 2.5 * (toFloat (n - 1))) 3
                |> filled white
            , roundedRect 20 (8.5 * (toFloat n) + 2.5 * (toFloat (n - 1))) 3
                |> outlined (solid 0.5) lightPink
            ]
            |> group
        }
      
  in
    if isSelected then
      beside
        (valueButton value)
        ((panel buttonLabels) |> moveBB (0, -5))
    else   
      valueButton value

-- VIEW
        
viewGrid : Float -> Float -> ShapeBB
viewGrid width height = 
  let
    gridX x = 
      case x of
        0 -> group []
        _ -> 
          [ openPolygon [ (-(width / 2) + (toFloat x * intervalWidth), (height / 2)),
                          (-(width / 2) + (toFloat x * intervalWidth), -(height / 2)) ]
              |> outlined 
                (if (floor (width / (2 * intervalWidth)) == x) then 
                   solid 0.5
                 else
                   solid 0.15
                ) 
                lightPink
          , gridX (x - 1)
          ]
          |> group
    
    gridY y = 
      case y of 
        0 -> group []
        _ -> 
          [ openPolygon [ (-(width / 2), -(height / 2) + (toFloat y * intervalWidth))
                        , ((width / 2), -(height / 2) + (toFloat y * intervalWidth)) ] 
              |> outlined 
                (if (floor (height / (2 * intervalWidth)) == y) then 
                   solid 0.5
                 else
                   solid 0.15
                ) 
                lightPink
          , gridY (y - 1)
          ]
          |> group
  in
    { w = width
    , h = height
    , sh =
        [ gridX (floor (width / intervalWidth) - 1)
        , gridY (floor (height / intervalWidth) - 1)
        , triangle 1.25
            |> filled lightPink
            |> rotate (degrees -30)
            |> scaleY 1.25
            |> move (0, (height / 2))
        , triangle 1.25
            |> filled lightPink
            |> rotate (degrees 30)
            |> scaleY 1.25
            |> move (0, -(height / 2))
        , triangle 1.25
            |> filled lightPink
            |> scaleX 1.25
            |> move ((width / 2), 0)
        , triangle 1.25
            |> filled lightPink
            |> scaleX 1.25
            |> rotate (degrees 180)
            |> move (-(width / 2), 0)
        ]
        |> group
    }

viewEquation : Sinusoidal -> ShapeBB
viewEquation wave =
  let 
    equation = waveToString wave
  in
    { w = 192
    , h = 12
    , sh =     
        text equation
          |> fixedwidth
          |> size 6
          |> centered
          |> filled black
    }
    |> moveBB (0, 100)
    
viewAnimation :  Sinusoidal ->
                (Float -> Float) ->
                 Float ->
                 AnimationProperty ->
                 Shape Msg -> 
                 ShapeBB
viewAnimation wave function time animationProperty shape =
  let
    width = 120
    height = 80
    
    { amplitude , period , phaseShift , verticalShift } = wave
    
    timeInInterval = 0.01 * toFloat (modBy ((floor width) * 100) (floor ((time / ((1 / intervalWidth) * period)) * 100)))

    functionValue = (verticalShift + amplitude * (function ((((1 / intervalWidth) * period) * (timeInInterval - (width / 2)) + (phaseShift * pi)))))
    
    animation =
      case animationProperty of 
        Scale -> scale (functionValue / intervalWidth )
        Rotate -> rotate (degrees functionValue)
        Move Horizontal -> move (functionValue, 0)
        Move Vertical -> move (0, functionValue)
    
    animatedShape = 
      { w = width
      , h = height
      , sh = 
          shape |> animation            
      }
  in
    layer 
      (viewGrid width height)
      (animatedShape)
    |> scaleBB 0.8
    |> moveBB (35, -65)

viewPipedFunction :  Sinusoidal ->
                    (Float -> Float) ->
                     Maybe WaveProperty ->
                     ShapeBB
viewPipedFunction wave function selectedWaveProperty =
  let
    width = 60
    height = 9
    
    { amplitude , period , phaseShift , verticalShift } = wave
    
    functionText f = 
      text ("|> " ++ f)
        |> fixedwidth
        |> size 5
        |> filled black
        |> move (-14, -2)
    
    valueText v = 
      text (String.fromFloat v)
        |> fixedwidth
        |> size 5
        |> underline
        |> filled lightPink
        |> move (-2, -2)
    
    
    
    pipeProperty f v waveProperty  = 
      beside
        ({ w = width / 2
         , h = height
         , sh = functionText f
         }
        )
        ( case selectedWaveProperty of 
            Just property -> propertyValue v waveProperty (property == waveProperty)
            Nothing -> propertyValue v waveProperty False
        )
    
    pipeFunction f =
      { w = width / 2 + width / 5
      , h = height
      , sh = functionText f 
               |> move (-6, 0)
      }
      
  in
    recursiveStack
      [ { w = width
        , h = height
        , sh = text "model.time" |> fixedwidth |> size 5 |> filled black |> move (-28, -2)
        }
      , (pipeProperty "(*)" period Period)
      , (pipeProperty "(+) pi *" phaseShift PhaseShift)
      , (pipeFunction "function") 
      , (pipeProperty "(*)" amplitude Amplitude)
      , (pipeProperty "(+)" verticalShift VerticalShift)
      ]
    |> moveBB (-55, 55)
      
viewWave :  Float -> Float ->
            Sinusoidal ->
           (Float -> Float) ->
            Float -> 
            ShapeBB
viewWave width height wave f t =
  let
    { amplitude , period , phaseShift , verticalShift } = wave
    
    plotPoint ff tt = 
      (-(width / 2) + tt, verticalShift + amplitude * (ff ((((1 / intervalWidth) * period) * (tt - (width / 2)) + (phaseShift * pi)))))
  
    timeInInterval = 0.01 * toFloat (modBy ((floor width) * 100) (floor ((t / ((1 / intervalWidth) * period)) * 100)))
    
    times = 
      List.range 0 (floor (width * (abs amplitude)))
        |> List.map (\x -> (toFloat x / (abs amplitude)))
  
    points = 
      times
        |> List.map (plotPoint f)
  in
    layer 
      (viewGrid width height)
      { w = width
      , h = height
      , sh = 
          [ openPolygon points
              |> outlined (solid 0.5) black 
          , SawToothBugCrawl.ladyBug
              |> scale 0.05
              |> rotate (degrees -70)
              |> move (plotPoint f timeInInterval)
          ]
          |> group 
      }
      |> scaleBB 0.8
      |> moveBB (35, 50)
 
viewWaveSelect : ShapeBB
viewWaveSelect =
  recursiveBeside 
    [ (waveButton sin "sin")
    , (waveButton sawtooth "sawtooth")
    , (waveButton triangleWave "triangle")
    , (waveButton squareWave "square")
    ]
    
viewAnimationSelect : AnimationProperty -> ShapeBB
viewAnimationSelect animationProperty =
  let
    animationPropertyLabel =
      { w = 55
      , h = 8
      , sh = 
          text (String.toLower (Debug.toString animationProperty))
            |> fixedwidth
            |> size 6
            |> centered
            |> filled black
      }
      |> moveBB (0, -1.5)
    
    triangleButton : Bool -> Msg -> ShapeBB
    triangleButton direction msg =
      { w = 8
      , h = 8
      , sh = 
          triangle 3
            |> filled lightPink
            |> (if direction then identity else rotate (degrees 180))
      }
      |> notifyTapBB msg
  in
    beside
      (beside
         (triangleButton False PreviousAnimationProperty)
         animationPropertyLabel
      )
      (triangleButton True NextAnimationProperty)
      
    |> moveBB (35, -20)

myShapes model =
  [
    rect 192 240
    |> outlined (dashed 1) lightPink
    |> makeTransparent 1
    
    , (viewWave 120 80 model.wave model.function model.time).sh

    , (viewEquation model.wave).sh

    , (viewPipedFunction model.wave model.function model.selectedWaveProperty).sh
    
    , (viewAnimation model.wave model.function model.time model.animationProperty model.shape).sh
    
    , (viewWaveSelect).sh
    
    , (viewAnimationSelect model.animationProperty).sh

  ]
textBox width height isHighlighted chars =
  [ rect width height |> filled white
  , text (String.join "" <| List.reverse chars ) |> centered |> size 4 |> filled black 
      |> clip (rect width height |> ghost)
  , rect width height |> outlined (solid 1) (if isHighlighted then rgb 0 0 255 else charcoal)
  ] |> group
type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp
         | SelectFunction (Float -> Float)
         | SelectProperty WaveProperty
         | DeselectProperty
         | Increment WaveProperty 
         | Decrement WaveProperty
         | NextAnimationProperty
         | PreviousAnimationProperty 



type State = NotTyping 
    -- | TODO
type alias Model =  { state : State   -- TODO
                    , window : Window -- do not change (used for resizing window)
                    , time : Float    -- not recommended that you change this
                    , function : (Float -> Float) 
                   , wave : Sinusoidal 
                   , shape : Shape Msg
                   , selectedWaveProperty : Maybe WaveProperty
                   , animationProperty : AnimationProperty
                    }
init =  { state = NotTyping -- TODO
        -- do not change these
        , time = 0
        , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
        ,  function = sin
       , wave = 
           { amplitude = 10
           , period = 1
           , phaseShift = 0
           , verticalShift = 0
           } 
        , shape = square 10 |> outlined (solid 0.75) red
        , selectedWaveProperty = Nothing
        , animationProperty = Scale
        }
update msg model = 
  case msg of
    -- TODO handle new messages here
    Tick t-> ({ model | time = t }, Cmd.none)
                     
    SelectFunction selectedFunction ->
      ({ model | function = selectedFunction }, Cmd.none)
    
    SelectProperty waveProperty ->
      ({ model | selectedWaveProperty = Just waveProperty }, Cmd.none)
    
    DeselectProperty ->
      ({ model | selectedWaveProperty = Nothing }, Cmd.none)

    Increment property ->
      let
        { amplitude, period, phaseShift, verticalShift } = model.wave

        newWave = 
          case property of 
            Amplitude -> { amplitude = amplitude + 1, period = period, phaseShift = phaseShift, verticalShift = verticalShift }
              
            Period -> { amplitude = amplitude, period = period + 0.25, phaseShift = phaseShift, verticalShift = verticalShift }

            PhaseShift -> { amplitude = amplitude, period = period, phaseShift = phaseShift + 0.25, verticalShift = verticalShift }

            VerticalShift -> { amplitude = amplitude, period = period, phaseShift = phaseShift, verticalShift = verticalShift + 1 }
      in
        ({ model | wave = newWave }, Cmd.none)
        
    Decrement property ->
      let
        { amplitude, period, phaseShift, verticalShift } = model.wave

        newWave = 
          case property of 
            Amplitude -> { amplitude = amplitude - 1, period = period, phaseShift = phaseShift, verticalShift = verticalShift }
              
            Period -> { amplitude = amplitude, period = period - 0.25, phaseShift = phaseShift, verticalShift = verticalShift }

            PhaseShift -> { amplitude = amplitude, period = period, phaseShift = phaseShift - 0.25, verticalShift = verticalShift }

            VerticalShift -> { amplitude = amplitude, period = period, phaseShift = phaseShift, verticalShift = verticalShift - 1 }
      in 
        ({ model | wave = newWave }, Cmd.none)

    NextAnimationProperty ->
      ({ model | 
          animationProperty = case model.animationProperty of
                                Scale -> Rotate
                                Rotate -> Move Vertical
                                Move Vertical -> Move Horizontal
                                Move Horizontal -> Scale
                                }, Cmd.none)
      
    PreviousAnimationProperty ->
      ({ model | 
          animationProperty = case model.animationProperty of
                                Scale -> Move Horizontal
                                Rotate -> Scale
                                Move Vertical -> Rotate
                                Move Horizontal -> Move Vertical
                                }, Cmd.none)

    -- get keyboard input
    KeyUp _ -> (model,Cmd.none)
    KeyDown code -> ( case model.state of
                         NotTyping -> model
                         -- TODO handle keyboard input here
                         -- ExampleState -> { model | chars1 = typeAndDelete model.chars1 code }
                    ,Cmd.none)
    -- don't change these unless you really need to
    WindowResize mWH ->
      case mWH of
        Just ( w, h ) ->
          ( { model | window = didResize model.window w h
              }
          , Cmd.none
          )
        -- need to get viewport size after the app starts
        Nothing ->
          ( model
          , getViewportSize
          )
    ReturnPosition message ( x, y ) ->
        let
            ( newModel, userCmds ) =
                update
                    (message (convertCoords model.window ( x, y ) ))
                    model
        in
        ( newModel, userCmds )
    NoOp -> ( model, Cmd.none )
typeAndDelete soFar code =
    if String.length code == 1 then 
        code :: soFar 
    else if code == "Backspace" then
        List.drop 1 soFar
    else soFar

main : Program () Model Msg
main =
    Browser.document
        { init =
            \ _ -> -- we do not support flags
                ( init , getViewportSize )
        , update = update
        , view = \ model -> { body = [createCollage collageWidth collageHeight <| myShapes model], title = appTitle }
        , subscriptions = subscriptions
        }
subscriptions model =
    Sub.batch
        ( let 
            at = allowTyping model 
            an = isAnimating model
          in
            if at && an then 
                [ onKeyUp (D.map KeyUp (D.field "key" D.string))
                , onKeyDown (D.map KeyDown (D.field "key" D.string))
                , onResize (\_ _ -> WindowResize Nothing) 
                , onAnimationFrame ( \ posix -> toFloat (Time.posixToMillis posix) * 0.001 |> Tick )
                ]
            else if at then
                [ onKeyUp (D.map KeyUp (D.field "key" D.string))
                , onKeyDown (D.map KeyDown (D.field "key" D.string))
                , onResize (\_ _ -> WindowResize Nothing) 
                ]
            else if an then 
                [ onResize (\_ _ -> WindowResize Nothing) 
                , onAnimationFrame ( \ posix -> toFloat (Time.posixToMillis posix) * 0.001 |> Tick )
                ]
            else
              [ onResize (\_ _ -> WindowResize Nothing) ]
        )
getViewportSize : Cmd Msg
getViewportSize = Task.attempt
            (\rvp -> case rvp of
                        Ok vp ->
                            WindowResize
                                <| Just ( vp.viewport.width, vp.viewport.height )
                        Err _ -> NoOp
            )
            (getViewportOf "render")
type alias Window =
    { cw : Float
    , ch : Float
    , sw : Float
    , sh : Float
    }
didResize window sw sh = { window | sw = sw, sh = sh }
convertCoords : Window -> ( Float, Float ) -> ( Float, Float ) -- NOTE:  reversed args
convertCoords gModel ( x, y ) =
    let
        sw =
            gModel.sw
        sh =
            gModel.sh
        cw =
            gModel.cw
        ch =
            gModel.ch
        aspectout =
            if not (sh == 0) then
                sw / sh
            else
                4 / 3
        aspectin =
            if not (ch == 0) then
                cw / ch
            else
                4 / 3
        scaledInX =
            aspectout < aspectin
        scaledInY =
            aspectout > aspectin
        cscale =
            if scaledInX then
                sw / cw
            else if scaledInY then
                sh / ch
            else
                1
    in
    ( (x - sw / 2) / cscale
    , (y + sh / 2) / cscale
    )
createCollage : Float -> Float -> List (Shape Msg) -> Html.Html Msg
createCollage w h shapes =
    Svg.svg
        [ SA.width "100%"
        , SA.height "100%"
        , SA.style "position:absolute;top:0px;left:0px;"
        , SA.viewBox
            (String.fromFloat (-w / 2)
                ++ " "
                ++ String.fromFloat (-h / 2)
                ++ " "
                ++ String.fromFloat w
                ++ " "
                ++ String.fromFloat h
            )
        , SA.id "render"
        ]
        (cPath w h
            :: [ Svg.g
                    [ SA.clipPath "url(#cPath)" ]
                    (List.indexedMap
                        (\n -> createSVG (String.fromInt n) w h ident identity ReturnPosition)
                        shapes
                    )
               ]
        )
cPath : Float -> Float -> Svg.Svg Msg
cPath w h =
    Svg.defs []
        [ Svg.clipPath
            [ SA.id "cPath" ]
            [ Svg.rect
                [ SA.width (String.fromFloat w)
                , SA.height (String.fromFloat h)
                , SA.x (String.fromFloat (-w / 2))
                , SA.y (String.fromFloat (-h / 2))
                ]
                []
            ]
        ]

-- repeat an animation for a given duration
repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
  speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition
repeatDistance : Float -> Float -> Float -> Float -> Float
repeatDistance speed distance startPosition time =
  repeatDuration speed (round <| distance / speed) startPosition time
animationPieces : List (Float, Float -> anytype) -> (Float -> anytype) -> Float -> anytype
animationPieces intervals finalAnimation time =
  case intervals of
    (duration, animation) :: rest ->
        if time <= duration then
          animation time
        else
          animationPieces rest finalAnimation (time - duration)
    [] ->
        finalAnimation time
plotGraph : (Float -> Float) -> Float -> Shape a
plotGraph f time =
  group
    [ openPolygon (List.map (\ t -> (-96+(toFloat t)/2.5 - 200 * toFloat (floor (time / 10)),f (toFloat t / 50))) <| List.range (500 * floor (time / 10)) (500 * ceiling (time / 10))) |> outlined (solid 1) (rgb 0 0 200)
    , group [
              circle 3 |> filled red
            , text ("(" ++ String.fromFloat time ++ ", " ++ String.fromFloat (toFloat (round <| (f time) * 100) / 100)  ++ ")")
                |> size 6
                |> filled black
                |> move (5, 5)
            ]
        |> move (-96+20* time - 200 * toFloat (floor (time / 10)),f (time))
    ]
 