module Main

%inline
jscall : (fname : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

setTimeout : (() -> JS_IO ()) -> (millis : Int) -> JS_IO ()
setTimeout f millis = do
  jscall "setTimeout(%0, %1)" (JsFn (() -> JS_IO ()) -> Int -> JS_IO ())
         (MkJsFn f) millis

-- addEventListener : String -> (Ptr -> JS_IO ()) -> JS_IO ()
-- addEventListener type listener = do
--   jscall "document.addEventListener(%0, %1)" (String -> (JsFn (Ptr -> JS_IO ())) -> JS_IO ())
--          type listener

requestAnimationFrame : (Double -> JS_IO ()) -> JS_IO ()
requestAnimationFrame f = do
  jscall "window.requestAnimationFrame(%0)" (JsFn (Double -> JS_IO ()) -> JS_IO ())
         (MkJsFn f)

performance_now : JS_IO Double
performance_now = do
  jscall "performance.now()" (JS_IO Double)

log : String -> JS_IO ()
log msg = do
  jscall "console.log(%0)" (String -> JS_IO ()) msg

record HTMLCanvasElement where
  constructor MkHTMLCanvasElement
  ptr : Ptr

record CanvasRenderingContext2D where
  constructor MkCanvasRenderingContext2D
  ptr : Ptr

-- HTMLCanvasElement API

getCanvas : JS_IO HTMLCanvasElement
getCanvas = do
  ptr <- jscall "document.getElementById(\"canvas\")" (JS_IO Ptr)
  pure $ MkHTMLCanvasElement ptr

getContext2D : HTMLCanvasElement -> JS_IO CanvasRenderingContext2D
getContext2D canvas = do
  ptr <- jscall "%0.getContext(\"2d\")" (Ptr -> JS_IO Ptr)
                (ptr canvas)
  pure $ MkCanvasRenderingContext2D ptr

width : HTMLCanvasElement -> JS_IO Int
width canvas = do
  width <- jscall "%0.width" (Ptr -> JS_IO Int) (ptr canvas)
  pure width

set_width : HTMLCanvasElement -> Int -> JS_IO ()
set_width canvas width = do
  jscall "%0.width = %1" (Ptr -> Int -> JS_IO ()) (ptr canvas) width

height : HTMLCanvasElement -> JS_IO Int
height canvas = do
  height <- jscall "%0.height" (Ptr -> JS_IO Int) (ptr canvas)
  pure height

-- CanvasRenderingContext2D interface
-- TODO: Find a more concise, elegant way to declare all these functions

canvas : CanvasRenderingContext2D -> JS_IO HTMLCanvasElement
canvas ctx = do
  ptr <- jscall "%0.canvas" (Ptr -> JS_IO Ptr) (ptr ctx)
  pure $ MkHTMLCanvasElement ptr

set_fillStyle : CanvasRenderingContext2D -> String -> JS_IO ()
set_fillStyle ctx fillStyle = do
  jscall "%0.fillStyle = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) fillStyle

set_font : CanvasRenderingContext2D -> String -> JS_IO ()
set_font ctx font = do
  jscall "%0.font = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) font

set_lineCap : CanvasRenderingContext2D -> String -> JS_IO ()
set_lineCap ctx lineCap = do
  jscall "%0.lineCap = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) lineCap

set_lineJoin : CanvasRenderingContext2D -> String -> JS_IO ()
set_lineJoin ctx lineJoin = do
  jscall "%0.lineJoin = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) lineJoin

set_lineWidth : CanvasRenderingContext2D -> Double -> JS_IO ()
set_lineWidth ctx lineWidth = do
  jscall "%0.lineWidth = %1" (Ptr -> Double -> JS_IO()) (ptr ctx) lineWidth

set_strokeStyle : CanvasRenderingContext2D -> String -> JS_IO ()
set_strokeStyle ctx strokeStyle = do
  jscall "%0.strokeStyle = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) strokeStyle

set_textAlign : CanvasRenderingContext2D -> String -> JS_IO ()
set_textAlign ctx textAlign = do
  jscall "%0.textAlign = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) textAlign

set_textBaseline : CanvasRenderingContext2D -> String -> JS_IO ()
set_textBaseline ctx textBaseline = do
  jscall "%0.textBaseline = %1" (Ptr -> String -> JS_IO ()) (ptr ctx) textBaseline

beginPath : CanvasRenderingContext2D -> JS_IO ()
beginPath ctx = do
  jscall "%0.beginPath()" (Ptr -> JS_IO ()) (ptr ctx)

fillRect : CanvasRenderingContext2D -> Double -> Double -> Double -> Double -> JS_IO ()
fillRect ctx x y width height = do
  jscall "%0.fillRect(%1, %2, %3, %4)" (Ptr -> Double -> Double -> Double -> Double -> JS_IO ())
         (ptr ctx) x y width height

fillText : CanvasRenderingContext2D -> String -> Double -> Double -> JS_IO ()
fillText ctx text x y = do
  jscall "%0.fillText(%1, %2, %3)" (Ptr -> String -> Double -> Double -> JS_IO ())
         (ptr ctx) text x y

lineTo : CanvasRenderingContext2D -> Double -> Double -> JS_IO ()
lineTo ctx x y = do
  jscall "%0.lineTo(%1, %2)" (Ptr -> Double -> Double -> JS_IO ()) (ptr ctx) x y

moveTo : CanvasRenderingContext2D -> Double -> Double -> JS_IO ()
moveTo ctx x y = do
  jscall "%0.moveTo(%1, %2)" (Ptr -> Double -> Double -> JS_IO ()) (ptr ctx) x y

stroke : CanvasRenderingContext2D -> JS_IO ()
stroke ctx = do
  jscall "%0.stroke()" (Ptr -> JS_IO ()) (ptr ctx)

record Vector2d where
  constructor MkVector2d
  x : Double
  y : Double

(+) : Vector2d -> Vector2d -> Vector2d
(+) a b = MkVector2d ((x a) + (x b)) ((y a) + (y b))

(*) : Double -> Vector2d -> Vector2d
(*) c v = MkVector2d (c * (x v)) (c * (y v))

record App where
  constructor MkApp
  canvas : HTMLCanvasElement
  ctx : CanvasRenderingContext2D
  tickCount : Int
  frameCount : Int
  lastTickTime : Double
  bodyLength : Nat
  bodyPoints : List Vector2d
  headPosition : Vector2d
  headAngle : Double

clear : CanvasRenderingContext2D -> JS_IO ()
clear ctx = do
  canvas <- canvas ctx

  -- clear canvas and reset all drawing state
  canvasWidth <- width canvas
  set_width canvas canvasWidth

  set_fillStyle ctx "white"
  canvasWidth <- width canvas
  canvasHeight <- height canvas
  fillRect ctx 0.0 0.0 (cast canvasWidth) (cast canvasHeight)

  set_fillStyle ctx "#000000"

  pure ()

-- tick constants

ticksPerSecond : Double
ticksPerSecond = 120.0
tickPeriodMillis : Double
tickPeriodMillis = 1000.0 / ticksPerSecond
tickLimit : Int
tickLimit = 15

-- initial constants

startBodyLength : Nat
startBodyLength = 100

-- movement constants

moveSpeed : Double
moveSpeed = 100.0
rotateSpeed : Double
rotateSpeed = 360.0

-- drawing constants

headFillStyle : String
headFillStyle = "LimeGreen"
headLength : Double
headLength = 20.0
headWidth : Double
headWidth = 20.0
eyeFillStyle : String
eyeFillStyle = "Teal"
eyeLength : Double
eyeLength = 6.0
eyeWidth : Double
eyeWidth = 3.0
bodyStrokeStyle : String
bodyStrokeStyle = "LimeGreen"
bodyWidth : Double
bodyWidth = 8.0
tongueStrokeStyle : String
tongueStrokeStyle = "Red"
maxTonguePoints : Int
maxTonguePoints = 32
tongueLength : Double
tongueLength = 24.0
tongueSideDisplacement : Double
tongueSideDisplacement = 2.0

pointAtAngle : Double -> Vector2d
pointAtAngle angle = MkVector2d (cos angle) (sin angle)

tick : App -> Double -> App
tick app delta = do
  let app : App = record { tickCount $= (+ 1) } app

  -- add body point
  let bodyPoints : List Vector2d = (bodyPoints app)
  let bodyPoints : List Vector2d = (headPosition app) :: bodyPoints
  let bodyPoints : List Vector2d =
    if (length bodyPoints) > (bodyLength app) then
      init bodyPoints
    else
      bodyPoints
  let app : App = record { bodyPoints = bodyPoints } app

  -- move head forward
  let headAngleRads = (headAngle app) * pi / 180.0
  let offset : Vector2d = moveSpeed * delta / 1000.0 * (pointAtAngle headAngleRads)
  let app : App = record { headPosition $= (+ offset) } app

  app

update : App -> Double -> App
update app now = do
  let app : App = record { frameCount $= (+ 1) } app
  tickLoop app 0
where
  tickLoop : App -> Int -> App
  tickLoop app n = do
    if n >= tickLimit then
      record { lastTickTime = now } app
    else if (now - (lastTickTime app)) < tickPeriodMillis then
      app
    else do
      let app : App = tick app tickPeriodMillis
      let app : App = record { lastTickTime $= (+ tickPeriodMillis) } app
      tickLoop app (n + 1)

draw : App -> JS_IO ()
draw app = do
  let ctx = ctx app

  clear ctx

  -- TODO: DPI scaling

  let message = "ticks: " ++ (show $ tickCount app) ++ "; frames: " ++ (show $ frameCount app)
  set_fillStyle ctx "black"
  set_textAlign ctx "left"
  set_textBaseline ctx "hanging"
  set_font ctx "16px sans-serif"
  fillText ctx message 0.0 0.0

  -- draw body

  set_strokeStyle ctx bodyStrokeStyle
  set_lineWidth ctx bodyWidth
  set_lineCap ctx "round"
  set_lineJoin ctx "round"
  beginPath ctx
  moveTo ctx (x (headPosition app)) (y (headPosition app))
  for (bodyPoints app) (\pt => lineTo ctx (x pt) (y pt))
  stroke ctx

  -- draw head
  -- TODO: implement

  let headAngleRads : Double = (headAngle app) * pi / 180.0
  let angleVector : Vector2d = pointAtAngle headAngleRads
  let angleVectorNormal : Vector2d = MkVector2d (-(y angleVector)) (x angleVector)

  pure ()

onFrame : App -> Double -> JS_IO ()
onFrame app now = do
  let app = update app now
  draw app
  requestAnimationFrame (onFrame app)

main : JS_IO ()
main = do
  canvas <- getCanvas
  ctx <- getContext2D canvas
  now <- performance_now
  let app : App = MkApp
    canvas -- canvas
    ctx -- ctx
    0 -- tickCount
    0 -- frameCount
    now -- lastTickTime
    startBodyLength -- bodyLength
    ((MkVector2d 150.0 150.0) :: Nil) -- bodyPoints
    (MkVector2d 150.0 150.0) -- headPosition
    0.0 -- headAngle

  onFrame app now
