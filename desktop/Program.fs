// For more information see https://aka.ms/fsharp-console-apps

open System.Diagnostics
open App
open FSharpWolfenstein.Desktop
open Silk.NET.OpenGL
open Silk.NET.Windowing
open Silk.NET.Maths
open Silk.NET.Input
open System.Numerics
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Model

let screenWidth = 608
let screenHeight = 304
let zoom = 2

let emptyGame =
  { Map = []
    Player = {
      Score = 0<points>
      Health = 100<hp>
      Radius = 0.5
      Weapons = [  ]
      CurrentWeaponIndex = -1
      Ammunition = 99
    }
    Camera = {
      Position = { vX = 12. ; vY = 6. }
      Direction = { vX = -1. ; vY = 0. }
      Plane = { vX = 0. ; vY = 1. } // 0.666 }
      FieldOfView = 1.
    }
    ControlState = ControlState.None
    GameObjects = []
    IsFiring = false
    TimeToNextWeaponFrame = None
    Doors = []
  }

type RenderData =
  { Gl: GL
    Image: SixLabors.ImageSharp.Image<Rgba32>
    SpriteRenderer: Vector2 -> Vector2 -> Texture.T -> unit
    DiagnosticFont: Font
  }
let mutable renderDataOption = None
let mutable gameLoop:(Model.Game -> float<Model.ms> -> Model.Game) = (fun _ _ -> emptyGame)
let mutable gameState:Model.Game = emptyGame
let mutable updateControlState = (fun game _ -> game)

open PlatformModel
open App.Render

let drawScene (image:SixLabors.ImageSharp.Image<Rgba32>) diagnosticFont graphics sprites game =
  let stopwatch = Stopwatch.StartNew()
  
  let viewportWidth = screenWidth
  let viewportHeight = screenHeight
  
  image.Mutate(fun x ->
    FillRectangleExtensions.Fill(
      x,
      SolidBrush(Color.FromRgb(0x39uy,0x39uy,0x39uy)),
      RectangleF(-1.f, -1.f, (float32 viewportWidth+1.f), (float32 viewportHeight/2.f))
    ) |> ignore
    FillRectangleExtensions.Fill(
      x,
      SolidBrush(Color.FromRgb(0x73uy,0x73uy,0x73uy)),
      RectangleF(-1.f, (float32 viewportHeight/2.f), (float32 viewportWidth+1.f), (float32 viewportHeight/2.f)+1.f)
    ) |> ignore
  )
 
  let outputTexture = {
    Image = image
    Width = int viewportWidth
    Height = int viewportHeight
  }

  let getPixel = GraphicsCommon.getPixel
  let setPixel = GraphicsCommon.setPixel outputTexture
  let getSpriteOffsets = GraphicsCommon.spriteOffsets graphics
  let getTextureStripOffset = GraphicsCommon.textureStripOffset graphics
  let getTextureColor = GraphicsCommon.textureColor graphics
  let isTransparent = Graphics.spriteColorIsTransparent
  
  let firingHitGameObjectIndexOption = 
    game
    |> Walls.draw viewportWidth viewportHeight setPixel getTextureStripOffset getTextureColor
    |> Objects.draw viewportWidth viewportHeight getPixel setPixel isTransparent getSpriteOffsets game sprites
    
  let drawImage texture x y _ _ =
    image.Mutate(fun i ->
      DrawImageExtensions.DrawImage(i, texture.Image, Point(int x,int y), GraphicsOptions(Antialias=false)) |> ignore
    )
    
    //bufferContext.drawImage ((U3.Case2 (Graphics.canvasFromTexture texture)), x, y, width, height)
    //((U3.Case2 (Graphics.canvasFromTexture weapon.CurrentSprite)), xPos, 0., height, height)
  Weapons.drawPlayerWeapon viewportWidth viewportHeight drawImage game
    
  stopwatch.Stop()
  
  image.Mutate(fun img ->
    img.DrawText ($"{stopwatch.ElapsedMilliseconds}ms", diagnosticFont, Color(Rgba32(1.0f,1.0f,1.0f,1.0f)), PointF(8.0f,8.0f)) |> ignore
  )
  
  let endTime = stopwatch.ElapsedMilliseconds
  firingHitGameObjectIndexOption
  
let initScene (renderData:RenderData) () =
  (drawScene renderData.Image renderData.DiagnosticFont),renderData.Image.Width/zoom,renderData.Image.Height/zoom

let load (window:IWindow) _ =
  let gl = GL.GetApi(window)
  let fontCollection = FontCollection()
  let fontFamily = fontCollection.Install("Fonts/8-bit-hud.ttf")
  let renderData =
    { SpriteRenderer = SpriteRenderer.create gl (float32 screenWidth) (float32 screenHeight)
      Gl = gl
      Image = new SixLabors.ImageSharp.Image<Rgba32>(window.Size.X, window.Size.Y, Rgba32(0.0f,0.0f,0.0f,0.0f))
      DiagnosticFont = fontFamily.CreateFont(16.f, FontStyle.Regular)
    }
  renderDataOption <- Some renderData
    
  let gameLoopResult,controlStateHandlerResult,initialGameStateResult =
    App.Game.init (initScene renderData) |> Async.RunSynchronously
  
  let controlStateFromKeyCode key =
    match key with
    | Key.Up -> ControlState.Forward
    | Key.Left -> ControlState.TurningLeft
    | Key.Right -> ControlState.TurningRight
    | Key.Q -> ControlState.StrafingLeft
    | Key.E -> ControlState.StrafingRight
    | Key.Down -> ControlState.Backward
    | Key.ControlLeft -> ControlState.Fire
    | Key.Space -> ControlState.Action
    | _ -> ControlState.None
  
  let inputContext = window.CreateInput()
  inputContext.Keyboards |> Seq.iter(fun keyboard ->
    keyboard.add_KeyDown (fun keyboard key keyArgs ->
      let controlState = key |> controlStateFromKeyCode
      if controlState <> ControlState.None then
        gameState <- controlStateHandlerResult gameState controlState
    )
    keyboard.add_KeyUp (fun keyboard key keyArgs ->
      let controlState = key |> controlStateFromKeyCode
      if controlState <> ControlState.None then
        gameState <- controlStateHandlerResult gameState controlState
    )
  )
    
  gameLoop <- gameLoopResult
  updateControlState <- controlStateHandlerResult
  gameState <- initialGameStateResult
  
  ()

let render (frameTime:double) =
  match renderDataOption with
  | Some rd ->
    //rd.Gl.Clear(ClearBufferMask.ColorBufferBit)
    let setPixel x y color =
      // ImageSharp wants to work per row for best performance but Wolfenstein renders per column
      // One optimisation would be to use a rotated image. I.e. have the image we render to rotated 90 degrees
      // and draw to that per row (so we are drawing Wolfenstein at a rotated angle) and then use OpenGL and the graphics
      // card to rotate the image back 90 degrees.
      //
      // Would likely need some internal work on the Wolf renderer but would be fun to try
      rd.Image.[x,y] <- color
    
    let newGameState = gameLoop gameState (frameTime*1000.<Model.ms>)
    gameState <- newGameState
    
    let texture = Texture.createWithImage rd.Gl rd.Image 
    let vSpriteSize = Vector2(rd.Image.Width |> float32, rd.Image.Height |> float32)
    rd.SpriteRenderer (Vector2(0.0f,0.0f)) vSpriteSize texture
    texture |> Texture.dispose
  | _ -> ()
  
let close () =
  ()

[<EntryPoint>]
let main _ =
  let mutable options = WindowOptions.Default
  options.Size <- Vector2D<int>(screenWidth * zoom,screenHeight * zoom)
  options.Title <- "F# Wolfenstein"
  let window = Window.Create(options)
  window.add_Load (load window)
  window.add_Render render
  window.add_Closing close
  
  window.Run ()
  
  0