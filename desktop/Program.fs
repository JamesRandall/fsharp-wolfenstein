// For more information see https://aka.ms/fsharp-console-apps

open System.Diagnostics
open App
open App.Model
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

//let screenWidth = 608
//let screenHeight = 304
let zoom = 2.

let private viewportZoom = 2.

let private wolfViewportWidth = 304.
let private wolfViewportHeight = 152.

let screenWidth = 320. * zoom
let screenHeight = 200. * zoom

let private getViewportPosition () =
  let zoomedViewportWidth = wolfViewportWidth * viewportZoom
  let left = float screenWidth / 2. - zoomedViewportWidth / 2.
  let zoomedViewportHeight = wolfViewportHeight * viewportZoom
  let statusBarHeight = 35. * zoom
  let verticalSpacing = (screenHeight - statusBarHeight - zoomedViewportHeight) / 3.
  
  left,verticalSpacing,(verticalSpacing+zoomedViewportHeight+verticalSpacing),zoomedViewportWidth,zoomedViewportHeight

let emptyGame =
  { Map = []
    Areas = []
    CompositeAreas = []
    Level = 0
    Player = {
      Score = 0<points>
      Health = 100<hp>
      Radius = 0.5
      Weapons = [  ]
      CurrentWeaponIndex = -1
      Ammunition = 99<bullets>
      Lives = 3<life>
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
    ViewportFilter = ViewportFilter.None
    PixelDissolver = None
  }

type RenderData =
  { Gl: GL
    ScreenImage: SixLabors.ImageSharp.Image<Rgba32>
    ViewportImage: SixLabors.ImageSharp.Image<Rgba32>
    SpriteRenderer: Vector2 -> Vector2 -> Texture.T -> unit
    DiagnosticFont: Font
  }
let mutable renderDataOption = None
let mutable gameLoop:(Model.Game -> float<Model.ms> -> Model.Game) = (fun _ _ -> emptyGame)
let mutable gameState:Model.Game = emptyGame
let mutable updateControlState = (fun game _ -> game)

open PlatformModel
open App.Render

let drawScene (screenImage:SixLabors.ImageSharp.Image<Rgba32>) (viewportImage:SixLabors.ImageSharp.Image<Rgba32>) diagnosticFont statusBarGraphics graphics sprites game =
  let stopwatch = Stopwatch.StartNew()
  
  let left,top,statusBarTop,viewportWidth,viewportHeight = getViewportPosition ()
  let viewportWidth = viewportImage.Width
  let viewportHeight = viewportImage.Height
  
  viewportImage.Mutate(fun x ->
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
    Image = viewportImage
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
  Weapons.drawPlayerWeapon viewportWidth viewportHeight getPixel setPixel isTransparent game
  Dissolve.render setPixel game
    
  (*let drawImage texture x y _ _ =
    viewportImage.Mutate(fun i ->
      DrawImageExtensions.DrawImage(i, texture.Image, Point(int x,int y), GraphicsOptions(Antialias=false)) |> ignore
    )*)
    
    //bufferContext.drawImage ((U3.Case2 (Graphics.canvasFromTexture texture)), x, y, width, height)
    //((U3.Case2 (Graphics.canvasFromTexture weapon.CurrentSprite)), xPos, 0., height, height)
  
    
  stopwatch.Stop()
  
  viewportImage.Mutate(fun img ->
    img.DrawText ($"{stopwatch.ElapsedMilliseconds}ms", diagnosticFont, Color(Rgba32(1.0f,1.0f,1.0f,1.0f)), PointF(8.0f,8.0f)) |> ignore
  )
  
  screenImage.Mutate(fun img ->
    let drawStatusBarImage texture x y =
      DrawImageExtensions.DrawImage(img, texture.Image, Point(int left + int (x * zoom),int statusBarTop + int (y * zoom)), GraphicsOptions(Antialias=false)) |> ignore
    let borderWidth = 1.5f
    // teal background
    FillRectangleExtensions.Fill(
      img,
      SolidBrush(Color.FromRgb(0x00uy,0x40uy,0x40uy)),
      RectangleF(0.f, 0.f, float32 screenWidth, float32 screenHeight)
    ) |> ignore
    img.DrawLines(
      SolidBrush(Color.FromRgb(0x00uy, 0x00uy, 0x00uy)),
      borderWidth,
      [|
        PointF(float32 left-borderWidth, float32 (int top+viewportHeight) + borderWidth)
        PointF(float32 left-borderWidth, float32 top-borderWidth)
        PointF(float32 (left + float viewportWidth), float32 top-borderWidth)
      |]
    ) |> ignore
    img.DrawLines(
      SolidBrush(Color.FromRgb(0x00uy, 0x70uy, 0x70uy)),
      borderWidth,
      [|
        PointF(float32 (left + float viewportWidth), float32 top)
        PointF(float32 (left + float viewportWidth), float32 (int top+viewportHeight) + borderWidth - 1.0f)
        PointF(float32 left-borderWidth, float32 (int top+viewportHeight) + borderWidth - 1.0f)
      |]
    ) |> ignore
    // raycast viewport
    img.DrawImage(viewportImage, Point(int left,int top), GraphicsOptions()) |> ignore
    // statusbar
    Render.StatusBar.drawStatusBar statusBarGraphics drawStatusBarImage game
    
    // right at the end we render any viewport filter (e.g. blood / hit effect) over the top of everything
    match game.ViewportFilter with
    | ViewportFilter.Overlay overlay ->
      FillRectangleExtensions.Fill(
        img,
        SolidBrush(Color.FromRgba(overlay.Red, overlay.Green, overlay.Blue, byte (255. * overlay.Opacity))),
        RectangleF(0.f, 0.f, float32 screenWidth, float32 screenHeight)
      ) |> ignore
    | _ -> ()
  )
  
  let endTime = stopwatch.ElapsedMilliseconds
  firingHitGameObjectIndexOption
  
let initScene (renderData:RenderData) _ =
  (drawScene renderData.ScreenImage renderData.ViewportImage renderData.DiagnosticFont),renderData.ViewportImage.Width,renderData.ViewportImage.Height

let load (window:IWindow) _ =
  let gl = GL.GetApi(window)
  let fontCollection = FontCollection()
  let fontFamily = fontCollection.Install("Fonts/8-bit-hud.ttf")
  let renderData =
    { SpriteRenderer = SpriteRenderer.create gl (float32 screenWidth) (float32 screenHeight)
      Gl = gl
      ViewportImage = new SixLabors.ImageSharp.Image<Rgba32>(wolfViewportWidth * zoom |> int, wolfViewportHeight * zoom |> int, Rgba32(0.0f,0.0f,0.0f,0.0f))
      ScreenImage = new SixLabors.ImageSharp.Image<Rgba32>(window.Size.X, window.Size.Y, Rgba32(0.0f,0.0f,0.0f,0.0f))
      DiagnosticFont = fontFamily.CreateFont(16.f, FontStyle.Regular)
    }
  renderDataOption <- Some renderData
    
  let gameLoopResult,controlStateHandlerResult,initialGameStateResult =
    App.Game.init zoom (initScene renderData) |> Async.RunSynchronously
  
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
    //let setPixel x y color =
      // ImageSharp wants to work per row for best performance but Wolfenstein renders per column
      // One optimisation would be to use a rotated image. I.e. have the image we render to rotated 90 degrees
      // and draw to that per row (so we are drawing Wolfenstein at a rotated angle) and then use OpenGL and the graphics
      // card to rotate the image back 90 degrees.
      //
      // Would likely need some internal work on the Wolf renderer but would be fun to try
    //  rd.Image.[x,y] <- color
    
    let newGameState = gameLoop gameState (frameTime*1000.<Model.ms>)
    gameState <- newGameState
    
    let texture = Texture.createWithImage rd.Gl rd.ScreenImage 
    let vSpriteSize = Vector2(rd.ScreenImage.Width |> float32, rd.ScreenImage.Height |> float32)
    rd.SpriteRenderer (Vector2(0.0f,0.0f)) vSpriteSize texture
    texture |> Texture.dispose
  | _ -> ()
  
let close () =
  ()

[<EntryPoint>]
let main _ =
  let mutable options = WindowOptions.Default
  options.Size <- Vector2D<int>(int(float screenWidth * zoom),int (float screenHeight * zoom))
  options.Title <- "F# Wolfenstein"
  let window = Window.Create(options)
  window.add_Load (load window)
  window.add_Render render
  window.add_Closing close
  
  window.Run ()
  
  0