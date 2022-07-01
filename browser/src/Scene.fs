module App.Scene

open App
open App.Model
open Browser.Types
open Fable.Core
open Browser
open Fable.Core.JS
open Fable.Core.JsInterop
open App.Render
open App.PlatformModel

let private viewportZoom = 2.
let private zoom = 2.
let private wolfViewportWidth = 304.
let private wolfViewportHeight = 152.

let private getViewportPosition (context:CanvasRenderingContext2D) =
  let totalZoomedWidth = wolfViewportWidth * zoom * viewportZoom
  let left = context.canvas.width / 2. - totalZoomedWidth / 2.
  
  let totalZoomedHeight = wolfViewportHeight * zoom * viewportZoom
  let statusBarHeight = 35. * zoom * viewportZoom
  let statusBarViewportSpace = round (statusBarHeight / 5.)
  let totalHeight = totalZoomedHeight + statusBarHeight + statusBarViewportSpace
  let topBasedOnHeight = (context.canvas.height/2. - totalHeight/2.) 
  let top = topBasedOnHeight
  
  left,top,(top+totalZoomedHeight+statusBarViewportSpace),totalZoomedWidth,totalZoomedHeight

let private drawFrame (context:CanvasRenderingContext2D) =
  context.fillStyle <- ("#004040" |> U3.Case1)
  context.fillRect(0, 0, context.canvas.width, context.canvas.height)
  
  let left, top, statusBarTop,totalZoomedWidth, totalZoomedHeight = getViewportPosition context
  let left = left-1.
  let top = top-1.
  
  // bottom right
  context.strokeStyle <- ("#007070" |> U3.Case1)
  context.beginPath()
  context.moveTo(left, top + totalZoomedHeight+1.)
  context.lineTo(left + totalZoomedWidth+1.,top + totalZoomedHeight+1.)
  context.lineTo(left + totalZoomedWidth+1., top)
  context.stroke()
  // top left
  context.lineCap <- "square"
  context.strokeStyle <- ("#000000" |> U3.Case1)
  context.beginPath()
  context.moveTo(left, top + totalZoomedHeight+1.)
  context.lineTo(left, top)
  context.lineTo(left + totalZoomedWidth+1.5, top)
  context.stroke()

let draw (context:CanvasRenderingContext2D) (bufferContext:CanvasRenderingContext2D) statusBarGraphics graphics sprites game  =
  let startTime = performance.now ()
  
  context.save ()
  context?imageSmoothingEnabled <- false // bufferContextOption |> Option.isSome
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  context.font <- "30px Consolas, Menlo, monospace"
  
  
  bufferContext?imageSmoothingEnabled <- false
  let viewportWidth = bufferContext.canvas.width
  let viewportHeight = bufferContext.canvas.height
  
  let ceilingColor = 0x393939
  let floorColor = 0x737373
  Primitives.fill bufferContext ceilingColor -1. -1. (viewportWidth+1.) (viewportHeight/2.)
  Primitives.fill bufferContext floorColor -1. (viewportHeight/2.) (viewportWidth+1.) ((viewportHeight/2.)+1.)
 
  let outputImageData = bufferContext.getImageData(0., 0., viewportWidth, viewportHeight)
  let outputTexture = {
    Data = Constructors.Uint32Array.Create(outputImageData.data.buffer)
    ClampedData = Constructors.Uint8ClampedArray.Create(outputImageData.data.buffer)
    SourceCanvas = bufferContext.canvas
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
  Weapons.drawPlayerWeapon bufferContext.canvas.width bufferContext.canvas.height getPixel setPixel isTransparent game
  Dissolve.render setPixel game
  drawFrame context
  
  let left, top, statusBarTop, _, _ = getViewportPosition context
  
  let imageData = Graphics.createImageData outputTexture.ClampedData outputTexture.Width outputTexture.Height
  bufferContext.putImageData(imageData, 0., 0.)
  context.drawImage ((U3.Case2 bufferContext.canvas), left, top, viewportWidth*zoom, viewportHeight*zoom)
  
  context.save()
  context.translate (left, statusBarTop)
  let drawStatusBarImage (context:CanvasRenderingContext2D) texture x y =
    let totalZoom = zoom * viewportZoom
    let drawImage (context:CanvasRenderingContext2D) texture x y width height =
      context.drawImage ((U3.Case2 (Graphics.canvasFromTexture texture)), x, y, width, height)
    drawImage context texture (x * totalZoom) (y * totalZoom) (float texture.Width * totalZoom) (float texture.Height * totalZoom)
  Render.StatusBar.drawStatusBar statusBarGraphics (drawStatusBarImage context) game
  context.restore()
  
  match game.ViewportFilter with
  | ViewportFilter.Overlay overlay ->
    context.fillStyle <- U3.Case1 $"rgba({overlay.Red},{overlay.Green},{overlay.Blue},{overlay.Opacity})"
    context.fillRect(-1., -1., context.canvas.width + 2., context.canvas.height + 2.)
  | _ -> ()
  
  let endTime = performance.now()
  Primitives.fillText context $"Render length: %.0f{endTime-startTime}ms" left (top / 2. + 8.)
  
  context.restore ()
  
  firingHitGameObjectIndexOption
  
  
// I would like to pass the actual renderers as parameters here so that the scene drawer doesn't need to access the
// Render.Walls.draw directly and Scene.fs can be before the cross platform code in the hierarchy. However doing so adds
// 30ms onto the frame rendering time. Need to investigate why.
let initScene (canvas:HTMLCanvasElement) _ =
  let context = canvas.getContext_2d()
 
  let bufferContext,canvasWidth,canvasHeight =
    let bufferCanvas = document.createElement("canvas") :?> HTMLCanvasElement
    // original game had a max viewport size of 304x152 pixels
    bufferCanvas.width <- wolfViewportWidth * viewportZoom
    bufferCanvas.height <- wolfViewportHeight * viewportZoom
    //bufferCanvas.width <- 320. * 2.
    //bufferCanvas.height <- 200. * 2.
    bufferCanvas.getContext_2d (),int bufferCanvas.width, int bufferCanvas.height
  (draw context bufferContext),canvasWidth,canvasHeight