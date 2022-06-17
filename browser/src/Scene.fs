module App.Scene

open App
open Browser.Types
open Fable.Core
open App.Model
open Browser
open Fable.Core.JS
open Fable.Core.JsInterop
open App.Render
open App.PlatformModel

let draw (context:CanvasRenderingContext2D) (bufferContextOption:CanvasRenderingContext2D option) graphics sprites game  =
  let startTime = performance.now ()
  
  context.save ()
  context?imageSmoothingEnabled <- false // bufferContextOption |> Option.isSome
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  context.font <- "30px Consolas, Menlo, monospace"
  
  let bufferContext = defaultArg bufferContextOption context
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
  
  if bufferContextOption |> Option.isSome then 
    let imageData = Graphics.createImageData outputTexture.ClampedData outputTexture.Width outputTexture.Height
    bufferContext.putImageData(imageData, 0., 0.)
    // we draw the weapon directly on the buffer context - it doesn't have any relation to other objects in the frame
    // and by rendering it here we can take advantage of the speed of drawImage (compared to pixel by pixel manipulation)
    // and as its big (height of the context) this is quite an optimisation
    let drawImage texture x y width height =
      bufferContext.drawImage ((U3.Case2 (Graphics.canvasFromTexture texture)), x, y, width, height)
      //((U3.Case2 (Graphics.canvasFromTexture weapon.CurrentSprite)), xPos, 0., height, height)
    Weapons.drawPlayerWeapon bufferContext.canvas.width bufferContext.canvas.height drawImage game
    context.drawImage ((U3.Case2 bufferContext.canvas), 0., 0., viewportWidth*2., viewportHeight*2.)
  else
    let imageData = Graphics.createImageData outputTexture.ClampedData outputTexture.Width outputTexture.Height
    context.putImageData(imageData, 0., 0.)
  
  let endTime = performance.now()
  Primitives.fillText context $"Render length: %.0f{endTime-startTime}ms" 32. 32.
  
  context.restore ()
  firingHitGameObjectIndexOption
  
// I would like to pass the actual renderers as parameters here so that the scene drawer doesn't need to access the
// Render.Walls.draw directly and Scene.fs can be before the cross platform code in the hierarchy. However doing so adds
// 30ms onto the frame rendering time. Need to investigate why.
let initScene (canvas:HTMLCanvasElement) _ =
  let context = canvas.getContext_2d()
  let useLowResolutionBuffer = true
  let bufferContext,canvasWidth,canvasHeight =
    if useLowResolutionBuffer then
      let bufferCanvas = document.createElement("canvas") :?> HTMLCanvasElement
      // original game had a max viewport size of 304x152 pixels
      bufferCanvas.width <- 304. * 2. // 576. // 286. * 2.
      bufferCanvas.height <- 152. * 2. // 142. * 2.
      bufferCanvas.getContext_2d () |> Some,int bufferCanvas.width, int bufferCanvas.height
    else
      None,int canvas.width,int canvas.height
  (draw context bufferContext),canvasWidth,canvasHeight