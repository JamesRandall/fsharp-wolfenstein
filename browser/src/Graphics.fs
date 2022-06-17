module App.Graphics

open System
open Browser.Dom
open Browser.Types
open Fable.Core
open App.Model
open GraphicsCommon
open App.PlatformModel

let spriteColorIsTransparent color = color = 4287234202ul

[<Emit("new ImageData($0,$1,$2)")>]
let createImageData (data:Fable.Core.JS.Uint8ClampedArray) width height : ImageData = jsNative

let scaleSprite _ _ image = image

let loadSprites () = async {
  // bit nasty - need to clean up
  // I may return to drawing sprites from the vswap file but its a bit of a ballache due to its internal format
  // being optimised for VGA
  let textureWidth = 64.
  let textureHeight = 64.
  let imageDataArray = FSharp.Collections.Array.create 436 None
  {0..435}
  |> Seq.iter(fun index ->
    let image =
      let spriteFileIndex =
        if index |> Assets.isSharewareSprite || not Assets.isShareware then
          index
        else
          0        
      try
        Image.Create(
          textureWidth,
          textureHeight,
          src = $"assets/sprites/s{spriteFileIndex}.png",
          onerror =
            (fun _ ->
              let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
              canvas.width <- 64
              canvas.height <- 64
              let context = canvas.getContext_2d()
              let buffer = context.getImageData(0., 0., 64., 64.).data.buffer
              imageDataArray.[index] <-
                { Data = JS.Constructors.Uint32Array.Create(buffer)
                  ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
                  Width = 64
                  Height = 64 } |> Some
            )
          )
      with
      | _ -> Image.Create(textureWidth, textureHeight, src = $"assets/sprites/s0.png")
    let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
    canvas.width <- image.width
    canvas.height <- image.height
    image.onload <- (fun _ ->      
      let context = canvas.getContext_2d()
      context.drawImage(Fable.Core.U3.Case1 image, 0., 0., image.width, image.height)
      let buffer = context.getImageData(0., 0., image.width, image.height).data.buffer
      imageDataArray.[index] <-
        { Data = JS.Constructors.Uint32Array.Create(buffer)
          ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
          Width = int image.width
          Height = int image.height } |> Some
    )
  )
  while imageDataArray |> Array.contains None do
    do! Async.Sleep (TimeSpan.FromMilliseconds 500.)
  return imageDataArray |> Array.map(fun element -> element.Value)
}
  
let canvasFromTexture (texture:Texture) =
  let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
  canvas.width <- texture.Width
  canvas.height <- texture.Height
  let context = canvas.getContext_2d()
  // We probably ought to copy the sprite first but its ok for now in the context we use this (only do it to sprites
  // that are rendered via canvas)
  {0..63}
  |> Seq.iter(fun y ->
    {0..63}
    |> Seq.iter(fun x ->
      if getPixel texture x y |> spriteColorIsTransparent then
        setPixel texture 0x000000FFul x y
    )
  )
  let imageData = createImageData texture.ClampedData texture.Width texture.Height
  context.putImageData(imageData, 0., 0.)
  canvas
  
