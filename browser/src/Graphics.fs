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

let loadTexture name textureWidth textureHeight= async {
  let mutable imageData = None
  
  let image =
    try
      Image.Create(
        textureWidth,
        textureHeight,
        src = name,
        onerror =
          (fun _ ->
            let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
            canvas.width <- textureWidth
            canvas.height <- textureHeight
            let context = canvas.getContext_2d()
            let buffer = context.getImageData(0., 0., textureWidth, textureHeight).data.buffer
            imageData <-
              { Data = JS.Constructors.Uint32Array.Create(buffer)
                ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
                SourceCanvas = canvas
                Width = int textureWidth
                Height = int textureHeight } |> Some
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
    imageData <-
      { Data = JS.Constructors.Uint32Array.Create(buffer)
        ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
        SourceCanvas = canvas
        Width = int image.width
        Height = int image.height } |> Some
  )
  
  while imageData |> Option.isNone do
    do! Async.Sleep (TimeSpan.FromMilliseconds 500.)
  return imageData.Value
}

let loadTextureSet nameFormatter fallbackName textureWidth textureHeight checkShareware indexes = async {
  // I may return to drawing sprites from the vswap file but its a bit of a ballache due to its internal format
  // being optimised for VGA
  let imageDataArray = FSharp.Collections.Array.create (indexes |> Seq.length) None
  indexes
  |> Seq.iter(fun index ->
    let image =
      let spriteFileIndex =
        if not checkShareware || index |> Assets.isSharewareSprite || not Assets.isShareware then
          index
        else
          0        
      try
        Image.Create(
          //textureWidth,
          //textureHeight,
          src = nameFormatter spriteFileIndex,
          onerror =
            (fun _ ->
              let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
              canvas.width <- textureWidth
              canvas.height <- textureHeight
              let context = canvas.getContext_2d()
              let buffer = context.getImageData(0., 0., textureWidth, textureHeight).data.buffer
              imageDataArray.[index] <-
                { Data = JS.Constructors.Uint32Array.Create(buffer)
                  ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
                  SourceCanvas = canvas
                  Width = 64
                  Height = 64 } |> Some
            )
          )
      with
      | _ -> Image.Create(textureWidth, textureHeight, src = fallbackName)
    let canvas = window.document.createElement("canvas") :?> Browser.Types.HTMLCanvasElement
    image.onload <- (fun _ ->
      canvas.width <- image.width
      canvas.height <- image.height 
      let context = canvas.getContext_2d()
      context.drawImage(Fable.Core.U3.Case1 image, 0., 0., image.width, image.height)
      let buffer = context.getImageData(0., 0., image.width, image.height).data.buffer
      imageDataArray.[index] <-
        { Data = JS.Constructors.Uint32Array.Create(buffer)
          ClampedData = JS.Constructors.Uint8ClampedArray.Create(buffer)
          SourceCanvas = canvas
          Width = int image.width
          Height = int image.height } |> Some
    )
  )
  while imageDataArray |> Array.contains None do
    do! Async.Sleep (TimeSpan.FromMilliseconds 500.)
  return imageDataArray |> Array.map(fun element -> element.Value)
}

let loadStatusBar _ = async {
  let! textureSet =
    loadTextureSet (fun i -> sprintf "assets/statusBar/PIC%05d.png" (i+109)) $"assets/sprites/s0.png" 24. 32. false {0..23}
  let! background =
    loadTexture "assets/statusBar/background.png" 304. 35.
  let! statusBarNumbers =
    loadTextureSet (fun i -> sprintf "assets/statusBar/font/%d.png" i) $"assets/sprites/s0.png" 8. 16. false {0..9}
  let! statusBarSpace = loadTexture "assets/statusBar/font/_.png" 8. 16.
  return
    { Background = background
      HealthFaces = [|
        textureSet.[0..2]
        textureSet.[3..5]
        textureSet.[6..8]
        textureSet.[9..11]
        textureSet.[12..14]
        textureSet.[15..17]
        textureSet.[18..20]
      |]
      Dead = textureSet.[21]
      GrinFace = textureSet.[22]
      GreyFace = textureSet.[23]
      Font = [| statusBarSpace |] |> Array.append statusBarNumbers
    }
}

let loadSprites () = async {
  return!
    loadTextureSet (fun i -> $"assets/sprites/s{i}.png") $"assets/sprites/s0.png" 64. 64. true {0..435} 
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
  
