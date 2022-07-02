module App.Graphics

open App.PlatformModel
open Fable.Core
open Model
open PlatformModel
open SixLabors.ImageSharp.PixelFormats

let spriteColorIsTransparent color = color = 4287168665ul || color = 0ul

let loadTexture name =
  let bytes = Utils.loadAssetBytes name
  let image = SixLabors.ImageSharp.Image.Load<Rgba32> bytes
  { Image = image
    Width = image.Width
    Height = image.Height
  }

let loadTextures skipSharewareIndicies nameFormatter indicies = 
  indicies
  |> Seq.map(fun index ->
    let spriteFileIndex =
      if not skipSharewareIndicies || index |> Assets.isSharewareSprite || not Assets.isShareware then
        index
      else
        0
    let bytes = Utils.loadAssetBytes (nameFormatter spriteFileIndex)
    let image = SixLabors.ImageSharp.Image.Load<Rgba32> bytes
    { Image = image
      Width = image.Width
      Height = image.Height
    }
  )
  |> Seq.toArray

let loadSprites _ = async {
  return
    {0..435}
    |> loadTextures true (fun si -> sprintf "Sprites.SPR%05d.png" si)
}

let scaleSprite (newWidth:float) (newHeight:float) (texture:Texture) =
  let image = texture.Image
  let newImage = new SixLabors.ImageSharp.Image<Rgba32>(int newWidth, int newHeight)
  let scaleX = newWidth / float image.Width
  let scaleY = newHeight / float image.Height
  {0..image.Width-1}
  |> Seq.iter(fun sourceX ->
    {0..image.Height-1}
    |> Seq.iter(fun sourceY ->
      let color = image[sourceX,sourceY]
      let fromX = int (float sourceX * scaleX)
      let toX = min (int (newWidth-1.)) (int (float fromX + scaleX))
      let fromY = int (float sourceY * scaleY)
      let toY = min (int (newHeight-1.)) (int (float fromY + scaleY))
      {fromX..toX}
      |> Seq.iter(fun targetX ->
        {fromY..toY} |> Seq.iter(fun targetY -> newImage[targetX,targetY] <- color)
      )
    )
  )
  { texture with Image = newImage ; Width = newImage.Width ; Height = newImage.Height }

let loadStatusBar scale = async {
  let textureSet =
    loadTextures false (fun i -> sprintf "StatusBar.PIC%05d.png" (i+109)) {0..23}
    |> Array.map(fun texture ->
      scaleSprite (float texture.Width*scale) (float texture.Height*scale) texture
    )
  let background =
    loadTexture "StatusBar.background.png"
  let statusBarNumbers =
    loadTextures false (fun i -> sprintf "StatusBar.Font.%d.png" i) {0..9}
  let statusBarSpace = loadTexture "StatusBar.Font._.png"
  return
    { Background = background |> scaleSprite (float background.Width*scale) (float background.Height*scale)
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
      Font = [|
        statusBarSpace |> scaleSprite (float statusBarSpace.Width*scale) (float statusBarSpace.Height*scale)
      |] |> Array.append (statusBarNumbers |> Array.map (fun n -> n |> scaleSprite (float n.Width*scale) (float n.Height*scale)))
    }
}
