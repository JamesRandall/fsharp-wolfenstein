module App.Graphics

open App.PlatformModel
open Fable.Core
open Model
open PlatformModel
open SixLabors.ImageSharp.PixelFormats

let spriteColorIsTransparent color = color = 4287168665ul

let loadSprites _ = async {
  //let textureWidth = 64.
  //let textureHeight = 64.
  //let imageDataArray = FSharp.Collections.Array.create 436 None
  return
    {0..435}
    |> Seq.map(fun index ->
      let spriteFileIndex =
        if index |> Assets.isSharewareSprite || not Assets.isShareware then
          index
        else
          0
      let bytes = Utils.loadAssetBytes $"Sprites.s{spriteFileIndex}.png"
      let image = SixLabors.ImageSharp.Image.Load<Rgba32> bytes
      { Image = image
        Width = image.Width
        Height = image.Height
      }
    )
    |> Seq.toArray
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
      let toX = int (float fromX + scaleX)
      let fromY = int (float sourceY * scaleY)
      let toY = int (float fromY + scaleY)
      {fromX..toX}
      |> Seq.iter(fun targetX ->
        {fromY..toY} |> Seq.iter(fun targetY -> newImage[targetX,targetY] <- color)
      )
    )
  )
  { texture with Image = newImage ; Width = newImage.Width ; Height = newImage.Height }

