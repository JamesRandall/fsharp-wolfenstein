module App.PlatformModel

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System

type Texture =
  { Image: Image<Rgba32>
    Width: int
    Height: int
  }
  member this.setPixel (color:UInt32) x y =
    (*let color = Color.FromRgba(
      (color >>> 24) &&& 0xFFul |> byte,
      (color >>> 16) &&& 0xFFul |> byte,
      (color >>> 8) &&& 0xFFul |> byte,
      color &&& 0xFFul |> byte);*)
    this.Image[x,y] <- Rgba32(color)
  member this.getPixel x y =
    let color = this.Image[x,y]
    color.PackedValue