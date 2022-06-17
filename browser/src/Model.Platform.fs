module App.PlatformModel
open Fable.Core.JS
open System

type Texture =
  { Data: Fable.Core.JS.Uint32Array
    ClampedData: Fable.Core.JS.Uint8ClampedArray
    Width: int
    Height: int
  }
  member this.setPixel (color:UInt32) x y =
    let offset = int y*int this.Width + x
    this.Data.[offset] <- color
  member this.getPixel x y =
    let offset = int y*int this.Width + x
    this.Data.[offset]

