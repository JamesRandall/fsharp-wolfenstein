module FSharpWolfenstein.Desktop.Texture

open System
open Silk.NET.OpenGL
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open Microsoft.FSharp.NativeInterop

exception TextureException

type T =
  { Gl: GL
    Handle: uint32
  }

let bindWithSlot (slot:TextureUnit) texture =
  texture.Gl.ActiveTexture slot
  texture.Gl.BindTexture (TextureTarget.Texture2D, texture.Handle)

let bind = bindWithSlot TextureUnit.Texture0

let createWithImage (gl:GL) (img:Image<Rgba32>) =
  //img.Mutate(fun x -> x.Flip FlipMode.Vertical |> ignore)
  let width = img.Width |> uint
  let height = img.Height |> uint
  let handle = gl.GenTexture()
  let texture = { Gl = gl ; Handle = handle }
  texture |> bind
  
  let mutable pixelSpan:Span<Rgba32> = Span<Rgba32>()
  
  if img.TryGetSinglePixelSpan &pixelSpan then
    let pixelArray = pixelSpan.ToArray()
    use ptr = fixed pixelArray
    let voidPtr = ptr |> NativePtr.toVoidPtr
    gl.TexImage2D(TextureTarget.Texture2D, 0, InternalFormat.Rgba |> int, width, height, 0, PixelFormat.Rgba, PixelType.UnsignedByte, voidPtr)
    gl.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, GLEnum.Repeat |> int)
    gl.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, GLEnum.Repeat |> int)
    // using nearest texture scaling gives us nice blocky scaling - combined with multiples for zoom and this looks
    // pretty authentic
    gl.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, GLEnum.Nearest |> int)
    gl.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, GLEnum.Nearest |> int)
    gl.GenerateMipmap TextureTarget.Texture2D
  else
    raise TextureException
  texture
    
let create (gl:GL) (path:string) = createWithImage gl (Image.Load(path) :?> Image<Rgba32>)

    
let dispose texture = texture.Gl.DeleteTexture texture.Handle