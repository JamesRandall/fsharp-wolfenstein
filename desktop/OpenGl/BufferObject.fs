module FSharpWolfenstein.Desktop.BufferObject

open FSharp.Core
open Silk.NET.OpenGL
open Microsoft.FSharp.NativeInterop

type BufferObject =
  { Gl: GL
    Handle: uint32
    BufferType: BufferTargetARB
    Size: uint
  }

let bind bufferObject =
  bufferObject.Gl.BindBuffer(bufferObject.BufferType, bufferObject.Handle)

let inline create (gl:GL) (data:^dataType array) (bufferType:BufferTargetARB) =
  let handle = gl.GenBuffer()
  let typeSize = sizeof<'dataType>
  let result = { Gl = gl ; Handle = handle ; BufferType = bufferType ; Size = typeSize |> uint}
  result |> bind 
      
  let size = (data.Length * typeSize) |> unativeint
  use ptr = fixed data //fixed &data.[0]
  let dataVoidPtr = ptr |> NativePtr.toVoidPtr
  
  gl.BufferData(bufferType, size, dataVoidPtr, BufferUsageARB.StaticDraw)
  result
  
let inline createFloat (gl:GL) (data:float array) (bufferType:BufferTargetARB) =
  let handle = gl.GenBuffer()
  let typeSize = sizeof<float32>
  let result = { Gl = gl ; Handle = handle ; BufferType = bufferType ; Size = typeSize |> uint}
  result |> bind 
      
  let size = (data.Length * typeSize) |> unativeint
  use ptr = fixed data //fixed &data.[0]
  let dataVoidPtr = ptr |> NativePtr.toVoidPtr
  
  gl.BufferData(bufferType, size, dataVoidPtr, BufferUsageARB.StaticDraw)
  result
  
let dispose bufferObject =
  bufferObject.Gl.DeleteBuffer bufferObject.Handle