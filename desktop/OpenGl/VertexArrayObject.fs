module FSharpWolfenstein.Desktop.VertexArrayObject

open System
open Silk.NET.OpenGL

type VertexArrayObject =
  { Gl: GL
    Handle: uint32
    VertexSize: uint
  }
  
let bind vertexArrayObject =
  vertexArrayObject.Gl.BindVertexArray vertexArrayObject.Handle
  
let create (gl:GL) (vertexBufferObject:BufferObject.BufferObject) elementBufferObject =
  let vertexArrayObject = { Gl = gl ; Handle = gl.GenVertexArray() ; VertexSize = vertexBufferObject.Size }
  bind vertexArrayObject
  BufferObject.bind vertexBufferObject
  BufferObject.bind elementBufferObject
  vertexArrayObject
  
let vertexAttributePointer vertexArrayObject index count (pointerType:VertexAttribPointerType) vertexSize offset =
  vertexArrayObject.Gl.VertexAttribPointer (
    index,
    count,
    pointerType,
    false,
    vertexSize * vertexArrayObject.VertexSize,
    IntPtr(offset * (vertexArrayObject.VertexSize |> int)).ToPointer()
  )
  vertexArrayObject.Gl.EnableVertexAttribArray index
  
