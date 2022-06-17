module FSharpWolfenstein.Desktop.SpriteRenderer
open System.Numerics
open Silk.NET.OpenGL
open System

(*
let private vertices = [|
  0.0f; 1.0f; 0.0f; 1.0f
  1.0f; 0.0f; 1.0f; 0.0f
  0.0f; 0.0f; 0.0f; 0.0f 

  0.0f; 1.0f; 0.0f; 1.0f
  1.0f; 1.0f; 1.0f; 1.0f
  1.0f; 0.0f; 1.0f; 0.0f
|]*)

let private vertices = [|
  1.0f ; 1.0f ; 1.0f ; 1.0f
  1.0f ; 0.0f ; 1.0f ; 0.0f 
  0.0f ; 0.0f ; 0.0f ; 0.0f
  0.0f ; 1.0f ; 0.0f ; 1.0f
|]

(*
let vertices = [|
  0.5f ; 0.5f ; 1.0f ; 1.0f
  0.5f ; -0.5f ; 1.0f ; 0.0f 
  -0.5f ; -0.5f ; 0.0f ; 0.0f
  -0.5f ; 0.5f ; 0.0f ; 1.0f
|]*)


let private indices = [|
  0u ; 1u ; 3u
  1u ; 2u ; 3u
|]

let create gl consoleWidth consoleHeight =
  let vbo = BufferObject.create gl vertices BufferTargetARB.ArrayBuffer
  let ebo = BufferObject.create gl indices BufferTargetARB.ElementArrayBuffer      
  let vao = VertexArrayObject.create gl vbo ebo
  
  VertexArrayObject.vertexAttributePointer vao 0u 2 VertexAttribPointerType.Float 4u 0
  VertexArrayObject.vertexAttributePointer vao 1u 2 VertexAttribPointerType.Float 4u 2
  
  let shader = Shader.createShader gl "sprite"
  let projectionMatrix = Matrix4x4.CreateOrthographicOffCenter(0.0f, consoleWidth, consoleHeight, 0.0f, -1.0f, 1.0f)
  
  let render (position:Vector2) (size:Vector2) (texture:Texture.T) =
    
    let model =
      Matrix4x4.Identity *
      Matrix4x4.CreateScale(Vector3(size.X, size.Y, 1.0f)) *
      Matrix4x4.CreateTranslation(Vector3(position.X, position.Y, 0.0f))      
    
    Shader.useShader shader
    Texture.bindWithSlot TextureUnit.Texture0 texture
    Shader.setUniformInt shader "image" 0
    Shader.setMatrix4x4 shader "projection"  projectionMatrix
    Shader.setMatrix4x4 shader "model" model
    Shader.setVector3f shader "spriteColor" (Vector3(1.0f, 1.0f, 1.0f))    
    
    VertexArrayObject.bind vao
    gl.DrawElements (PrimitiveType.Triangles, indices.Length |> uint32, DrawElementsType.UnsignedInt, IntPtr.Zero.ToPointer())        
    
  render
    