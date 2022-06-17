module FSharpWolfenstein.Desktop.Shader

open System
open System
open System.IO
open System.Numerics
open Silk.NET.OpenGL

exception ShaderException of string

type Shader =
  { Gl: GL
    Handle: uint32
  }

let loadShader (gl:GL) (shaderType:ShaderType) path =
  let src = File.ReadAllText path
  let handle = gl.CreateShader shaderType
  gl.ShaderSource (handle, src)
  gl.CompileShader handle
  let error = gl.GetShaderInfoLog handle
  if String.IsNullOrWhiteSpace error then
    Ok handle
  else
    Error $"Unable to compile shader: {error}"
  
let createShader (gl:GL) shaderName =
  let vertexShaderResult = loadShader gl ShaderType.VertexShader $"Shaders/{shaderName}.vert"
  let fragmentShaderResult = loadShader gl ShaderType.FragmentShader $"Shaders/{shaderName}.frag"
  match vertexShaderResult, fragmentShaderResult with
  | Ok vertexShader, Ok fragmentShader ->
    let handle = gl.CreateProgram()
    let detach () =
      gl.DetachShader (handle,vertexShader)
      gl.DetachShader (handle,fragmentShader)
      gl.DeleteShader vertexShader
      gl.DeleteShader fragmentShader    
    gl.AttachShader (handle, vertexShader)
    gl.AttachShader (handle, fragmentShader)  
    gl.LinkProgram handle      
    let status = gl.GetProgram(handle, GLEnum.LinkStatus)
    if status = 0 then
      detach ()
      ShaderException $"Shader failed to link with error {gl.GetProgramInfoLog(handle)}" |> raise
    else
      detach ()
      { Gl = gl ; Handle = handle }
  | Error msg1, Error msg2 -> ShaderException $"{msg1}\n{msg2}" |> raise
  | Error msg, _ -> ShaderException msg |> raise
  | _, Error msg -> ShaderException msg |> raise

let useShader shader = shader.Gl.UseProgram shader.Handle
  
let setUniformInt shader (name:string) (value:int) =
  let location = shader.Gl.GetUniformLocation(shader.Handle, name)
  useShader shader
  shader.Gl.Uniform1(location, value)

let setUniformFloat shader (name:string) (value:float) =
  let location = shader.Gl.GetUniformLocation(shader.Handle, name)
  useShader shader
  shader.Gl.Uniform1(location, value |> float32)
  
let setMatrix4x4 shader (name:string) (value:Matrix4x4) =
  let location = shader.Gl.GetUniformLocation(shader.Handle, name)
  let matrixAsArray = [|
    value.M11 ; value.M12 ; value.M13 ; value.M14
    value.M21 ; value.M22 ; value.M23 ; value.M24
    value.M31 ; value.M32 ; value.M33 ; value.M34
    value.M41 ; value.M42 ; value.M43 ; value.M44
  |]
  //use ptr = fixed matrixAsArray
  let matrixAsSpan = ReadOnlySpan<float32>(matrixAsArray)
  useShader shader
  shader.Gl.UniformMatrix4 (location, 1u, false, matrixAsSpan)
  
let setVector3f shader (name:string) (value:Vector3) =
  let location = shader.Gl.GetUniformLocation(shader.Handle, name)
  useShader shader  
  shader.Gl.Uniform3(location, value.X, value.Y, value.Z)  
  
let dispose shader =
  shader.Gl.DeleteProgram shader.Handle
