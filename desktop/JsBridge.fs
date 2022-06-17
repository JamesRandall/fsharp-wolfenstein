module Fable.Core.JS

open System

type ArrayBuffer =
  { Bytes: byte array }

type DataView =
  { Bytes: byte array }
  member this.getUint8 byteOffset =
    uint8 this.Bytes.[byteOffset] 
  member this.setUint8 (byteOffset:int,value:uint8) =
    this.Bytes.[byteOffset] <- value
  member this.getUint16 (byteOffset:int,littleEndian:bool) =
    BitConverter.ToUInt16(this.Bytes, byteOffset)
    //let byte1 = uint16 this.Bytes.[byteOffset]
    //let byte2 = uint16 this.Bytes.[byteOffset+1]
    //if littleEndian then byte2 <<< 8 ||| byte1 else byte1 <<< 8 ||| byte2
  member this.setUint16 (byteOffset:int,value:uint16,littleEndian:bool) =
    let bytes = BitConverter.GetBytes(value)
    this.Bytes.[byteOffset] <- bytes.[0]
    this.Bytes.[byteOffset+1] <- bytes.[1]
  member this.getUint32 (byteOffset:int,littleEndian:bool) =
    BitConverter.ToUInt32(this.Bytes, byteOffset)
  member this.byteLength = this.Bytes.Length
  
type _ArrayBuffer = ArrayBuffer
type _DataView = DataView
  
module Constructors =
  type ArrayBuffer =
    static member Create (size) = { Bytes = Array.zeroCreate<byte> size }:_ArrayBuffer
    static member CreateWithBytes bytes = { Bytes = bytes }:_ArrayBuffer
  
  type DataView =
    static member Create (arrayBuffer:_ArrayBuffer) = { Bytes = arrayBuffer.Bytes }:_DataView
    static member Create (arrayBuffer:_ArrayBuffer, offset:int, length:float) =
      { Bytes = arrayBuffer.Bytes[offset..(offset+int length-1)] }:_DataView