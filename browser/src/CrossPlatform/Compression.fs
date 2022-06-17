module App.Compression
open Fable.Core.JS

// assets are double compressed, these are the two decompression algorithms

let rlewDecode (createDataView:int->DataView) (mapHeadView:DataView) (inView:DataView) =
  let rlewTag = mapHeadView.getUint16(0, true);
  let size = inView.getUint16(0, true);
  let outView = createDataView (int size)
  
  List.unfold(fun (inOffset, outOffset) ->
    let nextInOffset, nextOutOffset =
      let w = inView.getUint16(inOffset, true)
      let newInOffset = inOffset + 2
      if w = rlewTag then
        let n = inView.getUint16(newInOffset, true)
        let x = inView.getUint16(newInOffset + 2, true)
        {0..(int n-1)}
        |> Seq.iter(fun i ->
          outView.setUint16(outOffset+i*2, x, true)
        )
        (newInOffset+4,outOffset+2*int n)
      else
        outView.setUint16(outOffset, w, true)
        newInOffset,outOffset+2
    if nextInOffset < inView.byteLength then ((),(nextInOffset, nextOutOffset)) |> Some else None
  ) (2,0) |> ignore
  
  outView
  
let carmackDecode (createDataView:int->DataView) (inView:DataView) =
  let size = inView.getUint16(0,true)
  let outView = createDataView (int size)
  let isNearPointer possiblePointer = possiblePointer = 0xA7uy
  let isPointer possiblePointer = possiblePointer |> isNearPointer || possiblePointer = 0xA8uy
  
  List.unfold(fun (inOffset,outOffset) ->
    let newInOffset,newOutOffset =
      // possibly a pointer
      let x = inView.getUint8(inOffset + 1)
      if x |> isPointer then
        let n = inView.getUint8(inOffset)
        if n = 0uy then
          // exception (not really a pointer)
          outView.setUint8(outOffset, inView.getUint8(inOffset + 2))
          outView.setUint8(outOffset + 1, x)
          (inOffset+3,outOffset+2)
        elif x |> isNearPointer then
          let offset = 2 * (inView.getUint8(inOffset + 2) |> int)
          let newOutOffset =
            {0..(int n-1)}
            |> Seq.fold(fun foldOffset _ ->
              outView.setUint16(foldOffset, outView.getUint16(foldOffset - offset, true), true)
              foldOffset + 2
            ) outOffset
          (inOffset+3,newOutOffset)
        else
          // far pointer
          let offset = 2 * int (inView.getUint16(inOffset + 2 |> int, true))
          let newOutOffset =
            {0..(int n-1)}
            |> Seq.fold (fun foldOffset i ->
              outView.setUint16(foldOffset, outView.getUint16(offset + 2 * i, true), true)
              foldOffset + 2
            ) outOffset
          (inOffset+4,newOutOffset)
      else
        outView.setUint16(outOffset, inView.getUint16(inOffset, true), true)
        (inOffset+2,outOffset+2)
    if newInOffset >= inView.byteLength then
      None
    else
      ((),(newInOffset,newOutOffset)) |> Some
  ) (2,0) |> ignore
  outView