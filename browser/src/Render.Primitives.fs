namespace App.Render

open Browser.Types
open Fable.Core

module Primitives =
  let toColorString color =
    let r = (color >>> 16) 
    let g= (color >>> 8) &&& 0xFF
    let b = color &&& 0xFF
    $"rgb({r},{g},{b})"
    
  let adjustColor color value =
    let r = (color >>> 16) + value;
    let b = ((color >>> 8) &&& 0x00FF) + value;
    let g = (color &&& 0x0000FF) + value;
    g ||| (b <<< 8) ||| (r <<< 16);

  let fill (context:CanvasRenderingContext2D) color left top width height =
    context.fillStyle <- (color |> toColorString |> U3.Case1)
    context.fillRect (left, top, width, height)
    
  let clearCanvas (context:CanvasRenderingContext2D) =
    fill context 0 -1. -1. (context.canvas.width+2.) (context.canvas.height+2.)
    
  let fillText (context:CanvasRenderingContext2D) text x y =
    context.textAlign <- "start"
    context.fillStyle <- ("#e0e0e0" |> U3.Case1)
    context.fillText (text,x,y)
  

