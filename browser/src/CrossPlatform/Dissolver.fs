module App.Dissolver
open App.Model

let private random = System.Random().Next

let create (width:int) (height:int) (zoom:float) =
  let dissolveWidth = float width/zoom - zoom
  let dissolveHeight = float height/zoom - zoom
  Utils.log $"Dissolve size: {dissolveWidth},{dissolveHeight}"
  let coordinates =
    {0..int dissolveHeight+1}
    |> Seq.map(fun y ->
      {0..int dissolveWidth+1}
      |> Seq.map(fun x ->
        (x,y)
      )
    )
    |> Seq.concat
    |> Seq.sortBy(fun _ -> random(width*height))
    |> Seq.toList
  { RemainingPixels = coordinates ; DrawnPixels = [] ; PixelSize = zoom }