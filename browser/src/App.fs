module App.StartupObject

open App.Model
open Browser.Dom
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop

let canvas = document.querySelector(".game-canvas") :?> Browser.Types.HTMLCanvasElement
let loadingImage =
  Image.Create(
    textureWidth,
    textureHeight,
    src = "/assets/loadingScreen.png"
  )
let mutable isShowingLoadingScreen = true
let drawImage _ =
  let context = canvas.getContext_2d()
  context?imageSmoothingEnabled <- false
  let widthRatio = canvas.width / loadingImage.width
  let heightRatio = canvas.height / loadingImage.height
  let aspectRatio = min widthRatio heightRatio
  
  let renderHeight = loadingImage.height * aspectRatio
  let renderWidth = loadingImage.width * aspectRatio
  let xPos = (canvas.width - renderWidth) / 2.
  let yPos = (canvas.height - renderHeight) / 2.
  context.drawImage(U3.Case1 loadingImage, xPos, yPos, renderWidth, renderHeight)
loadingImage.onload <- drawImage

let setCanvasSize _ =
  canvas.width <- window.innerWidth
  canvas.height <- window.innerHeight
window.onresize <- (fun _ ->
  setCanvasSize()
  if isShowingLoadingScreen then
    drawImage ()
)
setCanvasSize ()



let private controlStateFromKeyCode keyCode =
  match keyCode with
  | "ArrowUp" -> ControlState.Forward
  | "ArrowLeft" -> ControlState.TurningLeft
  | "ArrowRight" -> ControlState.TurningRight
  | "KeyQ" -> ControlState.StrafingLeft
  | "KeyE" -> ControlState.StrafingRight
  | "ArrowDown" -> ControlState.Backward
  | "ControlLeft" -> ControlState.Fire
  | "Space" -> ControlState.Action
  | "Digit1" -> ControlState.Weapon0
  | "Digit2" -> ControlState.Weapon1
  | "Digit3" -> ControlState.Weapon2
  | "Digit4" -> ControlState.Weapon3
  | _ -> ControlState.None


let initialisationPromise = (App.Game.init 1. (App.Scene.initScene canvas)) |> Async.StartAsPromise
initialisationPromise
  .``then``(fun (gameLoop,controlStateHandler,initialGameState) ->
    isShowingLoadingScreen <- false
    let mutable previousTimestamp = 0.
    let mutable currentGameState = initialGameState
    let rec wrappedGameLoop timestamp =
      let timeInFrame = min (1000./30.) (timestamp - previousTimestamp)
      previousTimestamp <- timestamp
      currentGameState <- gameLoop currentGameState (timeInFrame * 1.<ms>)
      window.requestAnimationFrame wrappedGameLoop |> ignore
    window.onkeydown <- (fun ke ->
      if not ke.repeat then
        let controlState = ke.code |> controlStateFromKeyCode
        if controlState <> ControlState.None then
          ke.preventDefault()
          currentGameState <- controlStateHandler currentGameState controlState
    )
    window.onkeyup <- (fun ke ->
      let controlState = ke.code |> controlStateFromKeyCode
      if controlState <> ControlState.None then
        ke.preventDefault()
        currentGameState <- controlStateHandler currentGameState controlState
    )
    window.requestAnimationFrame wrappedGameLoop |> ignore
  )
  .catch(fun exn -> console.error $"Error initialising engine {exn}")
  |> ignore

