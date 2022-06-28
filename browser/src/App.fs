module App.StartupObject

open App.Model
open Browser.Dom
open Fable.Core

let canvas = document.querySelector(".game-canvas") :?> Browser.Types.HTMLCanvasElement
let setCanvasSize _ =
  canvas.width <- window.innerWidth
  canvas.height <- window.innerHeight
window.onresize <- setCanvasSize
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
  | _ -> ControlState.None

let initialisationPromise = (App.Game.init 1. (App.Scene.initScene canvas)) |> Async.StartAsPromise
initialisationPromise
  .``then``(fun (gameLoop,controlStateHandler,initialGameState) ->
    let mutable previousTimestamp = 0.
    let mutable currentGameState = initialGameState
    let rec wrappedGameLoop timestamp =
      let timeInFrame = timestamp - previousTimestamp
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

