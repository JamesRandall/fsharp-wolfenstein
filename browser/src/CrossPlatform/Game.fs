module App.Game

open App
open App.Model
open Update

let private initialGameState =
  { Map = []
    Areas = []
    CompositeAreas = []
    Level = -1
    Player = {
      Lives = 3<life>
      Score = 0<points>
      Health = 100<hp>
      Radius = 0.5
      Weapons = [  ]
      CurrentWeaponIndex = -1
      Ammunition = 9<bullets>
      CurrentFaceIndex = 0
      TimeToFaceChangeMs = 1500.<ms>
    }
    Camera = {
      Position = { vX = 12. ; vY = 6. }
      Direction = { vX = -1. ; vY = 0. }
      Plane = { vX = 0. ; vY = 1. } // 0.666 }
      FieldOfView = 1.
    }
    ControlState = ControlState.None
    GameObjects = []
    IsFiring = false
    TimeToNextWeaponFrame = None
    Doors = []
    ViewportFilter = ViewportFilter.None
    PixelDissolver = None
  }

let init statusBarScale initScene = async {
  let! rawMap = Map.loadRawMap 0 //Map.loadLevel 0  
  let! graphics = GraphicsCommon.loadGraphics ()
  let! sprites = Graphics.loadSprites ()
  let! statusBarTextures = Graphics.loadStatusBar statusBarScale
  
  let drawScene,viewportWidth,viewportHeight = initScene ()
  
  let gameLoop (game:Game) (frameTime:float<ms>) =
    let updatedGameState =
      drawScene statusBarTextures graphics sprites game
      |> updateFrame game frameTime (fun _ -> Dissolver.create viewportWidth viewportHeight 2)
    updatedGameState
  let updateControlState game controlState =
    { game with ControlState = game.ControlState ^^^ controlState }
  
  let level = Map.loadLevelFromRawMap DifficultyLevel.IAmDeathIncarnate rawMap
  let gameState =
    { initialGameState with
        Level = 1
        Map = level.Map
        Areas = level.Areas
        CompositeAreas = {0..level.NumberOfAreas-1} |> Seq.map(fun i -> { Area = i ; ConnectedTo = [i] |> Set.ofList }) |> Seq.toList 
        Camera = level.PlayerStartingPosition
        GameObjects = level.GameObjects
        Player = {
          initialGameState.Player with
            Weapons = [
              Map.getWeapon sprites WeaponType.Knife (Graphics.scaleSprite (float viewportHeight) (float viewportHeight))
              Map.getWeapon sprites WeaponType.Pistol (Graphics.scaleSprite (float viewportHeight) (float viewportHeight))
            ]
            CurrentWeaponIndex = 1
        }
        Doors = level.Doors
        //PixelDissolver = Dissolver.create viewportWidth viewportHeight 2 |> Some
    }

  return gameLoop,updateControlState,gameState
}
  