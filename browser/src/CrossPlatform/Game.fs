module App.Game

open App
open App.Model
open Update

let private initialGameState =
  { Map = []
    Player = {
      Score = 0<points>
      Health = 100<hp>
      Radius = 0.5
      Weapons = [  ]
      CurrentWeaponIndex = -1
      Ammunition = 99
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
  }

let init statusBarScale initScene = async {
  let! rawMap = Map.loadRawMap 0 //Map.loadLevel 0  
  let! graphics = GraphicsCommon.loadGraphics ()
  let! sprites = Graphics.loadSprites ()
  let! statusBarTextures = Graphics.loadStatusBar statusBarScale
  
  let drawScene,_,viewportHeight = initScene ()
  
  let gameLoop (game:Game) (frameTime:float<ms>) =
    let updatedGameState =
      drawScene statusBarTextures graphics sprites game
      |> updateFrame game frameTime
    updatedGameState
  let updateControlState game controlState =
    { game with ControlState = game.ControlState ^^^ controlState }
  
  let level = Map.loadLevelFromRawMap rawMap
  let gameState =
    { initialGameState with
        Map = level.Plane0
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
    }

  return gameLoop,updateControlState,gameState
}
  