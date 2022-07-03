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
    ResetLevel = (fun g _ -> g)
  }

let init statusBarScale initScene = async {
  let startingLevel = 0
  
  let! graphics = GraphicsCommon.loadGraphics ()
  let! sprites = Graphics.loadSprites ()
  let! statusBarTextures = Graphics.loadStatusBar statusBarScale
  
  let! rawMap0 = Map.loadRawMap 0
  let! rawMap1 = Map.loadRawMap 1
  let! rawMap2 = Map.loadRawMap 2
  let! rawMap3 = Map.loadRawMap 3
  let! rawMap4 = Map.loadRawMap 4
  let! rawMap5 = Map.loadRawMap 5
  let! rawMap6 = Map.loadRawMap 6
  let! rawMap7 = Map.loadRawMap 7
  let! rawMap8 = Map.loadRawMap 8
  let! rawMap9 = Map.loadRawMap 9 // secret level!
  let levels = [
    rawMap0 ; rawMap1 ; rawMap2 ; rawMap3 ; rawMap4
    rawMap5 ; rawMap6 ; rawMap7 ; rawMap8 ; rawMap9
  ]
  
  let drawScene,viewportWidth,viewportHeight = initScene ()
  
  let nextLevel (inputGame:Game) =
    // TODO: stop the player going past the last level - we need to do the victory thing
    let newLevel = Map.loadLevelFromRawMap DifficultyLevel.IAmDeathIncarnate levels.[inputGame.Level + 1]
    { inputGame with
        Map = newLevel.Map
        Areas = newLevel.Areas
        Level = inputGame.Level + 1
        CompositeAreas = {0..newLevel.NumberOfAreas-1} |> Seq.map(fun i -> { Area = i ; ConnectedTo = [i] |> Set.ofList }) |> Seq.toList 
        Camera = newLevel.PlayerStartingPosition
        GameObjects = newLevel.GameObjects
        Doors = newLevel.Doors
    }
  
  let gameLoop (game:Game) (frameTime:float<ms>) =
    let updatedGameState =
      drawScene statusBarTextures graphics sprites game
      |> updateFrame game nextLevel frameTime (fun _ -> Dissolver.create viewportWidth viewportHeight 2)
    updatedGameState
  let updateControlState game controlState =
    { game with ControlState = game.ControlState ^^^ controlState }
  
  let level = Map.loadLevelFromRawMap DifficultyLevel.IAmDeathIncarnate levels.[startingLevel]
  let resetLevel game player =
    { game with
        Map = level.Map
        Areas = level.Areas
        Level = startingLevel
        CompositeAreas = {0..level.NumberOfAreas-1} |> Seq.map(fun i -> { Area = i ; ConnectedTo = [i] |> Set.ofList }) |> Seq.toList 
        Camera = level.PlayerStartingPosition
        GameObjects = level.GameObjects
        Player = {
          player with
            Weapons = [
              Map.getWeapon sprites WeaponType.Knife (Graphics.scaleSprite (float viewportHeight) (float viewportHeight))
              Map.getWeapon sprites WeaponType.Pistol (Graphics.scaleSprite (float viewportHeight) (float viewportHeight))
            ]
            CurrentWeaponIndex = 1
            Health = 100<hp>
            Ammunition = 9<bullets>
        }
        Doors = level.Doors
    }
  let gameState = { (resetLevel initialGameState initialGameState.Player) with ResetLevel = resetLevel ; Level = 1 }

  return gameLoop,updateControlState,gameState
}
  