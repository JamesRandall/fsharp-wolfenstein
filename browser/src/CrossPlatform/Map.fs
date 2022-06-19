module App.Map

open App.Model
open Fable.Core.JS
open Compression
open App.PlatformModel

exception MapLoadException of string
exception PlayerStartingPositionNotFoundException
exception InvalidDoorException

let mapFromTextures source =
  source
  |> List.map(fun row ->
    row |> List.map(fun textureIndex ->
      if textureIndex > 0 then
        Cell.Wall { NorthSouthTextureIndex = textureIndex ; EastWestTextureIndex = textureIndex }
      else
        Cell.Empty
    )  
  )
  
let getPlaneValue (plane:DataView) x y =
  plane.getUint16(2 * (x + 64 * y), true)
  
let private traverseMap handler (mapPlane:DataView) =
  let mapSize = int (sqrt (float (mapPlane.byteLength/2))) 
  {0..(mapSize-1)}
  |> Seq.map(fun rowIndex ->
    {0..(mapSize-1)}
    |> Seq.map (fun colIndex ->
      let rawMapCell = getPlaneValue mapPlane colIndex rowIndex
      handler colIndex rowIndex rawMapCell
    )
    |> Seq.filter Option.isSome
    |> Seq.map (fun v -> v.Value)
  )
  |> Seq.concat
  
let private getStartingPosition mapSize plane1 =
  let cameraOption =
    {0..mapSize}
    |> Seq.fold(fun outerCameraOption row ->
      {0..mapSize}
      |> Seq.fold(fun innerCameraOption column ->
        match innerCameraOption with
        | Some _ -> innerCameraOption
        | None ->
          let mapValue = getPlaneValue plane1 column row
          if 19us <= mapValue && mapValue <= 22us then
            let direction =
              match mapValue with
              | 19us -> { vX = 0. ; vY = -1. }
              | 20us -> { vX = -1. ; vY = 0. }
              | 21us -> { vX = 0. ; vY = 1. }
              | 22us | _ -> { vX = 1. ; vY = 0. }
            let fieldOfView = 1.
            let startingPosition = { vX = float mapSize - float column - 0.5 ; vY = float row + 0.5 }
            
            // A diagnostic while working on patrolling - puts you in a room facing a guard who is patrolling
            //let startingPosition = { vX = float mapSize - float 38 - 0.5 ; vY = float 36 + 0.5 }
            //let direction = Direction.north
            // A diagnostic for patrolling - puts you in a room with dogs and complex patrol paths
            //let startingPosition = { vX = float mapSize - float 51 - 0.5 ; vY = float 44 + 0.5 }
            //let direction = Direction.east
            
            { Position = startingPosition
              Direction = direction
              Plane = direction.CrossProduct * fieldOfView
              FieldOfView = fieldOfView
            } |> Some
          else
            None
      ) outerCameraOption
    ) None
  match cameraOption with | Some camera -> camera | _ -> raise PlayerStartingPositionNotFoundException
  
let loadRawMap levelIndex = async {
  let createDataView size = Constructors.DataView.Create(Constructors.ArrayBuffer.Create(int size))
  let! gameMapsResult = Utils.loadAsset Assets.GAMEMAPS
  let! mapHeadResult = Utils.loadAsset Assets.MAPHEAD
  return
    match gameMapsResult,mapHeadResult with
    | Ok gameMaps, Ok mapHead ->
      let mapHeadView = Constructors.DataView.Create mapHead 
      let offset = mapHeadView.getUint32(2 + 4 * levelIndex, true)
      let mapHeader = Constructors.DataView.Create(gameMaps, int offset, 42.)
      
      let plane0View =
        Constructors.DataView.Create (
          gameMaps, mapHeader.getUint32(0, true) |> int, mapHeader.getUint16(12, true) |> float
        )  
      let plane1View =
        Constructors.DataView.Create (
          gameMaps, mapHeader.getUint32(4, true) |> int, mapHeader.getUint16(14, true) |> float
        )
      let plane2View =
        Constructors.DataView.Create (
          gameMaps, mapHeader.getUint32(4, true) |> int, mapHeader.getUint16(14, true) |> float
        )
      let plane0= plane0View |> carmackDecode createDataView |> rlewDecode createDataView mapHeadView
      let plane1 = plane1View |> carmackDecode createDataView |> rlewDecode createDataView mapHeadView
      let plane2 = plane2View |> carmackDecode createDataView |> rlewDecode createDataView mapHeadView
      let mapSize = 64
      
      { MapSize = mapSize
        Plane0 = plane0
        Plane1 = plane1
        Plane2 = plane2
      }
    | _ -> raise (MapLoadException "Unexpected error")
}

let loadLevelFromRawMap (raw:RawMap) =          
  let plane0,doors =
    {0..(raw.MapSize-1)}
    |> Seq.fold(fun (rows,outerDoors) rowIndex ->
      let createdRow,rowDoors =
        {0..(raw.MapSize-1)}
        |> Seq.fold(fun (row,innerDoors:DoorState list) colIndex ->
          let rawMapCell = raw.Plane0.getUint16(2 * (colIndex + 64 * rowIndex), true)
          if rawMapCell <= 63us then
            let textureIndex = ((2*(int rawMapCell))-1)
            row @ [
              Cell.Wall { NorthSouthTextureIndex = textureIndex ; EastWestTextureIndex = (textureIndex-1) }
            ],innerDoors
          elif rawMapCell >= 90us && rawMapCell <= 101us then
            let spriteIndex,direction =
              if rawMapCell = 90us then 99,DoorDirection.NorthSouth
              elif rawMapCell = 92us then 105,DoorDirection.NorthSouth
              elif rawMapCell = 94us then 105,DoorDirection.NorthSouth
              elif rawMapCell = 100us then 103,DoorDirection.NorthSouth
              elif rawMapCell = 91us then 98,DoorDirection.EastWest
              elif rawMapCell = 93us then 104,DoorDirection.EastWest
              elif rawMapCell = 95us then 104,DoorDirection.EastWest
              elif rawMapCell = 101us then 102,DoorDirection.EastWest
              else raise InvalidDoorException
            let doorState =
              { TextureIndex = spriteIndex
                DoorDirection = direction
                Status = DoorStatus.Closed
                Offset = 0.
                TimeRemainingInAnimationState = 0.<ms>
                MapPosition = colIndex,rowIndex
              }
            
            row @ [Cell.Door innerDoors.Length],(innerDoors @ [doorState])
          else
            // a turning point is represented as a game object but we encode it into the cells as its more convenient
            // to lookup within the game loop this way
            let objectValue = getPlaneValue raw.Plane1 colIndex rowIndex
            let cell =
              if objectValue = 0x5Aus then
                Cell.TurningPoint MapDirection.East
              elif objectValue = 0x5Bus then
                Cell.TurningPoint MapDirection.NorthEast
              elif objectValue = 0x5Cus then
                Cell.TurningPoint MapDirection.North
              elif objectValue = 0x5Dus then
                Cell.TurningPoint MapDirection.NorthWest
              elif objectValue = 0x5Eus then
                Cell.TurningPoint MapDirection.West
              elif objectValue = 0x5Fus then
                Cell.TurningPoint MapDirection.SouthWest
              elif objectValue = 0x60us then
                Cell.TurningPoint MapDirection.South
              elif objectValue = 0x61us then
                Cell.TurningPoint MapDirection.SouthEast
              else
                Cell.Empty
            row @ [cell], innerDoors
        ) ([],outerDoors) 
      rows @ [createdRow],rowDoors
    ) ([], [])
  let patchedPlane0 =
    plane0 |> List.mapi(fun rowIndex row ->
      row |> List.mapi(fun colIndex cell ->
        match cell with
        | Cell.Wall wall ->
          let hasNorthSouthDoor =
            doors
            |> List.tryFind(fun doorState -> doorState.MapPosition = (colIndex+1,rowIndex))
            |> Option.isSome
            ||
            doors
            |> List.tryFind(fun doorState -> doorState.MapPosition = (colIndex-1,rowIndex))
            |> Option.isSome
          let hasEastWestDoor =
            doors
            |> List.tryFind(fun doorState -> doorState.MapPosition = (colIndex,rowIndex+1))
            |> Option.isSome
            ||
            doors
            |> List.tryFind(fun doorState -> doorState.MapPosition = (colIndex,rowIndex-1))
            |> Option.isSome
          { wall with
              EastWestTextureIndex = if hasEastWestDoor then 101 else wall.EastWestTextureIndex
              NorthSouthTextureIndex = if hasNorthSouthDoor then 100 else wall.NorthSouthTextureIndex
          }
          |> Cell.Wall
        | _ -> cell
      ) 
    )
  let playerStartingPosition = getStartingPosition raw.MapSize raw.Plane1
      
  let startingDirectionVectorFromInt direction =
    match direction with
    | 0us -> { vX = -1. ; vY = 0. } // east - but we have to flip horizontal later so west here
    | 1us -> { vX = 0. ; vY = -1. } // moving north
    | 2us -> { vX = 1. ; vY = 0. } // west - but we have to flip horizontal later so east here
    | 3us -> { vX = 0. ; vY = 1. } // moving south
    | _ -> raise (MapLoadException $"Direction of {direction} int not between 0 and 3")
    
  let startingMapDirectionFromInt directionOption =
    match directionOption with
    | Some direction ->
      match direction with
      | 0us -> MapDirection.East
      | 1us -> MapDirection.North
      | 2us -> MapDirection.West
      | 3us -> MapDirection.South
      | _ -> raise (MapLoadException $"Direction of {direction} int not between 0 and 3")
    | None -> MapDirection.None
    
  
  let standingOrMoving baseValue value =
    if value-baseValue < 4us then EnemyStateType.Standing else EnemyStateType.Path
  
  let gameObjects =
    let createEnemy spriteIndex spriteBlocks framesPerBlock deathSprites attackingSprites enemyType x y directionIntOption startingState =
      let position = { vX = float raw.MapSize - float x - 0.5 ; vY = float y + 0.5 }
      { EnemyType = enemyType
        BasicGameObject = {
          Position = position
          SpriteIndex = spriteIndex
          PlayerRelativePosition =  position - playerStartingPosition.Position
          UnsquaredDistanceFromPlayer = position.UnsquaredDistanceFrom playerStartingPosition.Position
          CollidesWithBullets = true
        }
        Direction = directionIntOption |> startingMapDirectionFromInt
        //DirectionVector = directionIntOption |> Option.map startingDirectionVectorFromInt
        State = startingState
        DeathSpriteIndexes = deathSprites
        AttackSpriteIndexes = attackingSprites
        SpriteBlocks = spriteBlocks
        CurrentAnimationFrame = 0
        FramesPerBlock = framesPerBlock
        TimeUntilNextAnimationFrame = Enemy.AnimationTimeForState startingState
      }
    let guardEnemy = createEnemy 50 4 8 [90 ; 91 ; 92 ; 93 ; 95] [96 ; 97 ; 98] EnemyType.Guard
    let dogEnemy = createEnemy 99 3 8 [131 ; 132 ; 133 ; 134] [135 ; 136 ; 137] EnemyType.Dog
    let officerEnemy = createEnemy 138 4 8 [179 ; 180 ; 181 ; 183] [184 ; 185 ; 186] EnemyType.SS
    let zombieEnemy = createEnemy 187 4 8 [228 ; 229 ; 230 ; 232 ; 233] [234 ; 235 ; 236 ; 237] EnemyType.Zombie
    let leonEnemy = createEnemy 238 4 8 [279 ; 280 ; 281 ; 283 ; 284] [285 ; 286 ; 287] EnemyType.Officer
    let hansEnemy = createEnemy 296 1 4 [304 ; 305 ; 306 ; 303] [300; 301 ; 302] EnemyType.Hans
    let schabbsEnemy = createEnemy 307 1 4 [313 ; 314 ; 315 ; 316] [311 ; 312]  EnemyType.Schabbs
    let fakeAdolfEnemy = createEnemy 321 1 4 [328 ; 329 ; 330 ; 331 ; 332 ; 333] [] EnemyType.FakeAdolf
    // Note Hitler has two states - robot adolf and plain adolf, this only deals with plain hitler
    let adolfEnemy = createEnemy 345 1 4 [353 ; 354 ; 355 ; 356 ; 357 ; 358 ; 359 ; 352] [349 ; 350 ; 351] EnemyType.Adolf 
    let ottoEnemy = createEnemy 360 1 4 [366 ; 367 ; 368 ; 369] [364 ; 365] EnemyType.Otto
    let gretelEnemy = createEnemy 385 1 4 [393 ; 394 ; 395 ; 392] [389 ; 390 ; 391] EnemyType.Gretel
    let fettgesichtEnemy = createEnemy 396 1 4 [404 ; 405 ; 406 ; 407] [400 ; 401 ; 402 ; 403] EnemyType.Fettgesicht
    
    raw.Plane1
    |> traverseMap (fun colIndex rowIndex value ->
        // this requires some additional nuance as their are different kinds of collectible
        if value >= 23us && value <= 70us then
          let position = { vX = float raw.MapSize - float colIndex - 0.5 ; vY = float rowIndex + 0.5 }
          { Position = position
            SpriteIndex = (int value) - 21
            PlayerRelativePosition =  position - playerStartingPosition.Position
            UnsquaredDistanceFromPlayer = position.UnsquaredDistanceFrom playerStartingPosition.Position
            CollidesWithBullets = false
          } |> GameObject.Treasure |> Some
        elif value >= 108us then
          // useful for debugging if you want a single enemy
          // find their location in maped42 and enter the co-ordinates below 
          //if colIndex = 59 && rowIndex = 44 then
            let enemy =
              // guards
              // TODO: tidy this up, crying out for a pipeline
              if 108us <= value && value < 116us then
                guardEnemy colIndex rowIndex (Some ((value-108us) % 4us)) (value |> standingOrMoving 108us) |> Some // level 1
              elif 144us <= value && value < 152us then
                guardEnemy colIndex rowIndex (Some ((value-144us) % 4us)) (value |> standingOrMoving 144us) |> Some // level 3
              elif 180us <= value && value < 188us then
                guardEnemy colIndex rowIndex (Some ((value-180us) % 4us)) (value |> standingOrMoving 180us) |> Some // level 4
              elif 116us <= value && value < 124us then
                leonEnemy colIndex rowIndex (Some ((value-116us) % 4us)) (value |> standingOrMoving 116us) |> Some // level 1
              elif 152us <= value && value < 160us then
                leonEnemy colIndex rowIndex (Some ((value-152us) % 4us)) (value |> standingOrMoving 152us) |> Some // level 3
              elif 188us <= value && value < 196us then
                leonEnemy colIndex rowIndex (Some ((value-188us) % 4us)) (value |> standingOrMoving 188us) |> Some // level 3
              elif 126us <= value && value < 134us then
                officerEnemy colIndex rowIndex (Some ((value-126us) % 4us)) (value |> standingOrMoving 126us) |> Some // level 1
              elif 162us <= value && value < 170us then
                officerEnemy colIndex rowIndex (Some ((value-162us) % 4us)) (value |> standingOrMoving 162us) |> Some // level 3
              elif 198us <= value && value < 206us then
                officerEnemy colIndex rowIndex (Some ((value-198us) % 4us)) (value |> standingOrMoving 198us) |> Some // level 4
                
              elif 124us = value then
                let deadGuard = guardEnemy colIndex rowIndex (Some ((value-108us) % 4us)) EnemyStateType.Dead 
                { deadGuard with CurrentAnimationFrame = deadGuard.DeathSpriteIndexes.Length-1 } |> Some
                
              elif 138us <= value && value < 142us then
                dogEnemy colIndex rowIndex (Some ((value-138us) % 4us)) EnemyStateType.Path |> Some
              elif 174us <= value && value < 178us then
                dogEnemy colIndex rowIndex (Some ((value-174us) % 4us)) EnemyStateType.Path |> Some
              elif 210us <= value && value < 214us then
                dogEnemy colIndex rowIndex (Some ((value-210us) % 4us)) EnemyStateType.Path |> Some
                
              elif 216us <= value && value < 224us then
                zombieEnemy colIndex rowIndex (Some ((value-216us) % 4us)) (value |> standingOrMoving 216us) |> Some
              elif 234us <= value && value < 242us then
                zombieEnemy colIndex rowIndex (Some ((value-234us) % 4us)) (value |> standingOrMoving 234us) |> Some
              elif value = 160us then
                fakeAdolfEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              elif value = 178us then
                adolfEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              elif value = 179us then
                fettgesichtEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              elif value = 196us then
                schabbsEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              elif value = 197us then
                gretelEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              elif value = 214us then
                hansEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              elif value = 215us then
                ottoEnemy colIndex rowIndex None EnemyStateType.Ambushing |> Some
              else
                None
            enemy |> Option.map (fun e -> e |> GameObject.Enemy)
          //else
          //  None
        else
          None
    )
    |> Seq.toList
    
  { Width = raw.MapSize
    Height = raw.MapSize
    Plane0 = patchedPlane0 |> List.map List.rev   
    PlayerStartingPosition = playerStartingPosition //|> reverseCamera
    GameObjects = gameObjects
    // This will only include the nearest enemy as a game object - useful for testing sometimes
    (*GameObjects =
      gameObjects
      |> List.sortBy(fun go -> go.BasicGameObject.UnsquaredDistanceFromPlayer)
      |> List.filter(fun go -> match go with GameObject.Enemy _ -> true | _ -> false) |> List.take 1*)
    Doors = doors
  }
  
let getWeapon (sprites:Texture array) weaponType scaleSprite =
  //let toSpriteSet sequence = sequence |> Seq.map (fun i -> Graphics.canvasFromTexture sprites.[i]) |> Seq.toList
  let toSpriteSet sequence =
    sequence
    |> Seq.map (fun i ->
      sprites.[i] |> scaleSprite 
    ) |> Seq.toList
  match weaponType with
  | WeaponType.Knife ->
    { Sprites = {416..420} |> toSpriteSet
      CurrentFrame = 0
      Damage = 25
      AutoRepeat = false
    }
  | WeaponType.Pistol ->
    { Sprites = ({421..425} |> toSpriteSet) @ ({424..422} |> toSpriteSet)
      CurrentFrame = 0
      Damage = 25
      AutoRepeat = false
    }
  | WeaponType.MachineGun ->
    { Sprites = {426..430} |> toSpriteSet
      CurrentFrame = 0
      Damage = 25
      AutoRepeat = true
    }
  | WeaponType.ChainGun ->
    { Sprites = {431..435} |> toSpriteSet
      CurrentFrame = 0
      Damage = 25
      AutoRepeat = true
    }

(* | WeaponType.Knife -> { SpriteIndex = 416 ; Ammunition = System.Int32.MaxValue ; CurrentFrame = 0 ; Damage = 25 }
  | WeaponType.Pistol -> { SpriteIndex = 421 ; Ammunition = 99 ; CurrentFrame = 0 ; Damage = 25 }
  | WeaponType.MachineGun -> { SpriteIndex = 426 ; Ammunition = 99 ; CurrentFrame = 0 ; Damage = 25 }
  | WeaponType.ChainGun -> { SpriteIndex = 431 ; Ammunition = 99 ; CurrentFrame = 0 ; Damage = 25 }) *)

