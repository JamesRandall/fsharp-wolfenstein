module App.Update

open App.Audio
open App.Model
open App.AI

let private weaponAnimationFrameTime = 100.<ms>
let private actionDistanceTolerance = 0.75
let private doorOpeningTime = 1000.<ms>
let private doorOpenTime = 5000.<ms>

let updateFrame game frameTime (renderingResult:WallRenderingResult) =
  let (|IsActive|_|) controlState game = if game.ControlState &&& controlState > ControlState.None then Some () else None 
  let frameMultiplier = float frameTime / 1000. 
  let movementSpeed = 6.0 * frameMultiplier // squares per second
  let rotationSpeed = 4.0 * frameMultiplier // radians per second
  let posX = game.Camera.Position.vX
  let posY = game.Camera.Position.vY
  let dirX = game.Camera.Direction.vX
  let dirY = game.Camera.Direction.vY
  let move speed inputGame =
    let canTraverse cell =
      match cell with
      | Cell.Door doorIndex ->
        let doorState = game.Doors.[doorIndex]
        match doorState.Status with
        | DoorStatus.Open
        | DoorStatus.Opening -> true
        | _ -> false
      | Cell.Wall _ -> false
      | _ -> true
    
    let newCameraPosition =
      { inputGame.Camera.Position with
          vX = if game.Map.[int posY].[int (posX + dirX * speed)] |> canTraverse then posX + (dirX * speed) else posX 
          vY = if game.Map.[int (posY + dirY * speed)].[int posX] |> canTraverse then posY + (dirY * speed) else posY
      }
    { inputGame with Camera = { inputGame.Camera with Position = newCameraPosition } }
  let strafe speed inputGame =
    let strafeDirection = game.Camera.Direction.CrossProduct
    let strafeX = strafeDirection.vX
    let strafeY = strafeDirection.vY
    let newCameraPosition =
      { inputGame.Camera.Position with
          vX = if game.Map.[int posY].[int (posX + strafeX * speed)] = Cell.Empty then posX + (strafeX * speed) else posX 
          vY = if game.Map.[int (posY + strafeY * speed)].[int posX] = Cell.Empty then posY + (strafeY * speed) else posY
      }
    { inputGame with Camera = { inputGame.Camera with Position = newCameraPosition } }
  let rotate inputGame =
    let rotationMultiplier =
      match inputGame with
      | IsActive ControlState.TurningRight -> -1.
      | IsActive ControlState.TurningLeft -> 1.
      | _ -> 0.
    let planeX = game.Camera.Plane.vX
    let planeY = game.Camera.Plane.vY
    let newDirX = dirX * cos(rotationMultiplier * rotationSpeed) - dirY * sin(rotationMultiplier * rotationSpeed)
    let newDirY = dirX * sin(rotationMultiplier * rotationSpeed) + dirY * cos(rotationMultiplier * rotationSpeed)
    let newPlaneX = planeX * cos(rotationMultiplier * rotationSpeed) - planeY * sin(rotationMultiplier * rotationSpeed)
    let newPlaneY = planeX * sin(rotationMultiplier * rotationSpeed) + planeY * cos(rotationMultiplier * rotationSpeed)
    { inputGame with
        Camera =
          { inputGame.Camera with
              Direction = { vX = newDirX  ; vY = newDirY}
              Plane = { vX = newPlaneX ;  vY = newPlaneY }
          }
    }
    
  let calculateRelativeGameObjectPosition (game:Game) (go:GameObject) =    
    go.UpdateBasicGameObject
      { go.BasicGameObject with
          UnsquaredDistanceFromPlayer = game.Camera.Position.UnsquaredDistanceFrom go.BasicGameObject.Position
          PlayerRelativePosition = go.BasicGameObject.Position - game.Camera.Position
      }
   
  let tryOpenDoor (doorState:DoorState) =
    match doorState.Status with
    | DoorStatus.Closed ->
      playSoundEffect SoundEffect.DoorOpen
      { doorState with Status = DoorStatus.Opening ; TimeRemainingInAnimationState = doorOpeningTime }
    | _ -> doorState
       
  let handleAction (game:Game) =
    if renderingResult.DistanceToWallInFrontOfPlayer <= actionDistanceTolerance &&
       game.ControlState &&& ControlState.Action > ControlState.None then
      let actionWallX, actionWallY = renderingResult.WallInFrontOfPlayer
      match game.Map.[actionWallY].[actionWallX] with
      | Cell.Door doorIndex ->
        let newDoorState = tryOpenDoor game.Doors.[doorIndex] 
        { game with
            Doors = game.Doors |> List.mapi(fun i d -> if i = doorIndex then newDoorState else d)
        }
      | _ -> game
    else
      game
      
  let updateTransitioningDoors game =
    let newDoors =
      game.Doors
      |> List.map(fun doorState ->
        let newTimeRemainingInAnimationState = doorState.TimeRemainingInAnimationState - frameTime
        match doorState.Status with
        | DoorStatus.Opening ->
          if newTimeRemainingInAnimationState < 0.<ms> then
            { doorState with Status = DoorStatus.Open ; TimeRemainingInAnimationState = doorOpenTime ; Offset = 64 }
          else
            let newOffset = (doorOpeningTime - newTimeRemainingInAnimationState) / doorOpeningTime * 64.
            { doorState with TimeRemainingInAnimationState = newTimeRemainingInAnimationState ; Offset = newOffset }
        | DoorStatus.Open ->
          if newTimeRemainingInAnimationState < 0.<ms> then
            playSoundEffect SoundEffect.DoorClose
            { doorState with Status = DoorStatus.Closing ; TimeRemainingInAnimationState = doorOpeningTime }
          else
            { doorState with TimeRemainingInAnimationState = newTimeRemainingInAnimationState }
        | DoorStatus.Closing ->
          if newTimeRemainingInAnimationState < 0.<ms> then
            { doorState with Status = DoorStatus.Closed ; TimeRemainingInAnimationState = doorOpeningTime ; Offset = 0. }
          else
            let newOffset = 64. - ((doorOpeningTime - newTimeRemainingInAnimationState) / doorOpeningTime * 64.)
            { doorState with TimeRemainingInAnimationState = newTimeRemainingInAnimationState ; Offset = newOffset }
        | _ -> doorState
      )
    { game with Doors = newDoors }
    
    
  let handleFiring game =
    let isFireKeyPressed = int (game.ControlState &&& ControlState.Fire) > 0 
    
    let updatePlayerWithWeapon weapon =
      { game.Player with
          Weapons = game.Player.Weapons |> List.mapi(fun i wp -> if i = game.Player.CurrentWeaponIndex then weapon else wp)
      }
    let currentWeapon = game.Player.Weapons.[game.Player.CurrentWeaponIndex]
    match game.IsFiring, game.TimeToNextWeaponFrame, isFireKeyPressed with
    | false, None, true ->
      let updatedWeapon = { currentWeapon with CurrentFrame = 1 }
      
      // begin firing
      playSoundEffect SoundEffect.PlayerPistol
      { game with
          IsFiring = true
          TimeToNextWeaponFrame = Some weaponAnimationFrameTime
          Player = updatedWeapon |> updatePlayerWithWeapon
      },true
    | true, Some timeInMs, _ ->
      let newTimeRemaining = timeInMs - frameTime
      if newTimeRemaining < 0.<ms> then
        if currentWeapon.CurrentFrame+1 < currentWeapon.AnimationFrames then
          let updatedWeapon = { currentWeapon with CurrentFrame = currentWeapon.CurrentFrame+1 }
          { game with
              TimeToNextWeaponFrame = Some (weaponAnimationFrameTime + newTimeRemaining)
              Player = updatedWeapon |> updatePlayerWithWeapon
          },false
        else
          let updatedWeapon = { currentWeapon with CurrentFrame = 0 }
          { game with
                IsFiring = false
                Player = updatedWeapon |> updatePlayerWithWeapon
                TimeToNextWeaponFrame = None
            },false
      else
        { game with TimeToNextWeaponFrame = Some newTimeRemaining },false
    | true, None, _ ->
      game,false
    | _ -> game,false
    
  let updateEnemyBasedOnPlayerFiring beganFiringSequenceOnFrame indexOfGameObject gameObject =
    match gameObject with
    | GameObject.Enemy e ->
      let updatedEnemy =
        if e.IsAlive then
          match renderingResult.SpriteInFrontOfPlayerIndexOption,beganFiringSequenceOnFrame with
          | Some firingHitGameObjectIndex, true ->
            if firingHitGameObjectIndex = indexOfGameObject then
              playRandomEnemyDeathSoundEffectAtVolume 1.0
              // begin the death sequence
              { e with
                  State = EnemyStateType.Die
                  CurrentAnimationFrame = 0
                  TimeUntilNextAnimationFrame = Enemy.AnimationTimeForState EnemyStateType.Die
                  BasicGameObject = { e.BasicGameObject with CollidesWithBullets = false }
              }
            else
              e
          | _ -> e
        else
          e
      GameObject.Enemy updatedEnemy
    | GameObject.Treasure t -> GameObject.Treasure t
    
  let updateEnemyAnimation frameTime gameObject =
    match gameObject with
    | GameObject.Enemy enemy ->
      let timeRemainingInAnimationFrame = enemy.TimeUntilNextAnimationFrame-frameTime
      match enemy.State with
      | EnemyStateType.Attack
      | EnemyStateType.Dead
      | EnemyStateType.Die ->
        if enemy.CurrentAnimationFrame < enemy.AnimationFrames-1 then
          if timeRemainingInAnimationFrame < 0.<ms> then
            { enemy with
                CurrentAnimationFrame = enemy.CurrentAnimationFrame+1
                TimeUntilNextAnimationFrame = (Enemy.AnimationTimeForState enemy.State) + timeRemainingInAnimationFrame } |> GameObject.Enemy
          else
            { enemy with TimeUntilNextAnimationFrame = timeRemainingInAnimationFrame } |> GameObject.Enemy
        else
          match enemy.State with
          | EnemyStateType.Attack ->
            { enemy with TimeUntilNextAnimationFrame = timeRemainingInAnimationFrame } |> GameObject.Enemy
          | _ -> enemy |> GameObject.Enemy
      | EnemyStateType.Chase _
      | EnemyStateType.Path ->
        if timeRemainingInAnimationFrame < 0.<ms> then
          let nextFrame =
            if enemy.CurrentAnimationFrame + 1 > enemy.NumberOfMovementAnimationFrames then 0 else enemy.CurrentAnimationFrame + 1
          let timeUntilNextFrame = (Enemy.AnimationTimeForState enemy.State) + timeRemainingInAnimationFrame
          { enemy with CurrentAnimationFrame = nextFrame ; TimeUntilNextAnimationFrame = timeUntilNextFrame  } |> GameObject.Enemy
        else
          { enemy with TimeUntilNextAnimationFrame = timeRemainingInAnimationFrame } |> GameObject.Enemy
      | _ -> enemy |> GameObject.Enemy
    | _ -> gameObject
    
  let openDoorsInRangeOfEnemies game =
    let rangeToOpenDoorsAt = 1.5
    game.GameObjects
    |> List.fold(fun (innerGame:Game) gameObject ->
      match gameObject with
      | GameObject.Enemy enemy ->
        match enemy.EnemyType with
        | EnemyType.Dog -> innerGame // dogs cannot open doors (well mine can, but game dogs can't!)
        | _ ->
          match enemy.State,enemy.DirectionVector with
          | EnemyStateType.Path, Some direction
          | EnemyStateType.Chase _, Some direction ->
            let maxDistanceToCheck = direction.Normalize().Abs()*rangeToOpenDoorsAt
            let setup () = false, enemy.BasicGameObject.Position.vX, enemy.BasicGameObject.Position.vY, direction
            let terminator (isHit, currentRayDistanceX, currentRayDistanceY, mapX, mapY, _) =
              (not isHit) &&
              (mapX >= 0 && mapX < game.Map.[0].Length && mapY >= 0 && mapY < game.Map.Length) &&
              (abs currentRayDistanceX < maxDistanceToCheck.vX || abs currentRayDistanceY < maxDistanceToCheck.vY)
            let isHit, _, _, _, _, hitMapX, hitMapY, _ = Ray.cast setup terminator game
            if isHit then
              match game.Map.[hitMapY].[hitMapX] with
              | Cell.Door doorIndex ->
                { innerGame with
                    Doors =
                      innerGame.Doors
                      |> List.mapi (fun i d -> if i = doorIndex then tryOpenDoor innerGame.Doors.[i] else d)
                }
              | _ -> innerGame
            else
              innerGame
          | _ -> innerGame
      | _ -> innerGame
    ) game
    
    
  let updateEnemies (game:Game,beganFiringSequenceOnFrame) =
    let updatedGameObjects =
      game.GameObjects
      |> List.mapi(fun i go ->
        go
        |> updateEnemyBasedOnPlayerFiring beganFiringSequenceOnFrame i
        |> updateEnemyAnimation frameTime 
        |> applyAi frameTime game
        |> calculateRelativeGameObjectPosition game
      )
    { game with GameObjects = updatedGameObjects } |> openDoorsInRangeOfEnemies
    
  let sortGameObjectsByDescendingDistance (game:Game) =
    // we need them in distance order for rendering and hit detection
    { game with
        GameObjects = game.GameObjects |> List.sortByDescending(fun s -> s.BasicGameObject.UnsquaredDistanceFromPlayer)
    }

  game
  |> (fun g -> match g with | IsActive ControlState.Forward -> move movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.Backward -> move (-movementSpeed/2.) g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.StrafingLeft -> strafe -movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.StrafingRight -> strafe movementSpeed g | _ -> g)
  |> (fun g -> match g with | IsActive ControlState.TurningLeft | IsActive ControlState.TurningRight -> rotate g | _ -> g)
  // firing must happen before we move objects as this will cause a sort by depth and we rely on the object index for a
  // potential hit that was recorded during scene rendering
  |> handleFiring
  |> updateEnemies
  |> handleAction
  |> updateTransitioningDoors
  |> sortGameObjectsByDescendingDistance