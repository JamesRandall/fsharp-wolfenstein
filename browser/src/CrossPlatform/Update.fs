module App.Update

open App.Audio
open App.Model
open App.AI

let private weaponAnimationFrameTime = 100.<ms>
let private actionDistanceTolerance = 0.75
// doors are 0.5 recessed which means we need to extend the action activation distance
let private actionDoorDistanceTolerance = actionDistanceTolerance + 0.5 
let private doorOpeningTime = 1000.<ms>
let private doorOpenTime = 5000.<ms>
let private random = System.Random()

let updateFrame game frameTime createPixelDissolver (renderingResult:WallRenderingResult) =
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
      let doorPosition = Vector2D.CreateFromMapPosition doorState.MapPosition
      playSoundEffect game doorPosition SoundEffect.DoorOpen
      { doorState with Status = DoorStatus.Opening ; TimeRemainingInAnimationState = doorOpeningTime }
    | _ -> doorState
    
  let updateCompositeAreas doorState compositeAreas =
    let updatedAreas =
      match doorState.Status with
      | DoorStatus.Opening ->
        compositeAreas
        |> List.map(fun ca ->
          if ca.ConnectedTo |> Set.contains doorState.AreaOne && not (ca.ConnectedTo |> Set.contains doorState.AreaTwo) then
            { ca with ConnectedTo = ca.ConnectedTo |> Set.add doorState.AreaTwo }
          elif ca.ConnectedTo |> Set.contains doorState.AreaTwo && not (ca.ConnectedTo |> Set.contains doorState.AreaOne) then
            { ca with ConnectedTo = ca.ConnectedTo |> Set.add doorState.AreaOne }
          else
            ca
        )
      | DoorStatus.Closed ->
        compositeAreas
        |> List.map(fun ca ->
          if ca.Area <> doorState.AreaOne && ca.ConnectedTo |> Seq.contains doorState.AreaOne then
            { ca with ConnectedTo = ca.ConnectedTo |> Set.remove doorState.AreaOne }
          elif ca.Area <> doorState.AreaTwo && ca.ConnectedTo |> Seq.contains doorState.AreaTwo then
            { ca with ConnectedTo = ca.ConnectedTo |> Set.remove doorState.AreaTwo }
          else
            ca
        )
      | _ -> compositeAreas
    //Utils.diagnoseCompositeAreas updatedAreas
    updatedAreas
       
  let handleAction (game:Game) =
    if (
        (not renderingResult.IsDoorInFrontOfPlayer && renderingResult.DistanceToWallInFrontOfPlayer <= actionDistanceTolerance) ||
        (renderingResult.IsDoorInFrontOfPlayer && renderingResult.DistanceToWallInFrontOfPlayer <= actionDoorDistanceTolerance)
       )       
       && game.ControlState &&& ControlState.Action > ControlState.None then
      let actionWallX, actionWallY = renderingResult.WallInFrontOfPlayer
      match game.Map.[actionWallY].[actionWallX] with
      | Cell.Door doorIndex ->
        let newDoorState = tryOpenDoor game.Doors.[doorIndex] 
        { game with
            Doors = game.Doors |> List.mapi(fun i d -> if i = doorIndex then newDoorState else d)
            CompositeAreas = game.CompositeAreas |> updateCompositeAreas newDoorState
        }
      | _ -> game
    else
      game
      
  let updateTransitioningDoors game =
    let newDoors,compositeAreas =
      game.Doors
      |> List.fold(fun (updatedDoors,updatedCompositeAreas:CompositeArea list) doorState ->
        let newTimeRemainingInAnimationState = doorState.TimeRemainingInAnimationState - frameTime
        let newDoorState, newCompositeAreas =
          match doorState.Status with
          | DoorStatus.Opening ->
            if newTimeRemainingInAnimationState < 0.<ms> then
              { doorState with Status = DoorStatus.Open ; TimeRemainingInAnimationState = doorOpenTime ; Offset = 64 },updatedCompositeAreas
            else
              let newOffset = (doorOpeningTime - newTimeRemainingInAnimationState) / doorOpeningTime * 64.
              { doorState with TimeRemainingInAnimationState = newTimeRemainingInAnimationState ; Offset = newOffset },updatedCompositeAreas
          | DoorStatus.Open ->
            if newTimeRemainingInAnimationState < 0.<ms> then
              let doorPosition = Vector2D.CreateFromMapPosition doorState.MapPosition
              playSoundEffect game doorPosition SoundEffect.DoorClose
              { doorState with Status = DoorStatus.Closing ; TimeRemainingInAnimationState = doorOpeningTime },updatedCompositeAreas
            else
              { doorState with TimeRemainingInAnimationState = newTimeRemainingInAnimationState },updatedCompositeAreas
          | DoorStatus.Closing ->
            if newTimeRemainingInAnimationState < 0.<ms> then
              let updatedDoorState =
                { doorState with Status = DoorStatus.Closed ; TimeRemainingInAnimationState = doorOpeningTime ; Offset = 0. }
              updatedDoorState, game.CompositeAreas |> updateCompositeAreas updatedDoorState
            else
              let newOffset = 64. - ((doorOpeningTime - newTimeRemainingInAnimationState) / doorOpeningTime * 64.)
              { doorState with TimeRemainingInAnimationState = newTimeRemainingInAnimationState ; Offset = newOffset },updatedCompositeAreas
          | _ -> doorState,updatedCompositeAreas
        // take care to do an append here as the door lists are index sensitive so we need to keep them in the same order
        updatedDoors @ [newDoorState],newCompositeAreas
      ) ([],game.CompositeAreas)
    { game with Doors = newDoors ; CompositeAreas = compositeAreas }
    
    
  let handleFiring game =
    let isFireKeyPressed = int (game.ControlState &&& ControlState.Fire) > 0 
    
    let updatePlayerWithWeapon weapon =
      { game.Player with
          Weapons = game.Player.Weapons |> List.mapi(fun i wp -> if i = game.Player.CurrentWeaponIndex then weapon else wp)
      }
    let currentWeapon = game.Player.Weapons.[game.Player.CurrentWeaponIndex]
    match game.IsFiring, game.TimeToNextWeaponFrame, isFireKeyPressed with
    | false, None, true ->
      if (game.Player.CurrentWeapon.RequiresAmmunition && game.Player.Ammunition > 0<bullets>) || not game.Player.CurrentWeapon.RequiresAmmunition then
        let updatedWeapon = { currentWeapon with CurrentFrame = 1 }
        
        // begin firing
        playSoundEffect game game.Camera.Position SoundEffect.PlayerPistol
        { game with
            IsFiring = true
            TimeToNextWeaponFrame = Some weaponAnimationFrameTime
            Player = { (updatedWeapon |> updatePlayerWithWeapon) with Ammunition = game.Player.Ammunition - 1<bullets> } 
        },true
      elif game.Player.CurrentWeapon.RequiresAmmunition then
        let newWeaponIndex = game.Player.Weapons |> List.findIndex(fun w -> not w.RequiresAmmunition) 
        let newWeapon = game.Player.Weapons.[newWeaponIndex]
        let updatedWeapon = { newWeapon with CurrentFrame = 1 }
        
        // begin firing
        playSoundEffect game game.Camera.Position SoundEffect.PlayerPistol
        { game with
            IsFiring = true
            TimeToNextWeaponFrame = Some weaponAnimationFrameTime
            Player = { (updatedWeapon |> updatePlayerWithWeapon) with
                        Ammunition = game.Player.Ammunition - 1<bullets>
                        CurrentWeaponIndex = newWeaponIndex
                     } 
        },true
      else
        game,false
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
              playRandomEnemyDeathSoundEffectAtVolume game e.BasicGameObject.Position
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
    | GameObject.Static t -> GameObject.Static t
    
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
                TimeUntilNextAnimationFrame = (Enemy.AnimationTimeForState enemy.State) + timeRemainingInAnimationFrame 
                FireAtPlayerRequired = enemy.CurrentAnimationFrame+1 = enemy.AnimationFrames-1 } |> GameObject.Enemy
          else
            { enemy with TimeUntilNextAnimationFrame = timeRemainingInAnimationFrame } |> GameObject.Enemy
        else
          match enemy.State with
          | EnemyStateType.Attack ->
            { enemy with TimeUntilNextAnimationFrame = timeRemainingInAnimationFrame } |> GameObject.Enemy
          | _ -> enemy |> GameObject.Enemy
      | EnemyStateType.Chase _
      | EnemyStateType.Path _ ->
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
          | EnemyStateType.Path _, Some direction
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
                let newDoorState = tryOpenDoor innerGame.Doors.[doorIndex]
                { innerGame with
                    Doors =
                      innerGame.Doors
                      |> List.mapi (fun i d -> if i = doorIndex then newDoorState else d)
                    CompositeAreas = innerGame.CompositeAreas |> updateCompositeAreas  newDoorState
                }
              | _ -> innerGame
            else
              innerGame
          | _ -> innerGame
      | _ -> innerGame
    ) game
    
  let resetNeedToFire gameObject =
    match gameObject with
    | GameObject.Enemy enemy ->
      { enemy with FireAtPlayerRequired = false } |> GameObject.Enemy
    | _ -> gameObject
    
    
  let updateEnemies (game:Game,beganFiringSequenceOnFrame) =
    let _,updatedGame,updatedGameObjects =
      game.GameObjects
      |> List.fold(fun (i,innerGame,gameObjects) go ->
        let updatedGameObject,updatedGame =
          go
          |> resetNeedToFire
          |> updateEnemyBasedOnPlayerFiring beganFiringSequenceOnFrame i
          |> updateEnemyAnimation frameTime 
          |> applyAi frameTime innerGame // this did read "game" but I think that's wrong, though I've not seen a bug
        i+1,updatedGame,(updatedGameObject |> calculateRelativeGameObjectPosition updatedGame) :: gameObjects
      ) (0,game,[])           
    { updatedGame with GameObjects = updatedGameObjects } |> openDoorsInRangeOfEnemies
    
  let sortGameObjectsByDescendingDistance (game:Game) =
    // we need them in distance order for rendering and hit detection
    { game with
        GameObjects =
          game.GameObjects
          |> List.sortByDescending(fun s ->
            // we sort by the object type to stop flickering when two objects are at the same distance
            let objectTypeDepth =
              match s with
              | GameObject.Enemy _ -> 1
              | GameObject.Static go ->
                if go.Pickupable then 0 else 2
            s.BasicGameObject.UnsquaredDistanceFromPlayer,objectTypeDepth
          )
    }
    
  let updateViewportFilter game =
    let newFilter =
      match game.ViewportFilter with
      | ViewportFilter.Overlay overlay ->
        let timeRemainingInFrame = overlay.TimeRemainingUntilNextFrame - frameTime
        if timeRemainingInFrame <= 0.<ms> then
          let newTimeRemainingInFrame = overlay.FrameLength + timeRemainingInFrame
          let newOpacity = overlay.Opacity + overlay.OpacityDelta
          if newOpacity >= overlay.MaxOpacity then
            { overlay with Opacity = overlay.MaxOpacity
                           OpacityDelta = overlay.OpacityDelta * -1.
                           TimeRemainingUntilNextFrame = newTimeRemainingInFrame
            } |> ViewportFilter.Overlay
          elif newOpacity <= 0. then
            ViewportFilter.None
          else
            { overlay with Opacity = newOpacity ; TimeRemainingUntilNextFrame = newTimeRemainingInFrame }
            |> ViewportFilter.Overlay
        else
          { overlay with TimeRemainingUntilNextFrame = timeRemainingInFrame } |> ViewportFilter.Overlay
      | _ -> game.ViewportFilter
    { game with ViewportFilter = newFilter }
    
  let removePixelDissolverIfDone game =
    match game.PixelDissolver with
    | Some { PixelDissolver.DissolverState = PixelDissolverState.Backwards ; PixelDissolver.DrawnPixels = [] } ->
      { game with PixelDissolver = None}
    | _ -> game
    
  let updatePixelDissolver game =
    match game.PixelDissolver with
    | Some pixelDissolver ->
      let lengthOfDissolve = 1000.<ms>
      let updatedDissolver,restartGameRequired =
        match pixelDissolver.DissolverState with
        | PixelDissolverState.Stopped -> pixelDissolver, false
        | PixelDissolverState.Forwards ->
          // we use the original length and not the current set length here else we'll dissolve ever less pixels
          let pixelsToDissolveThisFrame =
            min (int (float pixelDissolver.TotalPixels / lengthOfDissolve * frameTime)) pixelDissolver.RemainingPixels.Length
          ({ pixelDissolver with
              DrawnPixels = pixelDissolver.DrawnPixels @ (pixelDissolver.RemainingPixels |> List.take pixelsToDissolveThisFrame)
              RemainingPixels = pixelDissolver.RemainingPixels |> List.skip pixelsToDissolveThisFrame
          },false) |> (fun (pd,_) -> if pd.RemainingPixels.Length = 0 then { pd with DissolverState = PixelDissolverState.Transitioning },true else pd,false)
        | PixelDissolverState.Transitioning ->
          let newTimeRemaining = pixelDissolver.PauseTimeRemaining - frameTime
          if newTimeRemaining <= 0.<ms> then
            { pixelDissolver with DissolverState = PixelDissolverState.Backwards },false
          else
            { pixelDissolver with PauseTimeRemaining = newTimeRemaining },false
        | PixelDissolverState.Backwards ->
          // we use the original length and not the current set length here else we'll dissolve ever less pixels
          let pixelsToDissolveThisFrame =
            min (int (float pixelDissolver.TotalPixels / lengthOfDissolve * frameTime)) pixelDissolver.DrawnPixels.Length
          { pixelDissolver with
              DrawnPixels = pixelDissolver.DrawnPixels |> List.skip pixelsToDissolveThisFrame
              RemainingPixels = pixelDissolver.RemainingPixels @ (pixelDissolver.DrawnPixels |> List.take pixelsToDissolveThisFrame)
          },false
      if restartGameRequired then
        if game.Player.Lives = 1<life> then
          { game with
              PixelDissolver = Some {updatedDissolver with DissolverState = PixelDissolverState.Stopped }
              Player = { game.Player with Lives = 0<life> }
          } // TODO: We need to send an "end the game state" somewhere
        else
          let resetLevel = game.ResetLevel game game.Player
          { resetLevel with
              PixelDissolver = Some updatedDissolver
              Player = { resetLevel.Player with Lives = game.Player.Lives - 1<life> }
          }
      else
        { game with PixelDissolver = Some updatedDissolver }
    | None -> game
    
  let updatePlayerFace game =
    let newTimeRemaining = game.Player.TimeToFaceChangeMs - frameTime
    let player =
      if newTimeRemaining < 0.<ms> then
        let randomTime (maxDelta:float<ms>) =
          random.NextDouble() * maxDelta + 750.<ms>
        { game.Player with
            TimeToFaceChangeMs = (if random.Next(100) < 75 then 1000.<ms> else 2250.<ms>) |> randomTime
            CurrentFaceIndex = random.Next(3)
        }
      else
        { game.Player with TimeToFaceChangeMs = newTimeRemaining }
    { game with Player = player }
    
  let pickupObject (game:Game) =
    let pickupRadius = 0.5
    let playerMapPosition = game.PlayerMapPosition
    let gameObjectsAtLocation =
      game.GameObjects
      |> List.filter(fun go ->
        go.BasicGameObject.MapPosition = playerMapPosition && go.BasicGameObject.Pickupable &&
        (go.BasicGameObject.HitpointsRestored = 0<hp> || game.Player.Health < 100<hp>)
      )
    if gameObjectsAtLocation.Length > 0 then
      let playerMapX, playerMapY = game.PlayerMapPosition
      let center = { vX = float playerMapX + 0.5 ; vY = float playerMapY + 0.5 }
      let distanceFromCenter = (game.Camera.Position - center).Magnitude
      if distanceFromCenter < pickupRadius then
        let updatedPlayer =
          gameObjectsAtLocation
          |> List.fold(fun player go ->
            let bgo = go.BasicGameObject
            { player with
                Ammunition = min 99<bullets> (player.Ammunition + bgo.AmmoRestored)
                Health = min 100<hp> (player.Health + bgo.HitpointsRestored)
                Lives = min 9<life> (player.Lives + bgo.LivesRestored)
                Score = player.Score + bgo.Score
            }
          ) game.Player
        let viewportFilter =
          match game.ViewportFilter with
          | ViewportFilter.None -> OverlayAnimation.Pickup |> ViewportFilter.Overlay
          | _ -> game.ViewportFilter
        { game with
            GameObjects = game.GameObjects |> List.filter (fun go -> gameObjectsAtLocation |> List.contains go |> not )
            ViewportFilter = viewportFilter
            Player = updatedPlayer
        }
      else
        game
    else
      game
    
    
  let checkIfPlayerIsDead game =
    if game.Player.Health < 0<hp> then
      { game with
          PixelDissolver = createPixelDissolver () |> Some
          ViewportFilter = ViewportFilter.None
      }
    else
      game
      
  match game.PixelDissolver with
  | Some _ ->
    game
    |> updatePixelDissolver
    |> removePixelDissolverIfDone
  | None ->  
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
    |> pickupObject
    |> updateTransitioningDoors
    |> updateViewportFilter
    |> sortGameObjectsByDescendingDistance
    |> updatePlayerFace
    |> checkIfPlayerIsDead