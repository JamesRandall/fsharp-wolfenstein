module App.AI
open App.Model

let private randomGenerator = System.Random ()

module List =
  let random (values:'a list) =
    values.[randomGenerator.Next(0,values.Length)]

let inFieldOfView game (enemy:Enemy) =
  // If an enemy has a direction then it also has a field of view in which the player can be seen
  match enemy.DirectionVector with
  | Some directionVector ->
    let fieldOfViewAngle = (45. * System.Math.PI / 180.) * 1.<radians>
    let boundingVectorA = directionVector.Normalize().Rotate -fieldOfViewAngle
    let boundingVectorB = directionVector.Normalize().Rotate fieldOfViewAngle
    let playerTestPoint =
      { vX = game.Camera.Position.vX - enemy.BasicGameObject.Position.vX
        vY = game.Camera.Position.vY - enemy.BasicGameObject.Position.vY
      }.Normalize()/2.
    Ray.isPointInTriangle { vX = 0. ; vY = 0. } boundingVectorA boundingVectorB playerTestPoint
  | _ -> true  
  

let isPlayerVisibleToEnemy game (enemy:Enemy) =
  // Approach is to:
  //  1. check if the vector from the enemy to the player is within the enemies field of view based on its direction
  //  2. cast a ray between the enemy and the player and if the ray passes the player without collision they can be seen
  if inFieldOfView game enemy then
    let playerX = game.Camera.Position.vX
    let playerY = game.Camera.Position.vY
    let posX = enemy.BasicGameObject.Position.vX
    let posY = enemy.BasicGameObject.Position.vY
    let vectorToEnemy = { vX = playerX - enemy.BasicGameObject.Position.vX ; vY = playerY - enemy.BasicGameObject.Position.vY }
    let absVectorToEnemy = vectorToEnemy.Abs()
    let rayDirection = vectorToEnemy.Normalize()
    
    let setup () = false, posX, posY, rayDirection
    let terminator (isHit, currentRayDistanceX, currentRayDistanceY, mapX, mapY, _) =
      (not isHit) &&
      (mapX >= 0 && mapX < game.Map.[0].Length && mapY >= 0 && mapY < game.Map.Length ) &&
      (abs currentRayDistanceX < absVectorToEnemy.vX || abs currentRayDistanceY < absVectorToEnemy.vY)
      
    let isHit, _, _, _, _, _, _, _ = Ray.cast setup terminator game
    not isHit
  else
    false
  
let canMove game (enemy:Enemy) (mapDirection:MapDirection) =
  if mapDirection = MapDirection.None then
    false
  else
    let deltaX,deltaY = mapDirection.ToDelta()
    let posX,posY = enemy.BasicGameObject.MapPosition
    let newX,newY = posX + deltaX, posY + deltaY
    
    //Utils.log $"posx: {posX}, posy: {posY}, newx: {newX}, newy: {newY}, dir: {mapDirection}"
    
    if newX < 0 || newX > 63 || newY < 0 || newY > 63 then
      false
    else
      let cell = game.Map.[newY].[newX]
      match cell with
      | Cell.Wall _ ->
        false
      | _ -> true
  
// Chasing occurs on whole map units and the state is re-evaluated when we have completed the move
// we may adopt this approach for path too (we may need to to break the path to shoot)
// The logic is based on that in the original code - this can be found in the SelectChaseDir method in WL_STATE.C
let setupChaseState (game:Game) (enemy:Enemy) =  
  let turnaround = enemy.Direction.Reverse()
  let playerX,playerY = game.PlayerMapPosition
  let enemyX,enemyY = enemy.BasicGameObject.MapPosition
  
  let deltaX = playerX - enemyX
  let deltaY = playerY - enemyY
  
  Utils.log $"Setting up chase state: {enemyX}, {enemyY}"
  
  let dir =
    [|
      if deltaX > 0 then MapDirection.West elif deltaX < 0 then MapDirection.East else MapDirection.None
      if deltaY > 0 then MapDirection.South elif deltaY < 0 then MapDirection.North else MapDirection.None
    |]
    |> (fun d -> if abs deltaY > abs deltaX then d |> Array.rev else d)
    |> Array.map (fun d -> if d = turnaround then MapDirection.None else d)
    
  let updateEnemy (withDirection:MapDirection) =
    let mapDeltaX,mapDeltaY = withDirection.ToDelta()
    let posX,posY = enemy.BasicGameObject.MapPosition
    let newX,newY = posX + mapDeltaX, posY + mapDeltaY
    { enemy with Direction = withDirection ; State = EnemyStateType.Chase (newX,newY) }
  
  
  // all the below needs tidying up, pretty ugly but it helped me mirror the logic as it is implemented in C
  let onNone func optionValue =
    match optionValue with
    | Some value -> Some value
    | None -> func ()
  
  (
    // try and move in the primary direction
    if canMove game enemy dir.[0] then
      Some (updateEnemy dir.[0])
    else
      None
  )
  |> onNone (fun () ->
    // try and move in the secondary direction
    if canMove game enemy dir.[1] then
      Some (updateEnemy dir.[1])
    else
      None      
  )
  |> onNone (fun () ->
    // try and move in the currently set direction
    if canMove game enemy enemy.Direction then
      Some (updateEnemy enemy.Direction)
    else
      None
  )
  |> onNone (fun () ->
    // randomly determine a direction - note that Wolfenstein only seems to search north, northwest and west
    let randomize values = if randomGenerator.Next(255) > 128 then values else values |> List.rev
    [ MapDirection.North ; MapDirection.NorthWest ; MapDirection.West ]
    |> randomize
    |> List.filter(fun direction -> canMove game enemy direction)
    |> List.tryHead
    |> Option.map(fun direction -> (updateEnemy direction))
  )
  |> onNone (fun () ->
    if canMove game enemy turnaround then
      Some (updateEnemy turnaround)
    else
      None
  )
  |> Option.defaultWith(fun () ->
    { enemy with Direction = MapDirection.None ; State = EnemyStateType.Standing }
  )
  (* this can be a handy diagnostic if you want to know what a chase state has been set to 
  |> (fun newEnemy ->
    Utils.log $"Set up chase state. Direction: {newEnemy.Direction}, Target: {newEnemy.State}"
    newEnemy
  )
  *)
  
// Chasing occurs on whole map units and the state is re-evaluated when we have completed the move
// we may adopt this approach for path too (we may need to to break the path to shoot)
// The logic is based on that in the original code - this can be found in the SelectDodgeDir method in WL_STATE.C
let setupChaseStateWithDodge (game:Game) (enemy:Enemy) =
  let randomize (dt:MapDirection array) =
    if randomGenerator.Next(255) < 128 then
      [|
        dt.[0]
        dt.[2]
        dt.[1]
        dt.[4]
        dt.[3]
      |]
    else
      dt
  let shuffle deltaX deltaY (dt:MapDirection array) =
    let absDeltaX = abs deltaX
    let absDeltaY = abs deltaY
    if absDeltaX > absDeltaY then
      [|
        dt.[0]
        dt.[2]
        dt.[1]
        dt.[4]
        dt.[3]
      |]
    else
      dt
  let setDiagonal (dt:MapDirection array) =
    [|
      MapDirection.Diagonal dt.[1] dt.[2]
      dt.[1]
      dt.[2]
      dt.[3]
      dt.[4]
    |]
    
  let turnaround = if enemy.IsFirstAttack then MapDirection.None else enemy.Direction.Reverse()
  let playerX,playerY = game.PlayerMapPosition
  let enemyX,enemyY = enemy.BasicGameObject.MapPosition
  
  let deltaX = playerX - enemyX
  let deltaY = playerY - enemyY
  
  let directionOption =
    [|
      MapDirection.None
      if deltaX > 0 then MapDirection.West else MapDirection.East
      if deltaY > 0 then MapDirection.South else MapDirection.North
      if deltaX > 0 then MapDirection.East else MapDirection.West
      if deltaY > 0 then MapDirection.North else MapDirection.South
    |]
    |> shuffle deltaX deltaY
    |> randomize
    |> setDiagonal
    |> Array.filter(fun direction ->
      if direction = MapDirection.None || direction = turnaround then
        false
      else
        canMove game enemy direction
    )
    |> Array.tryHead
  
  match directionOption with
  | Some direction ->
    let mapDeltaX,mapDeltaY = direction.ToDelta()
    let posX,posY = enemy.BasicGameObject.MapPosition
    let newX,newY = posX + mapDeltaX, posY + mapDeltaY
    { enemy with Direction = direction ; State = EnemyStateType.Chase (newX,newY) }
  | None -> enemy
  
let createAttackState game enemy =
  { enemy with
      State = EnemyStateType.Attack
      CurrentAnimationFrame = 0
      Direction = MapDirection.None
      TimeUntilNextAnimationFrame = Enemy.AnimationTimeForState EnemyStateType.Attack
      IsFirstAttack = false
  }
  
let createChaseState canSeePlayer (game:Game) enemy =
  if canSeePlayer then
    let enemyX, enemyY = enemy.BasicGameObject.MapPosition
    let playerX, playerY = game.PlayerMapPosition
    let absDeltaX = abs (enemyX - playerX)
    let absDeltaY = abs (enemyY - playerY)
    let distance = if absDeltaX > absDeltaY then absDeltaX else absDeltaY
    let shouldShoot =
      // original source: if (!dist || (dist==1 && ob->distance<0x4000) )
      // not yet sure what that second part is about
      if enemy.State = EnemyStateType.Attack then
        false // attack never follows attack
      elif distance = 0 then 
        true
      else
        randomGenerator.Next(255) < 255 / distance
    if shouldShoot then
      createAttackState game enemy
    else
      setupChaseStateWithDodge game enemy
  else
    setupChaseState game enemy
  
let getNextState canSeePlayer game enemy =
  match enemy.State, canSeePlayer with
  // bug in this at the moment - we can't move out of a path state here as we can only swap state when we are on
  // the center of a tile - i.e. movement is unit based and the AI is aligned with that
  //| EnemyStateType.Path, true // comment this line out to test patrolling without it being interrupted
  | EnemyStateType.Standing, true
  | EnemyStateType.Ambushing, true ->
    (*(
      [
        fun () -> EnemyStateType.Attack
        fun () -> EnemyStateType.Chase (0,0)
      ] |> List.random
    ) ()*)
    createChaseState canSeePlayer game enemy
  | EnemyStateType.Attack, _ ->
    // the frame time can only drop below zero when we have gone past the last frame and so we use
    // this to detect if we need to move back to the chase state
    if enemy.TimeUntilNextAnimationFrame < 0.<ms> then
      createChaseState canSeePlayer game enemy
    else
      enemy
  | _ -> enemy
    
let preProcess canSeePlayer (enemy,game) =
  // preprocess looks for state changes based on the current game world state
  let newEnemy = enemy |> getNextState canSeePlayer game
  if newEnemy.State <> enemy.State then
    Utils.log $"Enemy at {enemy.BasicGameObject.Position.vX}, {enemy.BasicGameObject.Position.vY} moving from {enemy.State} to {newEnemy.State}"
  (newEnemy,game)
    
// this is loosely based on T_Shoot in WL_ACT2.C
let firingOnPlayer canSeePlayer (enemy:Enemy,game:Game)  =
  // Need to figure out what areabyplayer is
  // I _think_ what it is is that the map is divided into areas separated by doors and therefore this is actually saying
  // is that the player only takes damage if the enemy is in the same room as the player. These areas are also used
  // for sound propagation in the AI.
  // if (!areabyplayer[ob->areanumber])
  //	return;
  let damage =
    if enemy.FireAtPlayerRequired && canSeePlayer then
      let enemyX,enemyY = enemy.BasicGameObject.MapPosition
      let playerX,playerY = game.PlayerMapPosition
      let deltaX = abs (enemyX - playerX)
      let deltaY = abs (enemyY - playerY)
      
      // SS and bosses are better shots than regular enemies 
      let distance =
        match enemy.EnemyType,enemy.IsBoss with
        | EnemyType.SS,_
        | _,true -> (max deltaX deltaY)*2/3
        | _ -> max deltaX deltaY
      
      let hitchance =
        if game.IsPlayerRunning then
           if enemy.IsVisible then 160-distance*16 else 160-distance*8
        else
          if enemy.IsVisible then 256-distance*16 else 160-distance*8
      
      Audio.playAttackSound game enemy.BasicGameObject.Position enemy.EnemyType
          
      if randomGenerator.Next(255) < hitchance then
        if distance < 2 then
          randomGenerator.Next(255) >>> 2
        elif distance < 4 then
          randomGenerator.Next(255) >>> 3
        else randomGenerator.Next(255) >>> 4
      else
        0
    else
      0
      
  if damage > 0 then Utils.log $"Damage: {damage}"
  let maximumPossibleDamage = 64.
      
  enemy,
  { game with
      Player = { game.Player with Health = game.Player.Health - damage * 1<hp> }
      ViewportFilter =
        if damage > 0 then
          ViewportFilter.Overlay (OverlayAnimation.Blood ((float damage) / maximumPossibleDamage))
        else
          game.ViewportFilter
  }  
    
let updateBasedOnCurrentState canSeePlayer (frameTime:float<ms>) (enemy,game) =
  // updates the enemy based on its state
  match enemy.State,enemy.DirectionVector with
  | EnemyStateType.Chase (targetMapX, targetMapY), Some direction ->
    let enemyVelocityUnitsPerSecond = 1.
    let targetPosition = { vX = float targetMapX + 0.5 ; vY = float targetMapY + 0.5 }
    let distanceToTarget = targetPosition - enemy.BasicGameObject.Position
    let frameRateBasedDelta = (direction * (frameTime / 1000.<ms> * enemyVelocityUnitsPerSecond))
    
    // if we have further to travel than we need to update this frame based on our velocity then
    // we continue to chase - alternatively if our velocity would take us past the target then we
    // re-evaluate our chase state 
    if distanceToTarget.Magnitude > frameRateBasedDelta.Magnitude then
      let newPosition = enemy.BasicGameObject.Position + frameRateBasedDelta
      { enemy with BasicGameObject = { enemy.BasicGameObject with Position = newPosition } },game
    else
      ({ enemy with BasicGameObject = { enemy.BasicGameObject with Position = targetPosition } }
      |> createChaseState canSeePlayer game),game
    
  | EnemyStateType.Path (targetMapX, targetMapY), Some direction ->
    let enemyVelocityUnitsPerSecond = 0.5
    let targetPosition = { vX = float targetMapX + 0.5 ; vY = float targetMapY + 0.5 }
    let distanceToTarget = targetPosition - enemy.BasicGameObject.Position
    let frameRateBasedDelta = (direction * (frameTime / 1000.<ms> * enemyVelocityUnitsPerSecond))
    // if we have further to travel than we need to update this frame based on our velocity then
    // we continue to move - alternatively if our velocity would take us past the target then we
    // re-evaluate our path state 
    if distanceToTarget.Magnitude > frameRateBasedDelta.Magnitude then
      let newPosition = enemy.BasicGameObject.Position + frameRateBasedDelta
      { enemy with BasicGameObject = { enemy.BasicGameObject with Position = newPosition } },game
    else
      // we re-evaluate our state
      if canSeePlayer then
        // if the patrolling guard can see the player then we break out into chase
        (createChaseState canSeePlayer game enemy),game        
      else
        // otherwise move to the next cell on our path
        let newDirection =
          match game.Map.[targetMapY].[targetMapX] with
          | Cell.TurningPoint tpDirection -> tpDirection
          | _ -> enemy.Direction
        let newDirectionX, newDirectionY = newDirection.ToDelta()
        let newTargetX = targetMapX + newDirectionX
        let newTargetY = targetMapY + newDirectionY
        let turnedEnemy = 
          { enemy with
              BasicGameObject = { enemy.BasicGameObject with Position = targetPosition }
              State = (EnemyStateType.Path (newTargetX,newTargetY))
              Direction = newDirection
          }
        let canNowSeePlayer = turnedEnemy |> isPlayerVisibleToEnemy game
        (if canNowSeePlayer then (createChaseState canSeePlayer game turnedEnemy) else turnedEnemy),game
  | _ -> enemy,game
    
let applyAi frameTime game gameObject =
  match gameObject with
  | GameObject.Enemy enemy ->
    if enemy.IsAlive then
      let canSeePlayer = enemy |> isPlayerVisibleToEnemy game
      let updatedEnemy, updatedGame =
        (enemy,game)
        |> preProcess canSeePlayer
        |> updateBasedOnCurrentState canSeePlayer frameTime
        |> firingOnPlayer canSeePlayer
      updatedEnemy |> GameObject.Enemy, updatedGame
    else
      gameObject,game
  | _ -> gameObject,game
  
  
