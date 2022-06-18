module App.AI
open App.Model

module List =
  let private randomGenerator = System.Random ()
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
  
let getNextState canSeePlayer enemy =
  match enemy.State, canSeePlayer with
  //| EnemyStateType.Path, true
  | EnemyStateType.Standing, true
  | EnemyStateType.Ambushing, true ->
    (
      [
        fun () -> EnemyStateType.Attack
        fun () -> EnemyStateType.Chase
      ] |> List.random
    ) ()
  | _ -> enemy.State
    
let preProcess game enemy =
  // preprocess looks for state changes based on the current game world state
  let canSeePlayer = enemy |> isPlayerVisibleToEnemy game
  let newState = enemy |> getNextState canSeePlayer
  if newState <> enemy.State then
    Utils.log $"Enemy at {enemy.BasicGameObject.Position.vX}, {enemy.BasicGameObject.Position.vY} moving from {enemy.State} to {newState}"
    { enemy with State = newState }
  else
    enemy
    
let updateBasedOnCurrentState (frameTime:float<ms>) game enemy =
  let enemyVelocityUnitsPerSecond = 0.75
  
  // updates the enemy based on its state
  match enemy.State,enemy.DirectionVector with
  | EnemyStateType.Path, Some direction ->
    // A character entering a turning point causes it to turn in the direction the turning point indicates.
    //
    // Fast moving characters and/or low framerates could cause a character to essentially "skip" over a square and
    // so rather than check the new position on the map for a turning point we cast a ray between the previous position
    // and the new position looking for a hit of a turning point.
    //
    // As a fail safe if we don't hit a turning point we simply reverse the direction
    let newPosition = enemy.BasicGameObject.Position + (direction * (frameTime / 1000.<ms> * enemyVelocityUnitsPerSecond))
    let maxDistanceToCheck = (newPosition - enemy.BasicGameObject.Position).Abs()
    
    let castRay () =
      let setup () = true, enemy.BasicGameObject.Position.vX, enemy.BasicGameObject.Position.vY, direction
      let terminator (isHit, currentRayDistanceX, currentRayDistanceY, mapX, mapY, _) =
        (not isHit) &&
        (mapX >= 0 && mapX < game.Map.[0].Length && mapY >= 0 && mapY < game.Map.Length) &&
        (abs currentRayDistanceX < maxDistanceToCheck.vX || abs currentRayDistanceY < maxDistanceToCheck.vY)
      let isHit, _, _, _, _, hitMapX, hitMapY, _ = Ray.cast setup terminator game
      let isWallHit = match game.Map.[hitMapY].[hitMapX] with | Cell.Wall _ -> true | _ -> false
      isHit, isWallHit, hitMapX, hitMapY
      
    let isHit, isWallHit, hitMapX, hitMapY =
      match game.Map.[int newPosition.vY].[int newPosition.vX] with
      | Cell.TurningPoint tpDirection ->
        if tpDirection = direction then
          false, false, int newPosition.vX, int newPosition.vY
        else
          true, false, int newPosition.vX, int newPosition.vY
      | _ -> castRay ()
    
    // if we actually change direction then we must ensure the change of position is centered on the tile - otherwise
    // we will find that gradually things will move into "off" positions and move in strange ways
    let newDirection,finalPosition =
      match isHit, isWallHit with
      | true, false ->
        // we only actually turn on a turning point if we have moved past the center of the cell / turning point
        // and an easy way to figure that out is by comparing the distance to the new position based on the velocity
        // with the distance to the turning point center - if the former is greater or equal to the latter then we
        // have moved over the point
        let turningPointPosition = { vX = float hitMapX + 0.5 ; vY = float hitMapY + 0.5 }
        let existingDistanceToTurningPoint = (enemy.BasicGameObject.Position - turningPointPosition).Magnitude
        let distanceToNewPosition = (newPosition - enemy.BasicGameObject.Position).Magnitude
        if distanceToNewPosition >= existingDistanceToTurningPoint then       
          (match game.Map.[hitMapY].[hitMapX] with
          | Cell.TurningPoint newDirection -> newDirection
          | Cell.Door _ -> direction // if we hit a door keep walking, doors get opened by any characters heading towards them
          | _ -> direction.Reverse())
          ,{ vX = float hitMapX + 0.5 ; vY = float hitMapY + 0.5 }
        else
          direction,newPosition
      | true, true -> direction.Reverse(),{ vX = float hitMapX + 0.5 ; vY = float hitMapY + 0.5 }
      | _ -> direction,newPosition
        
    { enemy with
        BasicGameObject = { enemy.BasicGameObject with Position = finalPosition }
        DirectionVector = Some newDirection
    }
  | _ -> enemy

let applyAi frameTime game gameObject =
  match gameObject with
  | GameObject.Enemy enemy ->
    if enemy.IsAlive then
      enemy
      |> preProcess game
      // |> openDoorsInRange game
      |> updateBasedOnCurrentState frameTime game
      |> GameObject.Enemy
    else
      gameObject
  | _ -> gameObject