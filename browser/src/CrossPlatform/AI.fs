module App.AI
open App.Model
open Fable.Core.JS

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
    let directionToPlayer =
      { vX = game.Camera.Position.vX - enemy.BasicGameObject.Position.vX
        vY = game.Camera.Position.vY - enemy.BasicGameObject.Position.vY
      }
    directionToPlayer.Normalize().IsBetween boundingVectorA boundingVectorB 
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
    
    let setup () = posX,posY, rayDirection
    let terminator (isHit, currentRayDistanceX, currentRayDistanceY, mapX, mapY, side) =
      (not isHit) &&
      (mapX >= 0 && mapX < game.Map.[0].Length && mapY >= 0 && mapY < game.Map.Length ) &&
      (abs currentRayDistanceX < absVectorToEnemy.vX || abs currentRayDistanceY < absVectorToEnemy.vY)
      
    let isHit, _, _, _, _, _, _, _ = Ray.cast setup terminator game
    not isHit
  else
    false
  
let getNextState canSeePlayer enemy =
  match enemy.State, canSeePlayer with
  | EnemyStateType.Standing, true
  | EnemyStateType.Ambushing, true ->
    (
      [
        fun () -> EnemyStateType.Attack
        fun () -> EnemyStateType.Chase
      ] |> List.random
    ) ()
  | _ -> enemy.State
    
let applyAi game gameObject =
  match gameObject with
  | GameObject.Enemy enemy ->
    let canSeePlayer = enemy |> isPlayerVisibleToEnemy game
    let newState = enemy |> getNextState canSeePlayer
    (
      if newState <> enemy.State then
        //console.log $"Enemy at {enemy.BasicGameObject.Position.vX}, {enemy.BasicGameObject.Position.vY} moving from {enemy.State} to {newState}"
        { enemy with State = newState }
      else
        enemy
    )
    |> GameObject.Enemy
  | _ -> gameObject