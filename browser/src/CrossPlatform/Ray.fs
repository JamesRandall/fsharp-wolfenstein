module App.Ray

open App.Model

// casts a ray until a certain condition (terminator) is met
let cast setup terminator game =
  let includeTurningPoints,posX, posY, rayDirection = setup ()
  let mapX = int posX
  let mapY = int posY
  
  let deltaDistX = if rayDirection.vX = 0. then System.Double.MaxValue else abs (1. / rayDirection.vX)
  let deltaDistY = if rayDirection.vY = 0. then System.Double.MaxValue else abs (1. / rayDirection.vY)
  let halfStepDeltaX = if rayDirection.vX = 0. then System.Double.MaxValue else sqrt(1.+rayDirection.vY * rayDirection.vY / rayDirection.vX * rayDirection.vX)
  let halfStepDeltaY = if rayDirection.vY = 0. then System.Double.MaxValue else sqrt(1.+rayDirection.vX * rayDirection.vX / rayDirection.vY * rayDirection.vY)
    
  let stepX, initialSideDistX =
    if rayDirection.vX < 0. then
      -1,(posX - float mapX) * deltaDistX
    else
      1,(float mapX + 1.0 - posX) * deltaDistX
  let stepY, initialSideDistY =
    if rayDirection.vY < 0. then
      -1,(posY - float mapY)*deltaDistY
    else
      1,(float mapY + 1.0 - posY)*deltaDistY
      
  let isHit, totalRayDistanceX, totalRayDistanceY, hitMapX, hitMapY, side =
    Seq.initInfinite (fun _ -> 0)
    |> Seq.scan(fun (isHit, currentRayDistanceX, currentRayDistanceY, currentMapX, currentMapY, currentSide) _ ->
      if not (terminator (isHit, currentRayDistanceX, currentRayDistanceY, currentMapX, currentMapY, currentSide)) then
        // if we've hit then we just return the current results - we're done
        (isHit, currentRayDistanceX, currentRayDistanceY, currentMapX, currentMapY, currentSide)
      else
        // we move on a step at a time 
        let newMapX, newMapY, newSide =
          if currentRayDistanceX < currentRayDistanceY then currentMapX + stepX, currentMapY, Side.NorthSouth else currentMapX, currentMapY + stepY, Side.EastWest
          
        let newSideDistX, newSideDistY =
          if currentRayDistanceX < currentRayDistanceY then
            currentRayDistanceX + deltaDistX, currentRayDistanceY
          else
            currentRayDistanceX, currentRayDistanceY + deltaDistY
        let newIsHit =
          match game.Map.[newMapY].[newMapX] with
          | Cell.TurningPoint _ -> includeTurningPoints
          | Cell.Empty -> false
          | Cell.Wall _ -> true
          | Cell.Door doorIndex ->
            let doorState = game.Doors.[doorIndex]
            match doorState.Status with
            | DoorStatus.Open _ -> false // rays never hit an open door
            | _ ->
              // to detect if a ray hits a door we need to move the ray on half a step - if we are still in the same
              // cell we've hit the door, if we're not then we haven't
              // side = 0 then north south (vertical) door, side = then east west (horizontal) door
              let map_x2 = if posX < newMapX then newMapX - 1 else newMapX
              let map_y2 = if posY > newMapY then newMapY + 1 else newMapY
              
              let adj = if newSide = Side.EastWest then float map_y2-posY else float map_x2-posX+1.
              let ray_mult = if newSide = Side.EastWest then adj/rayDirection.vY else adj/rayDirection.vX
              
              let rxe2 = posX+rayDirection.vX*ray_mult
              let rye2 = posY+rayDirection.vY*ray_mult
              
              let true_delta_x = if halfStepDeltaX < 0.0001 then 100. else halfStepDeltaX
              let true_delta_y = if halfStepDeltaY < 0.0001 then 100. else halfStepDeltaY
              
              // TODO: consider modelling offset as a percentage (0.x style)
              if newSide = Side.NorthSouth then
                let true_y_step = sqrt(true_delta_x*true_delta_x-1.)
                let half_step_in_y=rye2+(float stepY*true_y_step)/2.
                floor half_step_in_y = newMapY && half_step_in_y-float newMapY<(1.0-doorState.Offset/64.0)
              else
                let true_x_step=sqrt(true_delta_y*true_delta_y-1.)
                let half_step_in_x=rxe2+(float stepX*true_x_step)/2.
                floor half_step_in_x = newMapX && half_step_in_x-float newMapX<(1.0-doorState.Offset/64.0)
        newIsHit, newSideDistX, newSideDistY, newMapX, newMapY, newSide
    ) (false, initialSideDistX, initialSideDistY, mapX, mapY, Side.NorthSouth)
    |> Seq.skipWhile terminator
    |> Seq.head
  
  isHit, deltaDistX, deltaDistY, totalRayDistanceX, totalRayDistanceY, hitMapX, hitMapY, side
  
let isPointInTriangle p1 p2 p3 testPoint  =
  // barycentric coordinate approach
  // https://stackoverflow.com/questions/40959754/c-sharp-is-the-point-in-triangle
  let a =
    ((p2.vY - p3.vY)*(testPoint.vX - p3.vX) + (p3.vX - p2.vX)*(testPoint.vY - p3.vY)) /
    ((p2.vY - p3.vY)*(p1.vX - p3.vX) + (p3.vX - p2.vX)*(p1.vY - p3.vY))
  let b =
    ((p3.vY - p1.vY)*(testPoint.vX - p3.vX) + (p1.vX - p3.vX)*(testPoint.vY - p3.vY)) /
    ((p2.vY - p3.vY)*(p1.vX - p3.vX) + (p3.vX - p2.vX)*(p1.vY - p3.vY))
  let c = 1. - a - b
  a >= 0. && a <= 1. && b >= 0. && b <= 1. && c >= 0. && c <= 1.