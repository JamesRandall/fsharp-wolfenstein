namespace App.Render

open App.Model
open App.PlatformModel
  
module Walls =
  open App
  
  let draw width height setPixel getTextureStripOffset getTextureColor game =
    let posX = game.Camera.Position.vX
    let posY = game.Camera.Position.vY
    let viewportWidth = width
    let viewportHeight = height
    
    let initialWallRenderResult = { WallInFrontOfPlayer = -1,-1 ; DistanceToWallInFrontOfPlayer = -1 ; ZIndexes = [] ; SpriteInFrontOfPlayerIndexOption = None }
    let viewportStart = 0.
    let viewportEnd = viewportWidth-1.
    {viewportStart..viewportEnd}
    |> Seq.fold(fun previousResult viewportX ->
      let cameraX = (2. * viewportX / viewportWidth) - 1.
      let rayDirection = {
        vX = game.Camera.Direction.vX + game.Camera.Plane.vX * cameraX
        vY = game.Camera.Direction.vY + game.Camera.Plane.vY * cameraX
      }
      let setup () = posX, posY, rayDirection
      let terminator (isHit, _, _, mapX, mapY, _) =
        (not isHit) && (mapX >= 0 && mapX < game.Map.[0].Length && mapY >= 0 && mapY < game.Map.Length )
      let _, deltaDistX, deltaDistY, totalRayDistanceX, totalRayDistanceY, hitMapX, hitMapY, side =
        Ray.cast setup terminator game
      
      let perpendicularWallDistance =
        match side with
        | Side.NorthSouth -> (totalRayDistanceX - deltaDistX)
        | Side.EastWest -> (totalRayDistanceY - deltaDistY)
      
      let perpendicularWallDistance =
        match game.Map.[hitMapY].[hitMapX] with
        | Cell.Door _ -> perpendicularWallDistance + 0.5 // doors are recessed by 0.5 map units
        | _ -> perpendicularWallDistance
      
      let lineHeight = viewportHeight / perpendicularWallDistance
      let startY = max (-lineHeight/2. + viewportHeight/2.) 0.
      let endY = min (lineHeight/2. + viewportHeight/2.) (viewportHeight-1.)
      
      match game.Map.[hitMapY].[hitMapX] with
      | Cell.Wall wall ->
        let wallX =
          match side with
          | Side.NorthSouth -> posY + perpendicularWallDistance * rayDirection.vY
          | Side.EastWest -> posX + perpendicularWallDistance * rayDirection.vX
        let clampedWallX = wallX - (floor wallX)
        let rawTextureX = int (clampedWallX * textureWidth)
        let textureX,textureIndex =
          if side = Side.NorthSouth && rayDirection.vX > 0. then (textureWidth - float rawTextureX - 1.,wall.NorthSouthTextureIndex)
          elif side = Side.EastWest && rayDirection.vY < 0. then (textureWidth - float rawTextureX - 1.,wall.EastWestTextureIndex)
          else (float rawTextureX,if side = Side.NorthSouth then wall.NorthSouthTextureIndex else wall.EastWestTextureIndex)
        
        let lineHeight = viewportHeight / perpendicularWallDistance
        let step = 1.0 * textureHeight / lineHeight
        let texPos = (startY - viewportHeight/2. + lineHeight/2.) * step
        let textureOffset = getTextureStripOffset textureIndex (int textureX)
        
        {0..int (endY-startY)}
        |> Seq.iter(fun drawY ->
          let textureY = int (texPos+step*float drawY) &&& (int textureHeight-1)
          let color = getTextureColor textureOffset textureY
          setPixel color (int viewportX) (drawY+int startY)
        )
      | Cell.Door doorIndex ->
        let door = game.Doors.[doorIndex]
        let wallX =
          if side = Side.NorthSouth then
            posY + perpendicularWallDistance * rayDirection.vY
          else
            posX + perpendicularWallDistance * rayDirection.vX
        let clampedWallX = wallX - (floor wallX)
        let rawTextureX = int (clampedWallX * textureWidth)
        
        let textureX = max 0. (textureWidth - float rawTextureX - 1. - float door.Offset)
        let textureIndex = door.TextureIndex
          
        let lineHeight = viewportHeight / perpendicularWallDistance
        let step = 1.0 * textureHeight / lineHeight
        let texPos = (startY - viewportHeight/2. + lineHeight/2.) * step
        let textureOffset = getTextureStripOffset textureIndex (int textureX)
        
        {0..int (endY-startY)}
        |> Seq.iter(fun drawY ->
          let textureY = int (texPos+step*float drawY) &&& (int textureHeight-1)
          let color = getTextureColor textureOffset textureY
          setPixel color (int viewportX) (drawY+int startY)
        )
      | _ -> ()
      
      // set the zIndex for the strip
      { previousResult with
          ZIndexes = previousResult.ZIndexes @ [perpendicularWallDistance]
          WallInFrontOfPlayer = if viewportX = viewportWidth/2. then hitMapX,hitMapY else previousResult.WallInFrontOfPlayer
          DistanceToWallInFrontOfPlayer = if viewportX = viewportWidth/2. then perpendicularWallDistance else previousResult.DistanceToWallInFrontOfPlayer
      } 
    ) initialWallRenderResult
    
module Objects =
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
    
  let draw width height getPixel setPixel isTransparent getSpriteOffsets game (sprites:Texture array) (wallRenderResult:WallRenderingResult) =
    let orientedSpriteIndex gameObject =
      match gameObject with
      | GameObject.Treasure t -> t.SpriteIndex
      | GameObject.Enemy e ->
        if e.IsAlive then
          e.DirectionVector
          |> Option.map(fun directionVector ->
            // we can precalc the vector array below whenever the enemy changes direction
            let spriteQuadrants = 8
            let quadrantSize = (360.<degrees> / float spriteQuadrants) |> degreesToRadians
            let playerRelativePosition =
              { vX = game.Camera.Position.vX - e.BasicGameObject.Position.vX
                vY = game.Camera.Position.vY - e.BasicGameObject.Position.vY
              }.Normalize()
            let vectors =
              {0..spriteQuadrants-1}
              |> Seq.map(fun quadrant ->
                let centerAngle = float quadrant * quadrantSize
                let startAngle = centerAngle - quadrantSize/2.
                let endAngle = centerAngle + quadrantSize/2.
                let startVector = directionVector.Rotate startAngle
                let endVector = directionVector.Rotate endAngle
                startVector,endVector
              )
              |> Seq.toArray
            // Make sure the player position is within the triangle by shortening its distance from the enemy -
            // a magnitude point will not be in the triangle (the magnitude is essentially the radius on a circle
            // and a triangle formed from two points on the circle will not encompass the radius)
            let playerTestPoint = { vX = playerRelativePosition.vX/2. ; vY = playerRelativePosition.vY/2. }
            // Anchor the triangle at the center
            let p1 = { vX = 0. ; vY = 0. }
            let quadrantIndex =
              vectors
              |> FSharp.Collections.Array.tryFindIndex (fun (p2,p3) ->
                isPointInTriangle p1 p2 p3 playerTestPoint
              )
              |> Option.defaultValue 0
            //e.BasicGameObject.SpriteIndex + quadrantIndex
            e.BaseSpriteIndexForState + quadrantIndex
          )
          |> Option.defaultValue e.BaseSpriteIndexForState //e.BasicGameObject.SpriteIndex
        else
          e.DeathSpriteIndexes.[e.CurrentAnimationFrame]
    
    let planeX = game.Camera.Plane.vX
    let planeY = game.Camera.Plane.vY
    let dirX = game.Camera.Direction.vX
    let dirY = game.Camera.Direction.vY
    let hitDetectionLeft = int width / 2 - firingTolerance / 2
    let hitDetectionRight = hitDetectionLeft + firingTolerance
    // The fold is here to return the last object in the centre of the viewport for later firing hit collision
    let spriteIndexInCenterOfViewportOption =
      game.GameObjects
      |> List.fold (fun (outerGameObjectHitIndex,gameObjectIndex) sprite ->
        let spriteX = sprite.BasicGameObject.Position.vX - game.Camera.Position.vX
        let spriteY = sprite.BasicGameObject.Position.vY - game.Camera.Position.vY
        let invDet = 1.0 / (planeX * dirY - planeY * dirX)
        let transformX = invDet * (dirY * spriteX - dirX * spriteY)
        let transformY = invDet * (-planeY * spriteX + planeX * spriteY)
        let spriteScreenX = float((width / 2.) * (1. + transformX / transformY))
        
        //using 'transformY' instead of the real distance prevents fisheye - must have been my mistake back in 92!
        let spriteHeight = int (abs (height / transformY))
        let drawStartY = max 0 (-spriteHeight/2 + int height / 2)
        let drawEndY = min (int height - 1) (spriteHeight/2 + int height/2)
        
        let spriteWidth = int (abs (height / transformY))
        let drawStartX = max 0 (-spriteWidth / 2 + int spriteScreenX)
        let drawEndX = min (int width-1) (spriteWidth / 2 + int spriteScreenX)
        let spriteIndex = orientedSpriteIndex sprite
        let spriteOffsets = getSpriteOffsets spriteIndex
        let spriteTexture = sprites.[spriteIndex]
        let lineHeight = height / transformY
        let step = 1.0 * textureHeight / lineHeight
        
        if drawStartX >= 0 && drawEndX < int width then
          (
            {drawStartX..(drawEndX-1)}
            |> Seq.fold (fun gameObjectHitIndex stripe ->
              let textureX = int(256. * (float stripe - (float (-spriteWidth) / 2. + spriteScreenX)) * 64. / float spriteWidth) / 256
              if transformY > 0. && stripe > 0 && stripe < int width && transformY < wallRenderResult.ZIndexes.[stripe] then
                if textureX >= int spriteOffsets.FirstColumn && textureX <= int spriteOffsets.LastColumn then
                  {drawStartY..(drawEndY-1)}
                  |> Seq.iter (fun y ->
                    let texY = int ((float y - (height)/2. + lineHeight/2.) * step)
                    let color = getPixel spriteTexture textureX texY
                    if not (color |> isTransparent) then
                      setPixel color stripe y
                  )
                  if sprite.BasicGameObject.CollidesWithBullets && stripe > hitDetectionLeft && stripe < hitDetectionRight  then
                    gameObjectIndex |> Some
                  else
                    gameObjectHitIndex
                else
                  gameObjectHitIndex
              else
                gameObjectHitIndex
            ) outerGameObjectHitIndex,
            gameObjectIndex+1
          )
        else
          None,0
      ) (None,0)
      |> fst
    { wallRenderResult with SpriteInFrontOfPlayerIndexOption = spriteIndexInCenterOfViewportOption }
    
module Weapons =
  let drawPlayerWeapon canvasWidth canvasHeight drawImage game =
    let height = canvasHeight
    let xPos = canvasWidth/2. - height/2.
    let weapon = game.Player.Weapons.[game.Player.CurrentWeaponIndex]
    drawImage weapon.CurrentSprite xPos 0. height height
