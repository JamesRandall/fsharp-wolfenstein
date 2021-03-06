namespace App.Render

open App
open App.Model
open App.PlatformModel
  
module Walls =
  open App
  
  let draw width height setPixel getTextureStripOffset getTextureColor game =
    let posX = game.Camera.Position.vX
    let posY = game.Camera.Position.vY
    let viewportWidth = width
    let viewportHeight = height
    
    let initialWallRenderResult = { IsDoorInFrontOfPlayer = false ; WallInFrontOfPlayer = -1,-1 ; DistanceToWallInFrontOfPlayer = -1 ; ZIndexes = [] ; SpriteInFrontOfPlayerIndexOption = None }
    let viewportStart = 0.
    let viewportEnd = viewportWidth-1.
    {viewportStart..viewportEnd}
    |> Seq.fold(fun previousResult viewportX ->
      let cameraX = (2. * viewportX / viewportWidth) - 1.
      let rayDirection = {
        vX = game.Camera.Direction.vX + game.Camera.Plane.vX * cameraX
        vY = game.Camera.Direction.vY + game.Camera.Plane.vY * cameraX
      }
      let setup () = false,posX, posY, rayDirection
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
      
      let renderedPartOfDoor = 
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
          false
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
          true
        | _ -> false
      
      // set the zIndex for the strip
      { previousResult with
          ZIndexes = previousResult.ZIndexes @ [perpendicularWallDistance]
          WallInFrontOfPlayer = if viewportX = viewportWidth/2. then hitMapX,hitMapY else previousResult.WallInFrontOfPlayer
          DistanceToWallInFrontOfPlayer =
            if viewportX = viewportWidth/2. then perpendicularWallDistance else previousResult.DistanceToWallInFrontOfPlayer
          IsDoorInFrontOfPlayer =
            if viewportX = viewportWidth/2. then renderedPartOfDoor else previousResult.IsDoorInFrontOfPlayer
      } 
    ) initialWallRenderResult
    
module Objects =
  let draw width height getPixel setPixel isTransparent getSpriteOffsets game (sprites:Texture array) (wallRenderResult:WallRenderingResult) =
    let orientedSpriteIndex gameObject =
      match gameObject with
      | GameObject.Static t -> t.SpriteIndex
      | GameObject.Enemy e ->
        if e.IsAlive then
          e.DirectionVector
          |> Option.bind(fun directionVector ->
            match e.State with
            | EnemyStateType.Die
            | EnemyStateType.Pain _
            | EnemyStateType.Attack
            | EnemyStateType.Shoot -> None
            | _ ->
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
                  App.Ray.isPointInTriangle p1 p2 p3 playerTestPoint
                )
                |> Option.defaultValue 0
              //e.BasicGameObject.SpriteIndex + quadrantIndex
              e.BaseSpriteIndexForState + quadrantIndex |> Some
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
        let spriteTexture = sprites.[spriteIndex]
        let lineHeight = height / transformY
        let step = 1.0 * textureHeight / lineHeight
        
        if drawStartX >= 0 && drawEndX < int width then
          (
            {drawStartX..(drawEndX-1)}
            |> Seq.fold (fun gameObjectHitIndex stripe ->
              let textureX = int(256. * (float stripe - (float (-spriteWidth) / 2. + spriteScreenX)) * 64. / float spriteWidth) / 256
              if transformY > 0. && stripe > 0 && stripe < int width && transformY < wallRenderResult.ZIndexes.[stripe] then
                // TODO: we should restore this as its a decent optimisation for some sprites
                //if textureX >= int spriteOffsets.FirstColumn && textureX <= int spriteOffsets.LastColumn then
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
                //else
                //  gameObjectHitIndex
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
  let drawPlayerWeapon canvasWidth canvasHeight getPixel setPixel isTransparent game =
    let weapon = game.Player.Weapons.[game.Player.CurrentWeaponIndex]
    let aspectRatio = canvasHeight / float weapon.CurrentSprite.Height
    let targetWidth = aspectRatio * float weapon.CurrentSprite.Width
    let targetHeight = canvasHeight
    let xPos = canvasWidth/2. - float targetWidth / 2.
    let scaleX = targetWidth / float weapon.CurrentSprite.Width
    let scaleY = targetHeight / float weapon.CurrentSprite.Height
    
    {0..(weapon.CurrentSprite.Width-1)}
    |> Seq.iter(fun spriteX ->
      {0..(weapon.CurrentSprite.Height-1)}
      |> Seq.iter(fun spriteY ->
        let color = getPixel weapon.CurrentSprite spriteX spriteY
        if not (color |> isTransparent) && color <> 0ul then
          let fromX = int (xPos + float spriteX * scaleX)
          let toX = fromX + (int scaleX)
          let fromY = int (float spriteY * scaleY)
          let toY = fromY + (int scaleY)
          {fromX..toX} |> Seq.iter(fun outX -> {fromY..toY} |> Seq.iter(fun outY -> setPixel color outX outY))
      )
    )

module StatusBar =
  let drawStatusBar (statusBarGraphics:StatusBarGraphics) drawImage game =
    let playerHp = game.Player.Health
    drawImage statusBarGraphics.Background 0. 0.
    let healthFace =
      if playerHp > 0<hp> then        
        let index = max 0 (statusBarGraphics.HealthFaces.Length - 1 - (playerHp / 16 |> int))
        statusBarGraphics.HealthFaces.[index].[game.Player.CurrentFaceIndex]
      else
        statusBarGraphics.Dead
    drawImage healthFace 128.0 1.0
    drawImage statusBarGraphics.Weapons.[game.Player.CurrentWeapon.StatusBarImageIndex] 247.0 3.0
    
    let drawNumber startX maximumLength (value:int) =
      let textY = 13.
      let textWidth = 8.
      let numberAsString = sprintf "%d" (max 0 value)
      let healthPercentageString = System.String(' ', maximumLength-numberAsString.Length) + numberAsString
      let getFontImage character =
        statusBarGraphics.Font.[if character = ' ' then 10 else ((character |> int) - ('0' |> int))]
      healthPercentageString
      |> Seq.fold (fun x value ->
        drawImage (value |> getFontImage) x textY
        x + textWidth
      ) startX
      |> ignore
      
    drawNumber 160. 3 (playerHp |> int)
    drawNumber 104. 1 (game.Player.Lives |> int)
    drawNumber 40. 6 (game.Player.Score |> int)
    drawNumber 15. 1 (game.Level |> int)
    drawNumber 209 2 (game.Player.Ammunition |> int)
    
module Dissolve =
  let render setPixel game =
    match game.PixelDissolver with
    | Some pixelDissolver ->
      pixelDissolver.DrawnPixels
      |> List.iter(fun (x,y) ->
        let fromX = int (float x * pixelDissolver.PixelSize)
        let toX = int (float fromX + pixelDissolver.PixelSize - 1.)
        let fromY = int (float y * pixelDissolver.PixelSize)
        let toY = int (float fromY + pixelDissolver.PixelSize - 1.)
        {fromX..toX}
        |> Seq.iter(fun zx ->
          {fromY..toY}
          |> Seq.iter(fun zy ->
            setPixel 0xFF0000BCul zx zy
          )
        )
      )
    | None -> ()
    
