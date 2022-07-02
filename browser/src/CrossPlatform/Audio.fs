module App.Audio
open App.Model
open App.PlatformAudio

let private random = System.Random()

module List =
  let random (set:List<'a>) = set.[random.Next(set.Length)]

let isSoundInArea (game:Game) (position:Vector2D) =
  let soundX, soundY = int position.vX,int position.vY
  let playerX, playerY = game.PlayerMapPosition
  let soundArea = game.Areas.[soundY].[soundX]
  let playerArea = game.Areas.[playerY].[playerX]
  if soundArea = -1 || playerX = -1 then
    Utils.log "Unable to find area for sound"
    true
  else
    let playerCompositeArea = game.CompositeAreas.[playerArea]
    playerCompositeArea.ConnectedTo |> Set.contains soundArea

let calculateVolumeAtDistance (game:Game) (position:Vector2D) =
  let distance = (game.Camera.Position - position).Magnitude
  // can't here half way across the map
  let maximumDistanceAtWhichSoundCanBeHeard = 16.
  if distance > maximumDistanceAtWhichSoundCanBeHeard then
    0.
  else
    // we modify the volume if their is a closed door between the player and the source of the sound
    let areaModifier =
      if isSoundInArea game position then id else (fun input -> max 0. (input - 0.3))
    let volume =
      if distance <= 1. then 1. else 1. - System.Math.Log(distance, maximumDistanceAtWhichSoundCanBeHeard)
      |> areaModifier
    volume

let playRandomEnemyDeathSoundEffectAtVolume (game:Game) (position:Vector2D) =
  [ SoundEffect.Aarggh
    SoundEffect.Aieeee
  ] |> List.random
  |> playSoundEffectAtVolume (calculateVolumeAtDistance game position)
  
let playAttackSound (game:Game) (position:Vector2D) enemyType =
  match enemyType with
  | EnemyType.Guard -> SoundEffect.GuardPistol
  // TODO: capture other firing (and dog chewing) sound
  | _ -> SoundEffect.GuardPistol
  |> playSoundEffectAtVolume (calculateVolumeAtDistance game position)
  
let playSoundEffect (game:Game) (position:Vector2D) soundEffect =
  soundEffect
  |> playSoundEffectAtVolume (calculateVolumeAtDistance game position)