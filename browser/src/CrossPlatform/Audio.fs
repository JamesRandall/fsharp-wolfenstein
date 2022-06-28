module App.Audio
open App.Model
open App.PlatformAudio

let private random = System.Random()

// TODO: Needs much more work
let calculateVolumeAtDistance (distance:float) =
  // can't here half way across the map
  let maximumDistanceAtWhichSoundCanBeHeard = 32.
  if distance > maximumDistanceAtWhichSoundCanBeHeard then
    0.
  else
    let maxDistanceAsRadians = System.Math.PI / 2.
    let distancePerRadian = maxDistanceAsRadians / maximumDistanceAtWhichSoundCanBeHeard
    let volume = cos (distancePerRadian * distance)
    Utils.log $"Distance: {distance}, Volume: {volume}"
    volume

let playRandomEnemyDeathSoundEffectAtVolume (game:Game) (position:Vector2D) =
  [ SoundEffect.EnemyDeathAaarrrg
    SoundEffect.EnemyDeathAieeeeHigh
    SoundEffect.EnemyDeathAieeeeLow
  ].[random.Next(3)]
  |> playSoundEffectAtVolume (calculateVolumeAtDistance (game.Camera.Position - position).Magnitude)
  
let playAttackSound (game:Game) (position:Vector2D) enemyType =
  match enemyType with
  | EnemyType.Guard -> SoundEffect.GuardGunshot
  // TODO: capture other firing (and dog chewing) sound
  | _ -> SoundEffect.GuardGunshot
  |> playSoundEffectAtVolume (calculateVolumeAtDistance (game.Camera.Position - position).Magnitude)
  
let playSoundEffect (game:Game) (position:Vector2D) soundEffect =
  soundEffect
  |> playSoundEffectAtVolume (calculateVolumeAtDistance (game.Camera.Position - position).Magnitude)