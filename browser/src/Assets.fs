module App.Assets

open App.Model

let isShareware = true
let extension = if isShareware then "WL1" else "WL6"

let GAMEMAPS = $"GAMEMAPS.{extension}"
let MAPHEAD = $"MAPHEAD.{extension}"
let VSWAP = $"VSWAP.{extension}"

let isSharewareSprite index =
  (index <= 186 || (index >= 296 && index <=306) || (index >= 408 && index<= 435))
  
let audioFilename extension soundEffect =
  let filename =
    let prefix =
      match soundEffect with
      | SoundEffect.PlayerPistol -> "Pistol"
      | SoundEffect.EnemyDeathAaarrrg -> "Aarrrgh (enemy death)"
      | SoundEffect.EnemyDeathAieeeeLow -> "Aieeee1 (enemy death)"
      | SoundEffect.EnemyDeathAieeeeHigh -> "Aieeee2 (enemy death)"
      | SoundEffect.DoorOpen -> "DoorOpen"
      | SoundEffect.DoorClose -> "DoorClose"
    $"{prefix}.{extension}"
  filename