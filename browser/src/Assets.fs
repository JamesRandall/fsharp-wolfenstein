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
      | SoundEffect.UttGuards -> "000_UttGuards"
      | SoundEffect.Dog -> "001_Dog"
      | SoundEffect.DoorClose -> "002_DoorClose"
      | SoundEffect.DoorOpen -> "003_DoorOpen"
      | SoundEffect.PlayerMachineGun -> "004_PlayerMachineGun"
      | SoundEffect.PlayerPistol -> "005_PlayerPistol"
      | SoundEffect.PlayerChainGun -> "006_PlayerChainGun_Question"
      | SoundEffect.Hoofafo -> "007_Hooffafo"
      | SoundEffect.GutenTag -> "008_GutenTag"
      | SoundEffect.Mutti -> "009_Mutti"
      | SoundEffect.GuardChainGun -> "010_GuardChainGun_Question"
      | SoundEffect.GuardMachineGun -> "011_GuardMachineGun_Question"
      | SoundEffect.Aarggh -> "012_Aaarggh"
      | SoundEffect.Aieeee -> "013_Aieeee"
      | SoundEffect.Ooof -> "014_Ooof"
      | SoundEffect.SecretDoor -> "015_SecretDoor"
      | SoundEffect.MeinLeben -> "026_MeinLeben"
      | SoundEffect.GuardPistol -> "027_GuardPistol"
      | SoundEffect.BubblesQuestion -> "028_Bubbles_Question"
      | SoundEffect.VictoryYeah -> "057_VictoryYeah"
      | SoundEffect.Tick -> "095_Tick"
    $"{prefix}.{extension}"
  filename