module App.PlatformAudio

open Fable.Core
open Fable.Core.JsInterop
open App.Model

[<Emit("new Audio($0)")>]
let createAudio path : obj = jsNative

let private createOverlayingAudioPlayer path =
  let audioPlayers =
    {0..4}
    |> Seq.map(fun _ -> createAudio path)
    |> Seq.toArray
  let mutable currentIndex = 0
  let play volume =
    audioPlayers.[currentIndex]?volume <- volume
    audioPlayers.[currentIndex]?play()
    currentIndex <- if currentIndex = audioPlayers.Length-1 then 0 else currentIndex+1
  play
    
let private soundEffects =
  let audioPath filename = sprintf "assets/sounds/%s" (System.Uri.EscapeUriString filename)  
  
  SoundEffect.All
  |> List.map (fun soundEffectType -> soundEffectType,soundEffectType |> App.Assets.audioFilename "mp3" |> audioPath)
  |> List.map (fun (soundEffectType,path) -> soundEffectType,(createOverlayingAudioPlayer path))
  |> Map.ofList
  
let playSoundEffect soundEffect =
  let audioObject = soundEffects.[soundEffect]
  audioObject 1.0
  
let playSoundEffectAtVolume (volume:float) soundEffect  =
  let audioObject = soundEffects.[soundEffect]
  audioObject volume