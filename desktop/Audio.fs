module App.Audio
open System
open System.Buffers.Binary
open Microsoft.FSharp.NativeInterop
open Silk.NET.OpenAL
open App.Model

exception AudioException of string

let private random = Random()

let private soundEffects =
  // Initialise AL
  let alc = ALContext.GetApi()
  let al = AL.GetApi()
  let device = alc.OpenDevice("")
  let context = alc.CreateContext(device, NativePtr.nullPtr);
  alc.MakeContextCurrent(context) |> ignore
  al.GetError() |> ignore
  
  let audioAssetName filename =
    sprintf "Sounds.%s" filename  
  
  let createAudioPlayer path =
    let bytes = Utils.loadAssetBytes path
    if char bytes[0] <> 'R' || char bytes[1] <> 'I' || char bytes[2] <> 'F' || char bytes[3] <> 'F' then
      raise (AudioException "Invalid file format")
    // we don't need the chunk size
    //let chunkSize = BinaryPrimitives.ReadInt32LittleEndian(bytes[4..7])
    if char bytes[8] <> 'W' || char bytes[9] <> 'A' || char bytes[10] <> 'V' || char bytes[11] <> 'E' then
      raise (AudioException "Invalid file format")
    
    let mutable numChannels = -1s
    let mutable sampleRate = -1
    let mutable byteRate = -1
    let mutable blockAlign = -1s
    let mutable bitsPerSample = -1s
    let mutable format = BufferFormat.Mono8
    
    
    let source = al.GenSource()
    let buffer = al.GenBuffer()
    al.SetSourceProperty(source, SourceBoolean.SourceRelative, true)
    
    Seq.initInfinite(fun _ -> ())
    |> Seq.scan(fun index _ ->
      let identifier = $"{char bytes[index]}{(char) bytes[index+1]}{(char) bytes[index+2]}{(char) bytes[index+3]}"
      let index = index + 4
      let size = BinaryPrimitives.ReadInt32LittleEndian(bytes[index..index+4-1])
      let index = index + 4
      if identifier = "fmt " then
        if size <> 16 then raise (AudioException "Invalid subchunk size")
        let audioFormat = BinaryPrimitives.ReadInt16LittleEndian(bytes[index..index+1])
        if audioFormat <> 1s then raise (AudioException "Unknown audio format")
        let index = index + 2
        numChannels <- (BinaryPrimitives.ReadInt16LittleEndian(bytes[index..index+1]))
        let index = index + 2
        sampleRate <- (BinaryPrimitives.ReadInt32LittleEndian(bytes[index..index+3]))
        let index = index + 4
        byteRate <- (BinaryPrimitives.ReadInt32LittleEndian(bytes[index..index+3]))
        let index = index + 4
        blockAlign <- (BinaryPrimitives.ReadInt16LittleEndian(bytes[index..index+1]))
        let index = index + 2
        bitsPerSample <- (BinaryPrimitives.ReadInt16LittleEndian(bytes[index..index+1]))
        let index = index + 2
        
        if numChannels = 1s then
          if bitsPerSample = 8s then
            format <- BufferFormat.Mono8
          elif bitsPerSample = 16s then
            format <- BufferFormat.Mono16
          else raise (AudioException $"Can't play {bitsPerSample} bits per sample")
        elif numChannels = 2s then
          if bitsPerSample = 8s then
            format <- BufferFormat.Stereo8
          elif bitsPerSample = 16s then
            format <- BufferFormat.Stereo16
          else raise (AudioException $"Can't play {bitsPerSample} bits per sample")
        else
          raise (AudioException $"Can't play {numChannels} channels")
        index
      elif identifier = "data" then
        let data = bytes[44..44+size-1]
        let index = index + size
        use ptr = fixed data
        let voidPtr = ptr |> NativePtr.toVoidPtr
        al.BufferData(buffer, format, voidPtr, size, sampleRate)
        index
      elif identifier = "JUNK" then
        // apparently used to align things
        index + size
      elif identifier = "iXML" then
        index + size
      else
        // unknown chunk format
        index + size
    ) 12
    |> Seq.takeWhile(fun index -> index + 4 < bytes.Length)
    |> Seq.last
    |> ignore
    
    let playSound (volume:float) =
      al.SetSourceProperty(source, SourceInteger.Buffer, buffer)
      al.SourcePlay(source)
      
    playSound
      
  
  [ SoundEffect.PlayerPistol
    SoundEffect.EnemyDeathAaarrrg
    SoundEffect.EnemyDeathAieeeeLow
    SoundEffect.EnemyDeathAieeeeHigh
    SoundEffect.DoorOpen
    SoundEffect.DoorClose
  ]
  |> List.map (fun soundEffectType -> soundEffectType,soundEffectType |> Assets.audioFilename "wav" |> audioAssetName)
  |> List.map (fun (soundEffectType,path) -> soundEffectType,(createAudioPlayer path))
  |> Map.ofList

let playSoundEffectAtVolume volume soundEffect =
  match soundEffect |> soundEffects.TryGetValue with
  | true,player -> player volume
  | _ -> Utils.log $"Missing sound effect {soundEffect}"
  
let playSoundEffect soundEffect = playSoundEffectAtVolume 100. soundEffect

let playRandomEnemyDeathSoundEffectAtVolume volume =
  [ SoundEffect.EnemyDeathAaarrrg
    SoundEffect.EnemyDeathAieeeeHigh
    SoundEffect.EnemyDeathAieeeeLow
  ].[random.Next(3)]
  |> playSoundEffectAtVolume volume

