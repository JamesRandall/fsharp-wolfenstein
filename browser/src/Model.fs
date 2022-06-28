module App.Model

open System.Collections.Generic
open App.PlatformModel

[<Measure>] type hp
[<Measure>] type points
[<Measure>] type ms
[<Measure>] type radians
[<Measure>] type degrees

[<RequireQualifiedAccess>]
type DifficultyLevel =
  | CanIPlayDaddy
  | DontHurtMe
  | BringEmOn
  | IAmDeathIncarnate

type WallRenderingResult =
  { ZIndexes: float list
    WallInFrontOfPlayer: int*int // x,y
    DistanceToWallInFrontOfPlayer: float
    SpriteInFrontOfPlayerIndexOption: int option
  }

type Vector2D =
  { vX: float
    vY: float
  }
  member x.CrossProduct = { vX = x.vY ; vY = -x.vX }
  static member (-) (a,b) = { vX = a.vX - b.vX ; vY = a.vY - b.vY }
  static member (+) (a,b) = { vX = a.vX + b.vX ; vY = a.vY + b.vY }
  static member (*) (a,b) = { vX = a.vX * b ; vY = a.vY * b }
  static member (/) (a,b) = { vX = a.vX / b ; vY = a.vY / b }
  // we use this for sorting sprites, we don't need the square root
  member this.UnsquaredDistanceFrom (other:Vector2D) =
    ((this.vX - other.vX) * (this.vX - other.vX) + (this.vY - other.vY) * (this.vY - other.vY))
  member this.Normalize () =
    let distance = sqrt (this.vX * this.vX + this.vY * this.vY)
    { vX = this.vX / distance ; vY = this.vY / distance }
  member this.Abs () = { vX = abs this.vX ; vY = abs this.vY }
  member this.Reverse () = { vX = this.vX * -1. ; vY = this.vY * -1. }
  member this.Rotate (angle:float<radians>) =
    let ca = cos (float angle)
    let sa = sin (float angle)
    { vX = ca * this.vX - sa * this.vY ; vY = sa * this.vX + ca * this.vY }
  member this.StringValue = $"{{ vX: {this.vX}, vY: {this.vY} }}"
  member this.IsBetween a b =
    (a.vY * b.vX - a.vX * b.vY) * (a.vY * this.vX - a.vX * this.vY) >= 0 &&
    (this.vX * b.vX - this.vX * b.vY) * (this.vY * a.vX - this.vX * a.vY) >= 0
  member this.Magnitude =
    sqrt (this.vX*this.vX + this.vY*this.vY)
  member a.AngleBetween b =
    System.Math.Acos(
      (a.vX * b.vX + a.vY * b.vY) / a.Magnitude * b.Magnitude
    ) * 1.<radians>
  member v.LimitToMapUnit _ =
    { vX = if v.vX > 1. then 1. elif v.vX < -1. then -1. else v.vX
      vY = if v.vY > 1. then 1. elif v.vY < -1. then -1. else v.vY
    }
  static member CreateFromMapPosition position =
    { vX = float (position |> fst) + 0.5
      vY = float (position |> snd) + 0.5
    }
  
module Direction =
  // east and west directions are flipped due to our renderer
  let north = { vX = 0. ; vY = -1. }
  let northEast = { vX = -1. ; vY = -1. }.Normalize()
  let east = { vX = -1. ; vY= 0. }
  let southEast = { vX = -1. ; vY = 1. }.Normalize()
  let south = { vX = 0. ; vY = 1. }
  let southWest = { vX = 1. ; vY = 1. }.Normalize()
  let west = { vX = 1. ; vY = 0. }
  let northWest = { vX = 1.; vY = -1. }.Normalize()
  
let radiansToDegrees (r:float<radians>) = r * 180.<degrees> / (System.Math.PI * 1.<radians>)
let degreesToRadians (d:float<degrees>) = d * System.Math.PI / 180.<degrees> * 1.<radians> 

exception MapDirectionException of string

[<RequireQualifiedAccess>]
type MapDirection =
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  | None
  static member Diagonal a b =
    match a,b with
    | East, North
    | North, East -> NorthEast
    | East, South
    | South, East -> SouthEast
    | West, South
    | South, West -> SouthWest
    | West, North
    | North, West -> NorthWest
    | _ -> raise (MapDirectionException $"No diagonal for {a},{b}")
  member x.ToDelta () =
    match x with
    | MapDirection.North -> 0,-1
    | MapDirection.NorthEast -> -1,-1
    | MapDirection.East -> -1,0
    | MapDirection.SouthEast -> -1,1
    | MapDirection.South -> 0,1
    | MapDirection.SouthWest -> 1,1
    | MapDirection.West -> 1,0
    | MapDirection.NorthWest -> 1,-1
    | MapDirection.None -> 0,0
  member x.ToVector () =
    match x with
    | MapDirection.North -> Some Direction.north
    | MapDirection.NorthEast -> Some Direction.northEast
    | MapDirection.East -> Some Direction.east
    | MapDirection.SouthEast -> Some Direction.southEast
    | MapDirection.South -> Some Direction.south
    | MapDirection.SouthWest -> Some Direction.southWest
    | MapDirection.West -> Some Direction.west
    | MapDirection.NorthWest -> Some Direction.northWest
    | MapDirection.None -> Option.None
  member x.Reverse () =
    match x with
    | MapDirection.North -> MapDirection.South
    | MapDirection.NorthEast -> MapDirection.SouthWest
    | MapDirection.East -> MapDirection.West
    | MapDirection.SouthEast -> MapDirection.NorthWest
    | MapDirection.South -> MapDirection.North
    | MapDirection.SouthWest -> MapDirection.NorthEast
    | MapDirection.West -> MapDirection.East
    | MapDirection.NorthWest -> MapDirection.SouthEast
    | MapDirection.None -> MapDirection.None

[<RequireQualifiedAccess>]
type Side =
  | NorthSouth
  | EastWest

[<RequireQualifiedAccess>]
type Wall =
  { NorthSouthTextureIndex: int
    EastWestTextureIndex: int
  }
  
[<RequireQualifiedAccess>]
type SoundEffect =
  | PlayerPistol
  | EnemyDeathAaarrrg
  | EnemyDeathAieeeeLow
  | EnemyDeathAieeeeHigh
  | DoorOpen
  | DoorClose
  | GuardGunshot
  static member All =
    [ SoundEffect.PlayerPistol
      SoundEffect.EnemyDeathAaarrrg
      SoundEffect.EnemyDeathAieeeeLow
      SoundEffect.EnemyDeathAieeeeHigh
      SoundEffect.DoorOpen
      SoundEffect.DoorClose
      SoundEffect.GuardGunshot
    ]
  
[<RequireQualifiedAccess>]
type DoorDirection =
  | NorthSouth
  | EastWest
  
[<RequireQualifiedAccess>]
type DoorStatus =
  | Opening
  | Closing
  | Open
  | Closed
  
type DoorState =
  { TextureIndex: int
    DoorDirection: DoorDirection
    Status: DoorStatus
    Offset: float
    TimeRemainingInAnimationState: float<ms>
    MapPosition: (int*int)
    AreaOne: int
    AreaTwo: int
  }
  
[<RequireQualifiedAccess>]
type Cell =
  | Wall of Wall
  | Door of int // index of the door in the doors array
  | TurningPoint of MapDirection
  | Empty
  
type EnemyType =
  | Guard
  | Officer
  | SS
  | Dog
  | Zombie
  | FakeAdolf
  | Adolf
  | Fettgesicht
  | Schabbs
  | Gretel
  | Hans
  | Otto
  | Ghost
  
[<RequireQualifiedAccess>]
type EnemyStateType =
  | Standing
  | Ambushing
  | Attack
  | Path of int*int
  | Pain
  | Shoot
  | Chase of int*int // co-ordinates we are moving to as part of the chase
  | Die
  | Dead
    
type BasicGameObject =
  { Position: Vector2D
    PlayerRelativePosition: Vector2D
    UnsquaredDistanceFromPlayer: float
    SpriteIndex: int
    CollidesWithBullets: bool
  }
  member this.MapPosition = (int this.Position.vX),(int this.Position.vY)

// TODO: I need to do some clean up in here now I know how the sprites work - we have two types, the movement block based
// ones and indexed movement ones. Needs rationalising.
type Enemy =
  { EnemyType: EnemyType
    BasicGameObject: BasicGameObject
    Direction: MapDirection
    //DirectionVector: Vector2D option
    DeathSpriteIndexes: int list
    AttackSpriteIndexes: int list
    SpriteBlocks: int
    FramesPerBlock: int
    CurrentAnimationFrame: int
    TimeUntilNextAnimationFrame: float<ms>
    State: EnemyStateType
    IsFirstAttack: bool // shoud start at true and be set to false after first attack
    FireAtPlayerRequired: bool // set to true during the update and AI loop if the enemy has fired at the player that frame
    HitPoints: int
  }
  member this.DirectionVector = this.Direction.ToVector()
  member this.StationarySpriteBlockIndex = this.BasicGameObject.SpriteIndex
  member this.MovementSpriteBlockIndex frame = this.BasicGameObject.SpriteIndex + frame*this.FramesPerBlock
  member this.NumberOfMovementAnimationFrames = this.SpriteBlocks - 1
  member this.IsAlive = match this.State with | EnemyStateType.Dead | EnemyStateType.Die -> false | _ -> true
  member this.BaseSpriteIndexForState =
    match this.State with
    | EnemyStateType.Standing -> this.StationarySpriteBlockIndex
    | EnemyStateType.Chase _
    | EnemyStateType.Path _ -> this.MovementSpriteBlockIndex this.CurrentAnimationFrame
    | EnemyStateType.Attack -> this.AttackSpriteIndexes.[this.CurrentAnimationFrame]
    | _ -> this.BasicGameObject.SpriteIndex
  static member AnimationTimeForState state =
    match state with
    | EnemyStateType.Attack -> 200.<ms>
    | EnemyStateType.Chase _ -> 100.<ms>
    | EnemyStateType.Path _ -> 200.<ms>
    | EnemyStateType.Dead | EnemyStateType.Die -> 100.<ms>
    | _ -> 0.<ms>
  member this.SpriteIndexForAnimationFrame =
    match this.State with
    | EnemyStateType.Attack -> this.AttackSpriteIndexes.[this.CurrentAnimationFrame]
    | EnemyStateType.Dead | EnemyStateType.Die -> this.DeathSpriteIndexes.[this.CurrentAnimationFrame]
    | _ -> this.StationarySpriteBlockIndex
  member this.AnimationFrames =
    match this.State with
    | EnemyStateType.Attack -> this.AttackSpriteIndexes.Length
    | EnemyStateType.Dead | EnemyStateType.Die -> this.DeathSpriteIndexes.Length
    | _ -> 1
  member this.IsBoss =
    match this.EnemyType with
    | EnemyType.Guard
    | EnemyType.Officer
    | EnemyType.Dog
    | EnemyType.SS
    | EnemyType.Zombie -> false
    | _ -> true
  // some enemies can become invisible I believe... need to explore further
  member this.IsVisible = true
  
[<RequireQualifiedAccess>]
type WeaponType =
  | Knife
  | Pistol
  | MachineGun
  | ChainGun
  
type PlayerWeapon =
  { Sprites: Texture list
    CurrentFrame: int
    Damage: int
    AutoRepeat: bool
  }
  member x.AnimationFrames = x.Sprites.Length
  member x.CurrentSprite = x.Sprites.[x.CurrentFrame]
  
[<RequireQualifiedAccess>]
type GameObject =
  | Treasure of BasicGameObject
  | Enemy of Enemy
  member x.BasicGameObject =
    match x with | GameObject.Treasure t -> t | GameObject.Enemy e -> e.BasicGameObject
  member x.UpdateBasicGameObject go =
    match x with
    | GameObject.Treasure _ -> GameObject.Treasure go
    | GameObject.Enemy e -> { e with BasicGameObject = go } |> GameObject.Enemy
  
type ControlState =
  | None          = 0b00000000
  | Forward       = 0b00000001
  | TurningLeft   = 0b00000010
  | TurningRight  = 0b00000100
  | StrafingLeft  = 0b00001000
  | StrafingRight = 0b00010000
  | Backward      = 0b00100000
  | Fire          = 0b01000000
  | Action        = 0b10000000

type Player =
  { Score: int<points>
    Health: int<hp>
    Radius: float // how much room does the player take up on the map
    CurrentWeaponIndex: int
    Ammunition: int
    Weapons: PlayerWeapon list
  }

type Camera =
  { Position: Vector2D
    Direction: Vector2D
    Plane: Vector2D
    FieldOfView: float
  }
    
type SpriteLayout =
  { Offset: uint32
    FirstColumn: uint32
    LastColumn: uint32
    PixelPoolOffset: uint32
  }
  
type RawMap =
  { MapSize: int
    Plane0: Fable.Core.JS.DataView
    Plane1: Fable.Core.JS.DataView
    Plane2: Fable.Core.JS.DataView
  }
  
// will type these when we know more about them - need to decompress and inspect firstx
type WolfensteinMap =
  { Width: int
    Height: int
    Map: Cell list list
    Areas: int list list
    NumberOfAreas: int
    GameObjects: GameObject list
    PlayerStartingPosition: Camera
    // we store door state outside of the map and store an index to them in the map
    // when they move their state is animated regularly and (keeping things functional!) immutability updating the
    // map that often is expensive 
    Doors: DoorState list
  }

type OverlayAnimation =
  { Red: byte
    Green: byte
    Blue: byte
    Opacity: float
    MaxOpacity: float
    OpacityDelta: float
    FrameLength: float<ms>
    TimeRemainingUntilNextFrame: float<ms>
  }
  static member Blood maxOpacity =
    let maxOpacity = max maxOpacity 0.2
    let totalAnimationTime = 75.<ms>
    let totalFrames = 10.
    let opacityDelta = maxOpacity / (totalFrames / 2.) // we go in and out     
    let frameLength = totalAnimationTime / totalFrames
    { Red = 0xFFuy//0x8Cuy
      Green = 0uy
      Blue = 0uy
      Opacity = opacityDelta
      OpacityDelta = opacityDelta
      MaxOpacity = maxOpacity
      FrameLength = frameLength
      TimeRemainingUntilNextFrame = frameLength
    }

[<RequireQualifiedAccess>]
type ViewportFilter =
  | None
  | Overlay of OverlayAnimation  

type CompositeArea =
  { Area: int
    ConnectedTo: int Set
  }

type Game =
  { Map: Cell list list
    Areas: int list list
    CompositeAreas: CompositeArea list
    GameObjects: GameObject list
    Player: Player
    Camera: Camera
    ControlState: ControlState
    IsFiring: bool
    TimeToNextWeaponFrame: float<ms> option
    Doors: DoorState list
    ViewportFilter: ViewportFilter
  }
  member this.PlayerMapPosition = (int this.Camera.Position.vX),(int this.Camera.Position.vY)
  member this.IsPlayerRunning = (this.ControlState &&& ControlState.Forward) = ControlState.Forward
  
type StatusBarGraphics =
  { Background: Texture
    HealthFaces: Texture array array
    Dead: Texture
    GrinFace: Texture
    GreyFace: Texture
  }
  
let textureWidth = 64.
let textureHeight = 64.
// this is the width around the center of the screen that is included in the hit detection when a weapon is fired
let firingTolerance = 40
let attackAnimationFrameTime = 200.<ms>