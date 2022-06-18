module App.Model

open App.PlatformModel

[<Measure>] type hp
[<Measure>] type points
[<Measure>] type ms
[<Measure>] type radians
[<Measure>] type degrees

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
  // we use this for sorting sprites, we don't need the square root
  member this.UnsquaredDistanceFrom (other:Vector2D) =
    ((this.vX - other.vX) * (this.vX - other.vX) + (this.vY - other.vY) * (this.vY - other.vY))
  member this.Normalize () =
    let distance = sqrt (this.vX * this.vX + this.vY * this.vY)
    { vX = this.vX / distance ; vY = this.vY / distance }
  member this.Abs () =
    { vX = abs this.vX ; vY = abs this.vY }
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
  
let radiansToDegrees (r:float<radians>) = r * 180.<degrees> / (System.Math.PI * 1.<radians>)
let degreesToRadians (d:float<degrees>) = d * System.Math.PI / 180.<degrees> * 1.<radians> 

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
  }
  
[<RequireQualifiedAccess>]
type Cell =
  | Wall of Wall
  | Door of int // index of the door in the doors array
  | TurningPoint of Vector2D
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
  | Path
  | Pain
  | Shoot
  | Chase
  | Die
  | Dead
    
type BasicGameObject =
  { Position: Vector2D
    PlayerRelativePosition: Vector2D
    UnsquaredDistanceFromPlayer: float
    SpriteIndex: int
    CollidesWithBullets: bool
  }

type Enemy =
  { EnemyType: EnemyType
    BasicGameObject: BasicGameObject
    DirectionVector: Vector2D option
    DeathSpriteIndexes: int list
    AttackSpriteIndexes: int list
    SpriteBlocks: int
    FramesPerBlock: int
    CurrentAnimationFrame: int
    TimeUntilNextAnimationFrame: float<ms>
    State: EnemyStateType
  }
  member this.StationarySpriteBlockIndex = this.BasicGameObject.SpriteIndex
  member this.MovementSpriteBlockIndex frame = this.BasicGameObject.SpriteIndex + frame*this.FramesPerBlock
  member this.NumberOfAnimationFrames = this.SpriteBlocks - 1
  member this.IsAlive = match this.State with | EnemyStateType.Dead | EnemyStateType.Die -> false | _ -> true
  member this.BaseSpriteIndexForState =
    match this.State with
    | EnemyStateType.Standing -> this.StationarySpriteBlockIndex
    | EnemyStateType.Path -> this.MovementSpriteBlockIndex this.CurrentAnimationFrame
    | _ -> this.BasicGameObject.SpriteIndex
  static member AnimationTimeForState state =
    match state with
    | EnemyStateType.Path -> 200.<ms>
    | _ -> 0.<ms>
  
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
    Plane0: Cell list list
    GameObjects: GameObject list
    PlayerStartingPosition: Camera
    // we store door state outside of the map and store an index to them in the map
    // when they move their state is animated regularly and (keeping things functional!) immutability updating the
    // map that often is expensive 
    Doors: DoorState list
  }

type Game =
  { Map: Cell list list
    GameObjects: GameObject list
    Player: Player
    Camera: Camera
    ControlState: ControlState
    IsFiring: bool
    TimeToNextWeaponFrame: float<ms> option
    Doors: DoorState list
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

let textureWidth = 64.
let textureHeight = 64.
// this is the width around the center of the screen that is included in the hit detection when a weapon is fired
let firingTolerance = 40
let deathAnimationFrameTime = 100.<ms>