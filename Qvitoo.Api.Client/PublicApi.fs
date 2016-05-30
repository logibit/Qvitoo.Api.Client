module Qvitoo.Api.Client

open HttpFs
open Hopac
open NodaTime
open NodaTime.Text
open Chiron
open Chiron.Operators
open System
open System.Threading

/// A dependant variable.
type DVar<'a> = private { cell : 'a ref ; event : Event<'a> }

/// Operations on dependant variables.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DVar =

  /// Creates a DVar and initializes it with the specified value.
  let create (a:'a) : DVar<'a> =
    let e = new Event<'a>()
    let cell = ref (Unchecked.defaultof<'a>)
    e.Publish.Add <| fun a -> cell := a
    e.Trigger a
    { cell = cell ; event = e }

module Choice =

  let create v = Choice1Of2 v

  let createSnd v = Choice2Of2 v

  let map f = function
    | Choice1Of2 v -> Choice1Of2 (f v)
    | Choice2Of2 msg -> Choice2Of2 msg

  let mapSnd f = function
    | Choice1Of2 v -> Choice1Of2 v
    | Choice2Of2 v -> Choice2Of2 (f v)

  let bind (f : 'a -> Choice<'b, 'c>) (v : Choice<'a, 'c>) =
    match v with
    | Choice1Of2 v -> f v
    | Choice2Of2 c -> Choice2Of2 c

  let bindSnd (f : 'a -> Choice<'c, 'b>) (v : Choice<'c, 'a>) =
    match v with
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 x -> f x
    
  let fold f g =
    function
    | Choice1Of2 x -> f x
    | Choice2Of2 y -> g y
    
  let apply f v =
    bind (fun f' ->
      bind (fun v' ->
        create (f' v')) v) f

  let applySnd f v =
    bind (fun f' ->
      bindSnd (fun v' ->
        createSnd (f' v')) v) f

  let lift2 f v1 v2 =
    apply (apply (create f) v1) v2

  let lift3 f v1 v2 v3 =
    apply (apply (apply (create f) v1) v2) v3

  let lift4 f v1 v2 v3 v4 =
    apply (apply (apply (apply (create f) v1) v2) v3) v4

  let lift5 f v1 v2 v3 v4 v5 =
    apply (apply (apply (apply (apply (create f) v1) v2) v3) v4) v5

  let ofOption onMissing = function
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 onMissing

  let inject f = function
    | Choice1Of2 x -> f x; Choice1Of2 x
    | Choice2Of2 x -> Choice1Of2 x

  let injectSnd f = function
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 x -> f x; Choice2Of2 x

  module Operators =

    let inline (>>=) m f =
      bind f m

    let inline (>>-) m f = // snd
      bindSnd f m

    let inline (=<<) f m =
      bind f m

    let inline (-<<) f m = // snd
      bindSnd f m

    let inline (>>*) m f =
      inject f m

    let inline (>>@) m f = // snd
      injectSnd f m

    let inline (<*>) f m =
      apply f m

    let inline (<!>) f m =
      map f m

    let inline (>!>) m f =
      map f m

    let inline (<@>) f m = // snd
      mapSnd f m

    let inline (>@>) m f = // snd
      mapSnd f m

    let inline ( *>) m1 m2 =
      lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
      lift2 (fun x _ -> x) m1 m2

module Dtos =

  type Error = unit

  type TargetKey = string

  type Receipt =
    { description : string
      id          : string
      timestamp   : Instant
      total       : decimal
      totalTax    : decimal
      isPending   : bool
      isPublished : bool
      finishedTargets : Set<TargetKey> }

    static member FromJson (_ : Receipt) =
      (fun desc id ts tot totTax ->
        { description = desc
          id = id
          timestamp = InstantPattern.GeneralPattern.Parse(ts).Value
          total     = tot
          totalTax  = totTax
          isPending = false
          isPublished = false
          finishedTargets = Set.empty })
      <!> Json.read "description"
      <*> Json.read "id"
      <*> Json.read "timestamp"
      <*> Json.read "total"
      <*> Json.read "totalTax"


  type TargetInstance =
    { id : string
      principalId : string
      key : string
      description : string
      settings : Map<string, string> }

    static member FromJson (_ : TargetInstance) =
      (fun id pid key desc s ->
        { id = id
          principalId = pid
          key = key
          description = desc
          settings = s })
      <!> Json.read "id"
      <*> Json.read "principalId"
      <*> Json.read "key"
      <*> Json.read "description"
      <*> Json.read "settings"

module Api =
  open Dtos

  module Receipts =

    let getAll conf filters : Stream<Choice<Receipt, Error>> =
      Stream.nil

    let enableTarget conf (ti : TargetInstance) : Job<Choice<unit, Error>> =
      Job.result (Choice.create ())

    let configureTarget conf (ti : TargetInstance) settings : Job<Choice<unit, Error>> =
      Job.result (Choice.create ())

    let publish conf (r : Receipt) : Job<Choice<unit, Error>> =
      Job.result (Choice.create ())

  module TargetInstances =

    let getAll conf filters : Stream<Choice<TargetInstance, Error>> =
      Stream.nil