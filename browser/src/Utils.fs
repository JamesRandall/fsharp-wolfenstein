module App.Utils
open Fable.SimpleHttp
open Browser

let loadAsset assetName = async {
  let! response =
    Http.request $"assets/{assetName}"
    |> Http.method GET
    |> Http.overrideResponseType ResponseTypes.ArrayBuffer
    |> Http.send
  return
    match response.content with
    | ResponseContent.ArrayBuffer arrayBuffer ->
      Ok arrayBuffer
    | _ ->
      console.error "Unable to load asset"
      Error ""
}

let log (message:string) = console.log message