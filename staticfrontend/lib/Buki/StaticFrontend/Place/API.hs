module Buki.StaticFrontend.Place.API where

import Buki.StaticFrontend.Core.Preludes.API
import Buki.Backend.Auth (AuthorizationPermission(BookManagement))

import Data.Text (Text)

type ListPlaces =
  AuthProtect (ReqAuthorizedUser '[ 'BookManagement])
  :> "places"
  :> Get '[HTML] Html

type CreatePlace =
  AuthProtect (ReqAuthorizedUser '[ 'BookManagement])
  :> "places"
  :> ReqBody '[FormUrlEncoded] (FormValidation PlaceData)
  :> Post '[HTML] Html

type RenamePlace =
  AuthProtect (ReqAuthorizedUser '[ 'BookManagement])
  :> "places" :> Capture "placeId" PlaceId
