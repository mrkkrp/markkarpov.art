{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}

module PhotoMetadata
  ( PhotoMetadata,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Proxy
import Data.Scientific qualified as Scientific
import Data.Text (Text)
import Data.Text qualified as T

data PhotoMetadata = forall (camera :: Camera). (KnownCamera camera) => PhotoMetadata
  { camera :: Proxy camera,
    lens :: Lens camera,
    film :: Film
  }

instance FromJSON PhotoMetadata where
  parseJSON = withObject "photo metadata" $ \o -> do
    cameraValue <- o .: "camera"
    film <- o .: "film"
    let camera = Proxy
    case cameraValue of
      PentaxSV -> do
        lens :: Lens 'PentaxSV <- o .: "lens"
        pure PhotoMetadata {..}
      NikonF2 -> do
        lens :: Lens 'NikonF2 <- o .: "lens"
        pure PhotoMetadata {..}
      Hasselblad503cx -> do
        lens :: Lens 'Hasselblad503cx <- o .: "lens"
        pure PhotoMetadata {..}
      MamiyaRB67ProS -> do
        lens :: Lens 'MamiyaRB67ProS <- o .: "lens"
        pure PhotoMetadata {..}
      MamiyaUniversalPress -> do
        lens :: Lens 'MamiyaUniversalPress <- o .: "lens"
        pure PhotoMetadata {..}
      HarmanTitan4x5 -> do
        lens :: Lens 'HarmanTitan4x5 <- o .: "lens"
        pure PhotoMetadata {..}

instance ToJSON PhotoMetadata where
  toJSON PhotoMetadata {..} =
    toJSON $
      cameraPretty camera <> " + " <> lensPretty lens <> " + " <> filmPretty film

data Camera
  = PentaxSV
  | NikonF2
  | Hasselblad503cx
  | MamiyaRB67ProS
  | MamiyaUniversalPress
  | HarmanTitan4x5

instance FromJSON Camera where
  parseJSON = withText "camera id" $ \txt ->
    case txt of
      "pentaxsv" -> pure PentaxSV
      "nikonf2" -> pure NikonF2
      "hasselblad" -> pure Hasselblad503cx
      "mamiyarb67" -> pure MamiyaRB67ProS
      "mamiyapress" -> pure MamiyaUniversalPress
      "titan" -> pure HarmanTitan4x5
      _ -> fail $ "unknown camera id: " ++ T.unpack txt

class KnownCamera camera where
  cameraPretty :: Proxy camera -> Text

instance KnownCamera PentaxSV where
  cameraPretty Proxy = "Pentax SV"

instance KnownCamera NikonF2 where
  cameraPretty Proxy = "Nikon F2AS"

instance KnownCamera Hasselblad503cx where
  cameraPretty Proxy = "Hasselblad 503cx"

instance KnownCamera MamiyaRB67ProS where
  cameraPretty Proxy = "Mamiya RB67 Pro-S"

instance KnownCamera MamiyaUniversalPress where
  cameraPretty Proxy = "Mamiya Universal Press"

instance KnownCamera HarmanTitan4x5 where
  cameraPretty Proxy = "Harman Titan 4x5"

data Lens (camera :: Camera) where
  AutoTakumar55mm :: Lens 'PentaxSV
  Nikkor35mm :: Lens 'NikonF2
  Nikkor50mm :: Lens 'NikonF2
  Nikkor85mm :: Lens 'NikonF2
  CarlZeiss60mm :: Lens 'Hasselblad503cx
  CarlZeiss80mm :: Lens 'Hasselblad503cx
  CarlZeiss150mm :: Lens 'Hasselblad503cx
  MamiyaSekor65mm :: Lens 'MamiyaRB67ProS
  MamiyaSekor90mm :: Lens 'MamiyaRB67ProS
  MamiyaSekor180mm :: Lens 'MamiyaRB67ProS
  MamiyaSekor100mm :: Lens 'MamiyaUniversalPress
  Cone72mm :: Lens 'HarmanTitan4x5
  Cone110mm :: Lens 'HarmanTitan4x5
  Cone150mm :: Lens 'HarmanTitan4x5

instance FromJSON (Lens 'PentaxSV) where
  parseJSON = parseLens [(55, AutoTakumar55mm)]

instance FromJSON (Lens 'NikonF2) where
  parseJSON = parseLens [(35, Nikkor35mm), (50, Nikkor50mm), (85, Nikkor85mm)]

instance FromJSON (Lens 'Hasselblad503cx) where
  parseJSON = parseLens [(60, CarlZeiss60mm), (80, CarlZeiss80mm), (150, CarlZeiss150mm)]

instance FromJSON (Lens 'MamiyaRB67ProS) where
  parseJSON = parseLens [(65, MamiyaSekor65mm), (90, MamiyaSekor90mm), (180, MamiyaSekor180mm)]

instance FromJSON (Lens 'MamiyaUniversalPress) where
  parseJSON = parseLens [(100, MamiyaSekor100mm)]

instance FromJSON (Lens 'HarmanTitan4x5) where
  parseJSON = parseLens [(72, Cone72mm), (110, Cone110mm), (150, Cone150mm)]

parseLens :: [(Int, Lens camera)] -> Value -> Parser (Lens camera)
parseLens options = withScientific "lens id" $ \scientific ->
  case Scientific.floatingOrInteger scientific of
    Left (_ :: Double) ->
      fail $ "lens ids must be whole numbers, yet given " ++ show scientific
    Right n -> case lookup n options of
      Nothing -> fail $ "unknown lens id: " ++ show n
      Just x -> pure x

lensPretty :: Lens camera -> Text
lensPretty = \case
  AutoTakumar55mm -> "Auto-Takumar 55mm f/2.0"
  Nikkor35mm -> "Nikkor 35mm f/2.0 ais"
  Nikkor50mm -> "Nikkor 50mm f/1.8 ais"
  Nikkor85mm -> "Nikkor 85mm f/2.0 ais"
  CarlZeiss60mm -> "Carl Zeiss Distagon 60mm f/3.5 CF T*"
  CarlZeiss80mm -> "Carl Zeiss Planar 80mm f/2.8 CF T*"
  CarlZeiss150mm -> "Carl Zeiss Sonnar 150mm f/4.0 CF T*"
  MamiyaSekor65mm -> "Mamiya-Sekor C 65mm f/4.5"
  MamiyaSekor90mm -> "Mamiya-Sekor C 90mm f/3.8"
  MamiyaSekor180mm -> "Mamiya-Sekor C 180mm f/4.5"
  MamiyaSekor100mm -> "Mamiya-Sekor 100mm f/3.5"
  Cone72mm -> "72mm cone"
  Cone110mm -> "110mm cone"
  Cone150mm -> "150mm cone"

data Film
  = CineStill400D
  | FujicolorPro400H
  | KodakPortra160
  | KodakPortra400
  | KodakEktachromeE100

instance FromJSON Film where
  parseJSON = withText "film id" $ \txt ->
    case txt of
      "cinestill400d" -> pure CineStill400D
      "fuji400h" -> pure FujicolorPro400H
      "portra160" -> pure KodakPortra160
      "portra400" -> pure KodakPortra400
      "ektachrome" -> pure KodakEktachromeE100
      _ -> fail $ "unknown film id: " ++ T.unpack txt

filmPretty :: Film -> Text
filmPretty = \case
  CineStill400D -> "CineStill 400D"
  FujicolorPro400H -> "Fujicolor Pro 400H"
  KodakPortra160 -> "Kodak Portra 160"
  KodakPortra400 -> "Kodak Portra 400"
  KodakEktachromeE100 -> "Kodak Ektachrome E100"
