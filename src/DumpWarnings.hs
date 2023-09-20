{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DumpWarnings
  ( plugin
  ) where

import Data.Text qualified as T

import Relude

import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as A
import System.Directory (doesFileExist, removeFile, makeAbsolute)

import GHC qualified
import GHC.Data.FastString qualified as GHC
import GHC.Driver.Env.Types qualified as GHC
import GHC.Driver.Flags as GHC (warnFlagNames)
import GHC.Driver.Plugins qualified as GHC
import GHC.Prelude (show)
import GHC.Types.Error qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Utils.Logger qualified as GHC
import GHC.Utils.Outputable qualified as GHC

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.parsedResultAction = const removeOldWarnings
    , GHC.driverPlugin = const (pure . addWarningCapture)
    , GHC.pluginRecompile = GHC.purePlugin
    }

addWarningCapture :: GHC.HscEnv -> GHC.HscEnv
addWarningCapture hscEnv =
  hscEnv
    { GHC.hsc_logger = GHC.pushLogHook warningsHook (GHC.hsc_logger hscEnv)
    }

removeOldWarnings :: GHC.ModSummary -> GHC.ParsedResult -> GHC.Hsc GHC.ParsedResult
removeOldWarnings modSummary parsedModule = do
  let swapExtension = (<> "." <> warningFileExt) . stripSuffix' ".hi"
  let file = toString $ swapExtension $ toText $ GHC.ml_hi_file $ GHC.ms_location modSummary
  whenM (liftIO $ doesFileExist file) $ liftIO $ removeFile file
  pure parsedModule

warningsHook :: GHC.LogAction -> GHC.LogAction
warningsHook passThroughDefaultLogAction logFlags messageClass srcSpan sdoc = do
  dumpWarning
  passThroughDefaultLogAction logFlags messageClass srcSpan sdoc
  where
    dumpWarning =
      case messageClass of
#if MIN_VERSION_ghc(9,6,0)
        GHC.MCDiagnostic GHC.SevWarning reason _
#else
        GHC.MCDiagnostic sev reason
#endif
          | GHC.RealSrcSpan realSrcSpan _ <- srcSpan
          , Just modFile <- GHC.srcSpanFileName_maybe srcSpan -> do
                let file = GHC.unpackFS modFile
                absFile <- makeAbsolute file
                let warningLine =
                      WarningLine
                        { severity = Severity sev
                        , file = toText file
                        , absFile = toText absFile
                        , location = Span realSrcSpan
                        , message = SDoc sdoc
                        , flag = case reason of
                            GHC.WarningWithFlag flag -> Just $ toText $ head $ GHC.warnFlagNames flag
                            _ -> Nothing
                        }
                whenJust dumpFileMb $ \dumpFile -> do
                  appendFileLBS dumpFile $ delimitLine $ A.encode warningLine
        _ -> pure ()
    delimitLine = (<> "\n")
    dumpFileMb = (<> "/" <> GHC.log_dump_prefix logFlags <> warningFileExt) <$> GHC.log_dump_dir logFlags

data WarningLine = WarningLine
  { severity :: Severity
  , file :: Text
  , absFile :: Text
  , location :: Span
  , message :: SDoc
  , flag :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

newtype Severity = Severity GHC.Severity
  deriving newtype (Show)
instance ToJSON Severity where
  toJSON (Severity sev) = A.String $ case sev of
    GHC.SevIgnore -> "Ignore"
    GHC.SevWarning -> "Warning"
    GHC.SevError -> "Error"

newtype Span = Span GHC.RealSrcSpan
  deriving newtype (Show)
instance ToJSON Span where
  toJSON (Span s) =
    A.object
      [ "startLine" .= GHC.srcSpanStartLine s
      , "startCol" .= GHC.srcSpanStartCol s
      , "endLine" .= GHC.srcSpanEndLine s
      , "endCol" .= GHC.srcSpanEndCol s
      ]

newtype SDoc = SDoc GHC.SDoc

instance Show SDoc where
  show (SDoc msg) = showWarning msg

instance ToJSON SDoc where
  toJSON (SDoc msg) = A.toJSON $ showWarning msg

showWarning :: GHC.SDoc -> String
showWarning sdoc =
  foldMap (GHC.renderWithContext sdocCtx)
    . GHC.unDecorated
#if MIN_VERSION_ghc(9,6,0)
    . GHC.diagnosticMessage GHC.NoDiagnosticOpts
#else
    . GHC.diagnosticMessage
#endif
    $ diag
  where
    sdocCtx =
      GHC.defaultSDocContext
        { GHC.sdocPrintUnicodeSyntax = True
        , GHC.sdocCanUseUnicode = True
        }
    diag =
      GHC.DiagnosticMessage
        { GHC.diagMessage = GHC.mkSimpleDecorated sdoc
        , GHC.diagReason = GHC.WarningWithoutFlag
        , GHC.diagHints = []
        }

warningFileExt :: (IsString a) => a
warningFileExt = "warn"

stripSuffix' :: Text -> Text -> Text
stripSuffix' suffix txt = fromMaybe txt $ T.stripSuffix suffix txt
