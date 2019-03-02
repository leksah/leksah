{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
module IDE.Web.Events where

import Control.Lens (makePrisms)

import Data.Dependent.Map (DMap)
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare.TH (DeriveGEQ(..), DeriveGCompare(..))
import Data.Map (Map)

import Distribution.Types.PackageId (PackageIdentifier(..))

import IDE.Core.Types (LogRef(..))
import IDE.Web.Command (Command(..))

data FileEvent
  = OpenFile Bool FilePath
  | DeleteFile FilePath
  deriving (Eq, Ord, Show)

makePrisms ''FileEvent

-- Workspace Tree Events
type FileEvents    = Map FilePath FileEvent

data PackageEvent
  = PackageCommand Command
  | PackageFileEvents FileEvents

makePrisms ''PackageEvent

type PackageEvents = Map PackageIdentifier PackageEvent

data ProjectEvent
  = ProjectCommand Command
  | ProjectPackageEvents PackageEvents
  | ProjectFileEvents FileEvents

makePrisms ''ProjectEvent

type ProjectEvents = Map (Int, FilePath) ProjectEvent

newtype KeymapEvents =
  KeymapCommand Command

makePrisms ''KeymapEvents

type EditorEvents = ()
newtype ErrorsEvents =
  ErrorsGoto LogRef deriving (Eq, Show)

makePrisms ''ErrorsEvents

type FindbarEvents = ()
type GrepEvents = ()
type LogEvents = ()
type MenubarEvents = ()
type StatusbarEvents = ()
newtype ToolbarEvents =
  ToolbarCommand Command

makePrisms ''ToolbarEvents

data TabKey
  = WorkspaceKey
  | ErrorsKey
  | LogKey
  | GrepKey
  | EditorKey FilePath
    deriving (Ord, Eq, Show)

data TabEvents e where
  EditorTab    :: TabEvents EditorEvents
  ErrorsTab    :: TabEvents ErrorsEvents
  LogTab       :: TabEvents LogEvents
  GrepTab      :: TabEvents GrepEvents
  WorkspaceTab :: TabEvents ProjectEvents

deriveGEq      ''TabEvents
deriveGCompare ''TabEvents

data IDEWidget e where
  FindbarWidget   :: IDEWidget FindbarEvents
  MenubarWidget   :: IDEWidget MenubarEvents
  StatusbarWidget :: IDEWidget StatusbarEvents
  ToolbarWidget   :: IDEWidget ToolbarEvents
  TabWidget       :: IDEWidget (Map TabKey (DMap TabEvents Identity))
  KeymapWidget    :: IDEWidget KeymapEvents

deriveGEq      ''IDEWidget
deriveGCompare ''IDEWidget

