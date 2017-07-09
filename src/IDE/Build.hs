{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Build
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Simple build system for packages
--
-------------------------------------------------------------------------------


module IDE.Build (
    MakeSettings(..),
    MakeOp(..),
    moNoOp,
    makePackages,
    defaultMakeSettings
) where

import Data.Map (Map)
import IDE.Core.State
       (postAsyncIDE, postSyncIDE, triggerEventIDE, readIDE, IDEAction,
        Workspace(..), Project(..), ipdPackageId, ipdDepends, IDEPackage)
import qualified Data.Map as Map
       (insert, empty, lookup, toList, fromList)
import Data.Graph
       (edges, topSort, graphFromEdges, Vertex, Graph,
        transposeG)
import Distribution.Package (pkgVersion, pkgName, Dependency(..))
import Data.List (delete, nub, (\\), find)
import Distribution.Version (withinRange)
import Data.Maybe (fromMaybe, mapMaybe)
import IDE.Package
       (packageClean', buildPackage, packageConfig',
        packageTest', packageDoc', packageBench', packageInstall')
import IDE.Core.Types
       (ipdPackageName, IDEEvent(..), Prefs(..), IDE(..), WorkspaceAction)
import Control.Event (EventSource(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void)
import Control.Arrow ((***))
import Data.Text (Text)
import Distribution.Text (disp)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, unpack)
import System.Log.Logger (debugM)

-- import Debug.Trace (trace)
trace a b = b

-- * Exported

data MakeSettings = MakeSettings {
    msMakeMode                       :: Bool,
    msSingleBuildWithoutLinking      :: Bool,
    msSaveAllBeforeBuild             :: Bool,
    msBackgroundBuild                :: Bool,
    msMakeDocs                       :: Bool,
    msRunUnitTests                   :: Bool,
    msRunBenchmarks                  :: Bool,
    msJumpToWarnings                 :: Bool,
    msDontInstallLast                :: Bool}

-- | Take make settings from preferences
defaultMakeSettings :: Prefs -> MakeSettings
defaultMakeSettings prefs = MakeSettings  {
    msMakeMode                       = makeMode prefs,
    msSingleBuildWithoutLinking      = singleBuildWithoutLinking prefs,
    msSaveAllBeforeBuild             = saveAllBeforeBuild prefs,
    msBackgroundBuild                = backgroundBuild prefs,
    msMakeDocs                       = makeDocs prefs,
    msRunUnitTests                   = runUnitTests prefs,
    msRunBenchmarks                  = runBenchmarks prefs,
    msJumpToWarnings                 = jumpToWarnings prefs,
    msDontInstallLast                = dontInstallLast prefs}

-- | a make operation
data MakeOp =
    MoConfigure
    | MoBuild
    | MoTest
    | MoBench
    | MoInstall
    | MoClean
    | MoDocu
    | MoOther Text
    | MoMetaInfo -- rebuild meta info for workspace
    | MoComposed [MakeOp]
    deriving (Eq,Ord,Show)

moNoOp = MoComposed[]

-- | The interface to the build system
-- Consumes settings, a list of targets and a the operation to perform.
-- The firstOp will be applied to the first target
-- The restOp will be applied to all other targets
-- The finishOp will be applied to the last target after any op succeeded,
-- but it is applied after restOp has been tried on the last target
makePackages ::  MakeSettings -> [(Project, [IDEPackage])] -> MakeOp -> MakeOp -> MakeOp -> IDEAction
makePackages ms targets firstOp restOp  finishOp = do
    liftIO $ debugM "leksah" $ "makePackages : "
            <> "targets = " <> show (map (pjFile *** map ipdPackageName) targets)
            <> ", fistOp = " <> show firstOp
            <> ", restOp = " <> show restOp
    prefs' <- readIDE prefs
    let plan = constrMakeChain ms targets firstOp restOp finishOp
    liftIO $ debugM "leksah" $ "makePackages : makeChain : " <> show plan
    doBuildChain ms plan

-- ** Types

type MyGraph a = Map a [a]

type MakeGraph = MyGraph IDEPackage

data Chain alpha beta  =
    Chain {
        mcAction :: alpha,
        mcEle    :: beta,
        mcPos    :: Chain alpha beta,
        mcNeg    :: Maybe (Chain alpha beta)}
    | EmptyChain
    deriving Show

-- ** Functions
-- | Construct a make chain for a package,
-- which is a plan of the build to perform.
-- Consumes settings, the workspace and a list of targets.
-- The first op is applied to the first target.

constrMakeChain :: MakeSettings -> [(Project, [IDEPackage])] -> MakeOp ->
    MakeOp -> MakeOp -> Chain MakeOp (Project, IDEPackage)
-- No more targets
constrMakeChain _ [] _ _ _ = EmptyChain

constrMakeChain ms@MakeSettings{msMakeMode = makeMode}
                    targets firstOp restOp finishOp =
--    trace (T.unpack $ "topsorted: " <> showTopSorted topsorted)
    constrElem (map addTopSorted targets) ms
                    firstOp restOp finishOp False
  where
        depGraph packages | makeMode  = constrDepGraph packages
                          | otherwise = Map.empty
        addTopSorted (project, targets) = (project, targets, reverse $ topSortGraph $ constrParentGraph $ pjPackages project, depGraph $ pjPackages project)

-- Constructs a make chain
chainFor :: Project -> IDEPackage ->  MakeSettings -> MakeOp -> Chain MakeOp (Project, IDEPackage)
                -> Maybe (Chain MakeOp (Project, IDEPackage))
                -> Chain MakeOp (Project, IDEPackage)
chainFor project target settings (MoComposed [hdOp]) cont mbNegCont =
    chainFor project target settings hdOp cont mbNegCont
chainFor project target settings (MoComposed (hdOp:rest)) cont mbNegCont =
    chainFor project target settings hdOp (chainFor project target settings (MoComposed rest) cont mbNegCont)
        mbNegCont
chainFor project target settings op cont mbNegCont = Chain {
        mcAction =  op,
        mcEle    = (project, target),
        mcPos    =  cont,
        mcNeg    =  mbNegCont}

-- Recursive building of a make chain
-- The first list of packages are the targets
-- The second list of packages is the topsorted graph of all deps of all targets
constrElem  :: [(Project, [IDEPackage], [IDEPackage], MakeGraph)] -> MakeSettings
    -> MakeOp -> MakeOp -> MakeOp -> Bool -> Chain MakeOp (Project, IDEPackage)
constrElem [] _ _ _ _ _ = EmptyChain
constrElem ((project, currentTargets, tops, depGraph):rest) ms
    firstOp restOp finishOp doneAnything

-- finished traversing the topsorted deps or no targets
    | null currentTargets || null tops = constrElem rest ms
                                                firstOp restOp finishOp doneAnything
-- operations have to be applied to current
    | head tops `elem` currentTargets =
        let current = head tops
            dependents = fromMaybe
                            (trace ("Build>>constrMakeChain: unknown package" ++ show current)
                               [])
                            (Map.lookup current depGraph)
            withoutInstall = msDontInstallLast ms && null (delete current dependents)
            filteredOps = case firstOp of
                            MoComposed l -> MoComposed (filter (/= MoInstall) l)
                            MoInstall    -> MoComposed []
                            other        -> other
        in trace ("constrElem1 deps: " ++ show dependents ++ " withoutInstall: " ++ show withoutInstall)
            $
            chainFor project current ms (if withoutInstall then filteredOps else firstOp)
                (constrElem ((project, nub $ currentTargets ++ dependents, tail tops, depGraph):rest)
                        ms restOp restOp finishOp True)
                (Just $ if doneAnything
                            then chainFor project current ms finishOp EmptyChain Nothing
                            else EmptyChain)
-- no operations have to be applied to current, just try the next
    | otherwise  = trace ("constrElem2 " ++ show restOp) $
        constrElem ((project, currentTargets, tail tops, depGraph):rest) ms
            firstOp restOp finishOp doneAnything


-- | Performs the operations of a build chain
doBuildChain :: MakeSettings -> Chain MakeOp (Project, IDEPackage) -> IDEAction
doBuildChain _ EmptyChain = return ()
doBuildChain ms chain@Chain{mcAction = MoConfigure} =
    postAsyncIDE $ packageConfig' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoBuild} =
    postAsyncIDE $ buildPackage (msBackgroundBuild ms) (msJumpToWarnings ms) (not (msMakeMode ms) && msSingleBuildWithoutLinking ms)
        (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoDocu} =
    postAsyncIDE $ packageDoc' (msBackgroundBuild ms) (msJumpToWarnings ms) (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoTest} =
    postAsyncIDE $ packageTest' (msBackgroundBuild ms) (msJumpToWarnings ms) (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoBench} =
    postAsyncIDE $ packageBench' (msBackgroundBuild ms) (msJumpToWarnings ms) (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoInstall} =
    postAsyncIDE $ packageInstall' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoClean} =
    postAsyncIDE $ packageClean' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoMetaInfo} =
    postAsyncIDE . void $ triggerEventIDE UpdateWorkspaceInfo
doBuildChain ms chain  = doBuildChain ms (mcPos chain)

constrCont ms pos (Just neg) False = doBuildChain ms neg
constrCont ms pos _ _ = doBuildChain ms pos

-- | Construct a dependency graph for a package
-- pointing to the packages the subject package depends on
constrParentGraph :: [IDEPackage] -> MakeGraph
constrParentGraph targets = trace (T.unpack $ "parentGraph : " <> showGraph parGraph) parGraph
  where
    parGraph = Map.fromList
        $ map (\ p -> (p,nub $ mapMaybe (depToTarget targets)(ipdDepends p))) targets

-- | Construct a dependency graph for a package
-- pointing to the packages which depend on the subject package
constrDepGraph :: [IDEPackage] -> MakeGraph
constrDepGraph packages = trace (T.unpack $ "depGraph : " <> showGraph depGraph) depGraph
  where
    depGraph = reverseGraph (constrParentGraph packages)

showGraph :: MakeGraph -> Text
showGraph mg =
    T.pack $ show
        $ map (\(k,v) -> (disp (ipdPackageId k), map (disp . ipdPackageId) v))
            $ Map.toList mg

showTopSorted :: [IDEPackage] -> Text
showTopSorted = T.pack . show . map (disp .ipdPackageId)


-- | Calculates for every dependency a target (or not)

depToTarget :: [IDEPackage] -> Dependency -> Maybe IDEPackage
depToTarget list dep = find (doesMatch dep) list
        where
        doesMatch (Dependency name versionRange) thePack =
            name == pkgName (ipdPackageId thePack)
            &&  withinRange (pkgVersion (ipdPackageId thePack)) versionRange

reverseGraph :: Ord alpha => MyGraph alpha -> MyGraph alpha
reverseGraph = withIndexGraph transposeG

topSortGraph :: Ord alpha => MyGraph alpha -> [alpha]
topSortGraph myGraph =  map ((\ (_,x,_)-> x) . lookup) $ topSort graph
  where
    (graph,lookup,_) = fromMyGraph myGraph

withIndexGraph :: Ord alpha => (Graph -> Graph) -> MyGraph alpha -> MyGraph alpha
withIndexGraph idxOp myGraph = toMyGraph (idxOp graph) lookup
  where
    (graph,lookup,_) = fromMyGraph myGraph

fromMyGraph :: Ord alpha => MyGraph alpha -> (Graph, Vertex -> ((), alpha , [alpha]), alpha -> Maybe Vertex)
fromMyGraph myGraph =
    graphFromEdges
        $ map (\(e,l)-> ((),e,l))
            $ graphList ++ map (\e-> (e,[])) missingEdges
  where
    mentionedEdges = nub $ concatMap snd graphList
    graphList = Map.toList myGraph
    missingEdges = mentionedEdges \\ map fst graphList

toMyGraph ::  Ord alpha =>  Graph -> (Vertex -> ((), alpha, [alpha])) -> MyGraph alpha
toMyGraph graph lookup = foldr constr Map.empty myEdges
  where
    constr (from,to) map = case Map.lookup from map of
                                Nothing -> Map.insert from [to] map
                                Just l -> Map.insert from (to : l) map
    myEdges              = map (lookItUp *** lookItUp) $ edges graph
    lookItUp             =  (\(_,e,_)-> e) . lookup






