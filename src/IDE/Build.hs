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
       (triggerEventIDE, readIDE, IDEAction, Workspace(..), ipdPackageId,
        ipdDepends, IDEPackage)
import qualified Data.Map as Map
       (insert, empty, lookup, toList, fromList)
import Data.Graph
       (edges, topSort, graphFromEdges, Vertex, Graph,
        transposeG)
import Distribution.Package (pkgVersion, pkgName, Dependency(..))
import Data.List (delete, nub, (\\), find)
import Distribution.Version (withinRange)
import Data.Maybe (mapMaybe)
import IDE.Package
       (packageClean', packageCopy', packageRegister', buildPackage, packageConfig',
        packageTest', packageDoc')
import IDE.Core.Types
       (IDEEvent(..), Prefs(..), IDE(..), WorkspaceAction)
import Distribution.Text (Text(..))
import Control.Event (EventSource(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (MonadTrans(..))

-- import Debug.Trace (trace)
trace a b = b

-- * Exported

data MakeSettings = MakeSettings {
    msMakeMode                       :: Bool,
    msSingleBuildWithoutLinking      :: Bool,
    msSaveAllBeforeBuild             :: Bool,
    msBackgroundBuild                :: Bool,
    msRunUnitTests                   :: Bool,
    msJumpToWarnings                 :: Bool,
    msDontInstallLast                :: Bool}

-- | Take make settings from preferences
defaultMakeSettings :: Prefs -> MakeSettings
defaultMakeSettings prefs = MakeSettings  {
    msMakeMode                       = makeMode prefs,
    msSingleBuildWithoutLinking      = singleBuildWithoutLinking prefs,
    msSaveAllBeforeBuild             = saveAllBeforeBuild prefs,
    msBackgroundBuild                = backgroundBuild prefs,
    msRunUnitTests                   = runUnitTests prefs,
    msJumpToWarnings                 = jumpToWarnings prefs,
    msDontInstallLast                = dontInstallLast prefs}

-- | a make operation
data MakeOp =
    MoConfigure
    | MoBuild
    | MoTest
    | MoCopy
    | MoRegister
    | MoClean
    | MoDocu
    | MoOther String
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
makePackages ::  MakeSettings -> [IDEPackage] -> MakeOp -> MakeOp -> MakeOp -> WorkspaceAction
makePackages ms targets firstOp restOp  finishOp = trace ("makePackages : " ++ show firstOp ++ " " ++ show restOp) $ do
    ws <- ask
    lift $ do
        prefs' <- readIDE prefs
        let plan = constrMakeChain ms ws targets firstOp restOp finishOp
        trace ("makeChain : " ++ show plan) $ doBuildChain ms plan

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

constrMakeChain :: MakeSettings -> Workspace ->  [IDEPackage] -> MakeOp ->
    MakeOp -> MakeOp -> Chain MakeOp IDEPackage
-- No more targets
constrMakeChain _ _ [] _ _ _ = EmptyChain

constrMakeChain ms@MakeSettings{msMakeMode = makeMode}
                    Workspace{wsPackages = packages, wsNobuildPack = noBuilds}
                    targets firstOp restOp finishOp =
    trace ("topsorted: " ++ showTopSorted topsorted)
    constrElem targets topsorted depGraph ms noBuilds
                    firstOp restOp finishOp False
  where
        depGraph | makeMode  = constrDepGraph packages
                 | otherwise = Map.empty
        topsorted            = reverse $ topSortGraph $ constrParentGraph packages

-- Constructs a make chain
chainFor :: IDEPackage ->  MakeSettings -> MakeOp -> Chain MakeOp IDEPackage
                -> Maybe (Chain MakeOp IDEPackage)
                -> Chain MakeOp IDEPackage
chainFor target settings (MoComposed (hdOp:[])) cont mbNegCont =
    chainFor target settings hdOp cont mbNegCont
chainFor target settings (MoComposed (hdOp:rest)) cont mbNegCont =
    chainFor target settings hdOp (chainFor target settings (MoComposed rest) cont mbNegCont)
        mbNegCont
chainFor target settings op cont mbNegCont = Chain {
        mcAction =  op,
        mcEle    = target,
        mcPos    =  cont,
        mcNeg    =  mbNegCont}

-- Recursive building of a make chain
-- The first list of packages are the targets
-- The second list of packages is the topsorted graph of all deps of all targets
constrElem  :: [IDEPackage] -> [IDEPackage] -> MakeGraph -> MakeSettings -> [IDEPackage]
    -> MakeOp -> MakeOp -> MakeOp -> Bool -> Chain MakeOp IDEPackage
constrElem currentTargets tops  depGraph ms noBuilds
    firstOp restOp finishOp doneAnything

-- finished traversing the topsorted deps or no targets
    | null currentTargets || null tops = EmptyChain
-- operations have to be applied to current
    | elem (head tops) currentTargets && not (elem (head tops) noBuilds) =
        let current = head tops
            dependents = case Map.lookup current depGraph of
                            Nothing -> trace ("Build>>constrMakeChain: unknown package"
                                                ++ show current) []
                            Just deps -> deps
            withoutInstall = msDontInstallLast ms && null (delete current dependents)
            filteredOps = case firstOp of
                            MoComposed l -> MoComposed (filter (\e -> e /= MoCopy && e /= MoRegister) l)
                            MoCopy       -> MoComposed []
                            MoRegister   -> MoComposed []
                            other        -> other
        in trace ("constrElem1 deps: " ++ show dependents ++ " withoutInstall: " ++ show withoutInstall)
            $
            chainFor current ms (if withoutInstall then filteredOps else firstOp)
                (constrElem (nub $ currentTargets ++ dependents)
                    (tail tops) depGraph ms noBuilds restOp restOp finishOp True)
                (Just $ if doneAnything
                            then chainFor current ms finishOp EmptyChain Nothing
                            else EmptyChain)
-- no operations have to be applied to current, just try the next
    | otherwise  = trace ("constrElem2 " ++ show restOp) $
        constrElem currentTargets (tail tops) depGraph ms noBuilds
            firstOp restOp finishOp doneAnything


-- | Performs the operations of a build chain
doBuildChain :: MakeSettings -> Chain MakeOp IDEPackage -> IDEAction
doBuildChain _ EmptyChain = return ()
doBuildChain ms chain@Chain{mcAction = MoConfigure} =
    packageConfig' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoBuild} =
    buildPackage (msBackgroundBuild ms) (msJumpToWarnings ms) (not (msMakeMode ms) && msSingleBuildWithoutLinking ms)
        (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoDocu} =
    packageDoc' (msBackgroundBuild ms) (msJumpToWarnings ms) (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoTest} =
    packageTest' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoCopy} =
    packageCopy' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoRegister} =
    packageRegister' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoClean} =
    packageClean' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoMetaInfo} =
    triggerEventIDE UpdateWorkspaceInfo >> return ()
doBuildChain ms chain  = doBuildChain ms (mcPos chain)

constrCont ms pos (Just neg) False = doBuildChain ms neg
constrCont ms pos _ _ = doBuildChain ms pos

-- | Construct a dependency graph for a package
-- pointing to the packages the subject package depends on
constrParentGraph :: [IDEPackage] -> MakeGraph
constrParentGraph targets = trace ("parentGraph : " ++ showGraph parGraph) parGraph
  where
    parGraph = Map.fromList
        $ map (\ p -> (p,nub $ mapMaybe (depToTarget targets)(ipdDepends p))) targets

-- | Construct a dependency graph for a package
-- pointing to the packages which depend on the subject package
constrDepGraph :: [IDEPackage] -> MakeGraph
constrDepGraph packages = trace ("depGraph : " ++ showGraph depGraph) depGraph
  where
    depGraph = reverseGraph (constrParentGraph packages)

showGraph :: MakeGraph -> String
showGraph mg =
    show
        $ map (\(k,v) -> (disp (ipdPackageId k), (map (disp . ipdPackageId) v)))
            $ Map.toList mg

showTopSorted :: [IDEPackage] -> String
showTopSorted = show . map (disp .ipdPackageId)


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
    myEdges              = map (\(a,b) -> (lookItUp a, lookItUp b)) $ edges graph
    lookItUp             =  (\(_,e,_)-> e) . lookup






