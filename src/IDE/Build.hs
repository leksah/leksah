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
    constrDepGraph, -- :: [IDEPackage] -> MakeGraph
    constrMakeChain, -- :: MakeSettings -> MakeGraph -> [IDEPackage] -> BuildChain MakeOp
    doBuildChain, -- :: BuildChain MakeOp -> IDE Bool
    makePackages,
    MakeSettings(..),
    MakeOp(..),
    defaultMakeSettings
) where

import Data.Map (Map)
import IDE.Core.State
       (readIDE, IDEAction, Workspace(..), ipdPackageId, ipdDepends,
        IDEPackage)
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
       (packageClean', packageInstall', buildPackage, packageConfig')
import IDE.Core.Types
       (Prefs(..), IDE(..), WorkspaceAction)
import Control.Monad.Reader
import Distribution.Text (Text(..))

-- import Debug.Trace (trace)
trace a b = b


-- ** Types

type MyGraph a = Map a [a]

type MakeGraph = MyGraph IDEPackage

-- | a make operation
data MakeOp =
    MoConfigure
    | MoBuild
    | MoInstall
    | MoClean
    | MoDocu
    | MoOther String
    | MoComposed [MakeOp]
    deriving (Eq,Ord,Show)

data Chain alpha beta  =
    Chain {
        mcAction :: alpha,
        mcEle    :: beta,
        mcPos    :: Chain alpha beta,
        mcNeg    :: Maybe (Chain alpha beta)}
    | EmptyChain
    deriving Show

data MakeSettings = MakeSettings {
    msMakeMode                       :: Bool,
    msSingleBuildWithoutLinking      :: Bool,
    msSaveAllBeforeBuild             :: Bool,
    msBackgroundBuild                :: Bool,
    msDontInstallLast                :: Bool}

defaultMakeSettings :: Prefs -> MakeSettings
defaultMakeSettings prefs = MakeSettings  {
    msMakeMode                       = makeMode prefs,
    msSingleBuildWithoutLinking      = singleBuildWithoutLinking prefs,
    msSaveAllBeforeBuild             = saveAllBeforeBuild prefs,
    msBackgroundBuild                = backgroundBuild prefs,
    msDontInstallLast                = dontInstallLast prefs}

-- ** Functions

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

-- | Construct a make chain for a package,
-- which is a plan of the build to perform.
-- Consumes settings, the workspace and a list of targets.
constrMakeChain :: MakeSettings -> Workspace ->  [IDEPackage] -> MakeOp -> MakeOp -> Chain MakeOp IDEPackage
constrMakeChain _ _ [] _ _ = EmptyChain
constrMakeChain ms@MakeSettings{msMakeMode = makeMode}
                    Workspace{wsPackages = packages, wsNobuildPack = noBuilds}
                    targets@(headTarget:restTargets) op1 op2
    | not makeMode  =  chainFor headTarget ms op1 EmptyChain Nothing
    | otherwise =  trace ("topsorted: " ++ showTopSorted topsorted)
                    constrElem targets topsorted depGraph ms noBuilds op1 op2
      where
        depGraph        =  constrDepGraph packages
        topsorted       =  reverse $ topSortGraph $ constrParentGraph packages

constrElem  :: [IDEPackage] -> [IDEPackage] -> MakeGraph -> MakeSettings -> [IDEPackage]
    -> MakeOp -> MakeOp -> Chain MakeOp IDEPackage
constrElem _ [] _ _  _ _ _ = trace ("constrElem: 1") EmptyChain
constrElem [] _ _ _  _ _ _ = trace ("constrElem: 2") EmptyChain
constrElem currentTargets (current:rest)  depGraph ms noBuilds op1 op2
    | elem current currentTargets && not (elem current noBuilds) =
        let dependends = case Map.lookup current depGraph of
                        Nothing -> trace ("Build>>constrMakeChain: unknown package"
                                            ++ show current) []
                        Just deps -> deps
            withoutInstall = msDontInstallLast ms && null (delete current dependends)
            filteredOps = case op1 of
                            MoComposed l -> MoComposed (filter (\e -> e /= MoInstall) l)
                            MoInstall    -> MoComposed []
                            other        -> other
        in trace ("constrElem1 deps: " ++ show dependends ++ " withoutInstall: " ++ show withoutInstall)
            $
            chainFor current ms (if withoutInstall then filteredOps else op1)
                (constrElem (nub $ currentTargets ++ dependends)  rest depGraph ms noBuilds op2 op2)
                (Just EmptyChain)
    | otherwise  = trace ("constrElem2 " ++ show op2) $ constrElem currentTargets rest depGraph ms noBuilds op1 op2

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

doBuildChain :: MakeSettings -> Chain MakeOp IDEPackage -> IDEAction
doBuildChain _ EmptyChain = return ()
doBuildChain ms chain@Chain{mcAction = MoConfigure} = do
    packageConfig' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoBuild} = do
    buildPackage (msBackgroundBuild ms) (not (msMakeMode ms) && msSingleBuildWithoutLinking ms)
        (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoInstall} = do
    packageInstall' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain@Chain{mcAction = MoClean} = do
    packageClean' (mcEle chain) (constrCont ms (mcPos chain) (mcNeg chain))
doBuildChain ms chain  = doBuildChain ms (mcPos chain)

constrCont ms pos (Just neg) False = doBuildChain ms neg
constrCont ms pos _ _ = doBuildChain ms pos

makePackages ::  MakeSettings -> [IDEPackage] -> MakeOp -> MakeOp -> WorkspaceAction
makePackages ms targets op1 op2  = trace ("makePackages : " ++ show op1 ++ " " ++ show op2) $ do
    ws <- ask
    lift $ do
        prefs' <- readIDE prefs
        let plan = constrMakeChain ms ws targets op1 op2
        trace ("makeChain : " ++ show plan) $ doBuildChain ms plan


-- | Calculates for every dependency a target (or not)

-- TODO
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


--calculateReverseDependencies ::
--  Workspace -> Map IDEPackage [IDEPackage]
--calculateReverseDependencies Workspace{wsPackages = wsPackages} = constrDepGraph wsPackages





