{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module IDE.Pane.LeksahState.Inspectable where

import Data.Text (pack, Text)
import qualified Data.Map as Map
import Data.Kind
import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool
import Data.Proxy
import Data.IORef
import qualified Data.Sequence
import Data.Typeable
import Data.Foldable
import Data.Text (unpack)
import qualified Data.Text

data InspectorNode = InspectorNode Text [InspectorProperty]
    deriving (Show)

inspectorLeaf = flip InspectorNode []

data InspectorProperty = InspectorProperty
    { propName  :: Maybe Text
    , propValue :: InspectorNode
    } deriving (Show)

unnamedProperty :: InspectorNode -> InspectorProperty
unnamedProperty = InspectorProperty Nothing

class Inspectable a where
    toInspectorNode :: a -> InspectorNode

    default toInspectorNode :: (Generic a, GInspectable (Rep a)) => a -> InspectorNode
    toInspectorNode x = gInspectable (from x)

instance Inspectable Int where
    toInspectorNode x = inspectorLeaf (pack $ show x)

instance Inspectable String where
    toInspectorNode x = inspectorLeaf (pack $ show x)

instance {-# OVERLAPPABLE #-} (Inspectable a) => Inspectable [a] where
    toInspectorNode xs = InspectorNode
        (pack $ "(length = " ++ show (length xs) ++ ")")
        (map (unnamedProperty . toInspectorNode) (toList xs))

instance Inspectable a => Inspectable (Maybe a)
instance (All Inspectable '[a, b]) => Inspectable (a, b)
instance (All Inspectable '[a, b, c]) => Inspectable (a, b, c)
instance (All Inspectable '[a, b, c, d]) => Inspectable (a, b, c, d)
instance (All Inspectable '[a, b], All Typeable '[a, b]) => Inspectable (Map.Map a b) where
    toInspectorNode m =
        let pairs = zip [0..] (Map.toList m)
            props = map
                (\(n, (k,v)) -> unnamedProperty $ InspectorNode
                        ("#" <> pack (show n))
                        (map unnamedProperty [toInspectorNode k, toInspectorNode v])
                )
                pairs
        in InspectorNode
            (pack . show . typeRep $ Proxy @(Map.Map a b))
            props

instance {-# OVERLAPPABLE #-} (Typeable a) => Inspectable a where
    toInspectorNode _ = inspectorLeaf . (\x -> "_ :: " <> x) . pack . show . typeRep $ Proxy @a

instance Inspectable Bool
instance Inspectable Text where
    toInspectorNode t = inspectorLeaf . pack . show $ t
instance Inspectable a => Inspectable (Data.Sequence.Seq a) where
    toInspectorNode = toInspectorNode . toList

-- * Generic default instance for Inspectable
class GInspectable rep where
    gInspectable :: rep a -> InspectorNode

instance (GInspectable rep1, GInspectable rep2) => GInspectable (rep1 :+: rep2) where
    gInspectable (L1 rep) = gInspectable rep
    gInspectable (R1 rep) = gInspectable rep

-- | Applied constructor
instance (GToProperties rep, KnownSymbol name)
    => GInspectable (M1 C ('MetaCons name x y) rep) where
    gInspectable (M1 rep) = InspectorNode
        (pack $ symbolVal (Proxy :: Proxy name))
        (gToFields rep)

instance {-# OVERLAPPABLE #-} GInspectable rep => GInspectable (M1 D meta rep) where
    gInspectable (M1 rep) = gInspectable rep


-- | Flatten to a list to avoid building a cons-like structure
class GToProperties rep where
    gToFields :: rep a -> [InspectorProperty]

instance GToProperties U1 where
    gToFields U1 = []

instance {-# OVERLAPPING #-} (GToProperties rep1, GToProperties rep2)
    => GToProperties (rep1 :*: rep2) where
    gToFields (l :*: r) = gToFields l ++ gToFields r

-- Constructor field with selector name
instance {-# OVERLAPPING #-} (Inspectable t, KnownSymbol propName)
    => GToProperties (M1 S ('MetaSel ('Just propName) x y z) (K1 i t)) where
    gToFields (M1 (K1 x)) =
        let prop = InspectorProperty
                { propName  = Just . pack . symbolVal $ (Proxy :: Proxy propName)
                , propValue = toInspectorNode x
                }
        in [prop]

-- Constructor field without selector name
instance {-# OVERLAPPING #-} (Inspectable t)
    => GToProperties (M1 S ('MetaSel 'Nothing x y z) (K1 i t)) where
    gToFields (M1 (K1 x)) =
        let prop = InspectorProperty
                { propName  = Nothing
                , propValue = toInspectorNode x
                }
        in [prop]


type family All (c :: Type -> Constraint) (xs :: [Type]) :: Constraint where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)


-- | Pretty print inspector node
pretty :: InspectorNode -> String
pretty = unlines . map unpack . prettyNode
  where
    spacesPerIndent = 2

    prettyNode :: InspectorNode -> [Text]
    prettyNode node = case node of
      (InspectorNode name props) ->
        [name] ++ map indent (concatMap prettyProp props)

    prettyProp :: InspectorProperty -> [Text]
    prettyProp (InspectorProperty Nothing node) =
      prettyNode node
    prettyProp (InspectorProperty (Just name) node) =
        case prettyNode node of
          [x] -> [name <> " = " <> x]
          xs  -> [name <> " = " ] ++ map indent xs

    indent x = Data.Text.replicate spacesPerIndent " " <> x