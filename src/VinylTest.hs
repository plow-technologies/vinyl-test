{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module VinylTest where



import           Control.Applicative
import           Control.Lens                hiding (Identity)
import           Data.Char
import           Data.Singletons.TH
import           Data.Text
import           Data.Time
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.Idiom.Identity
import           Data.Vinyl.Idiom.Validation
import           Data.Vinyl.TH
import           Data.Vinyl.TyFun
import qualified Data.Vinyl.Universe.Const   as U
import           Data.Vinyl.Witnesses
import           Test.DocTest
import           Data.Validation

data Fields = LocationId | SlaveParameterId | ParameterTagId | Description | UnitId | StatusActive | StatusWritable | LastUpdate | Rslt | ValidationCode | Permissions | Delete | CompanyIdRef | SiteIdRef | PId | OTCList deriving Show

genSingletons [''Fields]
makeUniverse' ''Fields "OPU"
semantics ''OPU [
                   'LocationId       :~> ''Int
                ,  'SlaveParameterId :~> ''Int
                ,  'ParameterTagId   :~> ''Int
                ,  'Description      :~> ''Text
                ,  'UnitId           :~> ''Int
                ,  'StatusActive     :~> ''Int
                ,  'StatusWritable   :~> ''Int
                ,  'LastUpdate       :~> ''UTCTime
                ,  'Rslt             :~> ''Text
                ,  'ValidationCode   :~> ''Text
                ,  'Permissions      :~> ''Int
                ,  'Delete           :~> ''Int
                ,  'CompanyIdRef     :~> ''Int
                ,  'SiteIdRef        :~> ''Int
                ]



type OnpingTagCombined = [ LocationId , SlaveParameterId , ParameterTagId
                        , Description, UnitId , StatusActive , StatusWritable
                        , LastUpdate , Rslt , ValidationCode , Permissions , Delete ]
-- otc :: Int -> Int -> Int -> Text -> Int -> Int -> Int -> UTCTime -> Text -> Text -> Int -> Int -> Int -> Int -> Int -> PlainRec OPU OnpingTagCombined
-- otc l s t descr uid sActive sWrite lUpdate rs valCode perm del cidRef sidRef pid
type LId = [LocationId, SlaveParameterId]

otcTemplate :: Int -> Int -> Int -> Text -> Int -> Int -> Int -> UTCTime -> Text -> Text -> Int -> Int -> PlainRec OPU OnpingTagCombined
otcTemplate l s t descr uid sActive sWrite lUpdate rs valCode perm del = SLocationId =: l
                                                                                       <+> SSlaveParameterId =: s
                                                                                       <+> SParameterTagId =: t
                                                                                       <+> SDescription =: descr
                                                                                       <+> SUnitId =: uid
                                                                                       <+> SStatusActive =: sActive
                                                                                       <+> SStatusWritable =: sWrite
                                                                                       <+> SLastUpdate =: lUpdate
                                                                                       <+> SRslt =: rs
                                                                                       <+> SValidationCode =: valCode
                                                                                       <+> SPermissions =: perm
                                                                                       <+> SDelete =: del


semantics ''OPU [ 'OTCList :~> [t| [PlainRec OPU OnpingTagCombined] |]]


-- type Location = [LocationId, [OTC]]

type Location = [LocationId, OTCList]



otcHighTankLevel1 t = otcTemplate 1 1 2 "High Tank Level" 1 1 0 t "0" "" 1 0 
otcHighTankLevel2 t = otcTemplate 1 2 2 "High Tank Warning" 1 1 0 t "0" "" 1 0 
otcHighTankLevel3 t = otcTemplate 1 3 2 "High Tank Maintain" 1 1 0 t "0" "" 1 0 
otcHighTankLevel4 t = otcTemplate 1 4 2 "Low Tank Level" 1 1 0 t "0" "" 1 0 
otcHighTankLevel5 t = otcTemplate 1 5 2 "Low Tank Warning" 1 1 0 t "0" "" 1 0 
otcHighTankLevel6 t = otcTemplate 1 6 2 "Low Tank Maintain" 1 1 0 t "0" "" 1 0 
otcHighTankLevel7 t = otcTemplate 1 7 2 "Mid Cup Level" 1 1 0 t "0" "" 1 0 
otcHighTankLevel8 t = otcTemplate 1 8 2 "Mid Cup Warning" 1 1 0 t "0" "" 1 0 

otcList t = [otcHighTankLevel1 t, otcHighTankLevel2 t, otcHighTankLevel3 t, otcHighTankLevel4 t, otcHighTankLevel5 t, otcHighTankLevel6 t, otcHighTankLevel7 t, otcHighTankLevel8 t]


tFilter :: (IElem SlaveParameterId rs,
                  Eq (App el 'SlaveParameterId)) =>
                 PlainRec el rs -> (el $ 'SlaveParameterId) -> Bool
tFilter p tes = rGet SSlaveParameterId p == tes


-- tempValidator loc = 
-- tempLocation t = LocationId =: 1
--               <+> 


-- data Fields = Name | Age | Sleeping | Master deriving Show
-- genSingletons [ ''Fields ]
-- makeUniverse' ''Fields "ElF"
-- semantics ''ElF [ 'Name     :~> ''String
--                 , 'Age      :~> ''Int
--                 , 'Sleeping :~> ''Bool
--                 ]

-- type Sleepless = [Name, Age]
-- type LifeForm = [Name, Age, Sleeping]
-- type DogForm = [Name, Age, Sleeping, Master]


-- jon :: PlainRec ElF LifeForm
-- jon = SName =: "jon"
--   <+> SAge =: 20
--   <+> SSleeping =: False

-- instance Implicit (PlainRec (U.Const String) [Name, Age, Sleeping]) where
--   implicitly = SName =: "name"
--             <+> SAge =: "age"
--             <+> SSleeping =: "sleeping"



-- instance Show (PlainRec ElF LifeForm) where
--   show = rshow

-- semantics ''ElF [ 'Master :~> [t| PlainRec ElF LifeForm |]]

-- tucker :: PlainRec ElF DogForm
-- tucker = withUniverse ElF $
--   SName =: "tucker"
--   <+> SAge =: 7
--   <+> SSleeping =: True
--   <+> SMaster =: jon

-- instance Implicit (PlainRec (U.Const String) [Name,Age,Sleeping, Master]) where
--   implicitly = SName =: "name"
--             <+> SAge =: "age"
--             <+> SSleeping =: "sleeping"
--             <+> SMaster =: "master"

-- instance Implicit (PlainRec (U.Const String) [Name,Age]) where
--   implicitly = SName =: "name"
--             <+> SAge =: "age"

-- wakeUp :: (IElem Sleeping fields) => PlainRec ElF fields -> PlainRec ElF fields
-- wakeUp = SSleeping `rPut` False

-- -- sleepless :: PlainRec ElF Sleepless
-- -- sleepless = SName =: "kevin"
-- --         <+> SAge =: 20

-- -- tucker' :: PlainRec ElF LifeForm
-- tucker' = cast jon

-- type Person = [Name, Age]

-- instance Show (PlainRec ElF Person) where
--   show = rshow


-- validatePerson :: (IElem Name fields, IElem Age fields) => PlainRec ElF fields -> Result [String] (PlainRec ElF Person)
-- validatePerson p = (\n a -> SName =: n <+> SAge =: a) <$> vName <*> vAge
--   where vName = validateName (rGet SName p)
--         vAge = validateAge (rGet SAge p)
--         validateName str | all isAlpha str = Success str
--         validateName _ = Failure ["name must be alphabetic"]
--         validateAge i | i >= 0 = Success i
--         validateAge _ = Failure ["Age must be possitive"]

-- gcsDevice :: PlainRec OPU OnpingTagCombined -> Result [String] (PlainRec OPU OnpingTagCombined)
-- gcsDevice otc = otcTemplate <$> vL <*> vS <*> vP  

-- analogous

-- [ LocationId , SlaveParameterId , ParameterTagId
--                         , Description, UnitId , StatusActive , StatusWritable
--                         , LastUpdate , Rslt , ValidationCode , Permissions , Delete ]
