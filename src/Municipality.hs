module Municipality(muniLongName, fixMc)
where

import Prelude hiding (concat)
import Unicode
import Data.Text
import Types

-- todo this should be based on a database
muniLongName :: Municipality -> Text
muniLongName m = fixMc . toTitle . transform . mCouncilName $ m
   where
      transform = case mState m of
         Vic -> remove " (UNINCORPORATED)" . remove " (UNINC)"
         NSW -> replaceMunicipal . replaceRegional . removeCouncil . specialNSW
         Tas -> removeCouncil
         Qld -> replaceRegional
         SA → saMap
         WA → waMap
         NT → replace "Un-Incorporated" "Unincorporated"
         ACTg → id
         ACTd → id
         _ -> error (show m)

specialNSW ∷ Text → Text
specialNSW "COUNCIL OF THE CITY OF SYDNEY" = "SYDNEY CITY"
specialNSW "THE COUNCIL OF THE CITY OF BOTANY BAY" = "Botany Bay CITY"
specialNSW "THE COUNCIL OF THE SHIRE OF HORNSBY" = "Hornsby Shire"
specialNSW "CITY OF PARRAMATTA COUNCIL" = "PARRAMATTA CITY"
specialNSW "CITY OF CANADA BAY COUNCIL" = "CANADA BAY CITY"
specialNSW "THE COUNCIL OF THE MUNICIPALITY OF KIAMA" = "KIAMA municipality"
specialNSW "THE COUNCIL OF THE MUNICIPALITY OF HUNTERS HILL" = "Hunters Hill municipality"
specialNSW t = t

districtFrom ∷ Text → Text → Text
districtFrom prefix t = concat [remove prefix t, " District"]

cityFrom ∷ Text → Text → Text
cityFrom prefix t = concat [remove prefix t, " City"]

shireFrom ∷ Text → Text → Text
shireFrom prefix t = concat [remove prefix t, " Shire"]

townFrom ∷ Text → Text → Text
townFrom prefix t = concat [remove prefix t, " Town"]

waMap ∷ Text → Text
waMap t
  | "CITY OF" `isPrefixOf` t = cityFrom "CITY OF " t
  | "SHIRE OF" `isPrefixOf` t = shireFrom "SHIRE OF " t
  | "TOWN OF" `isPrefixOf` t = townFrom "TOWN OF " t
  | otherwise = error $ show t

saMap ∷ Text → Text
saMap t
   | "DC OF " `isPrefixOf` t = districtFrom "DC OF " t
   | "THE DC OF " `isPrefixOf` t = districtFrom "THE DC OF " t
   | "THE CITY OF " `isPrefixOf` t = cityFrom "THE CITY OF " t
   | "CITY OF " `isPrefixOf` t = cityFrom "CITY OF " t
   | t ≡ "NORTHERN AREAS COUNCIL" = "The Northern Areas"
   | t ≡ "MARALINGA_TJATJURA" = "Maralinga-Tjatjura"
   | t ≡ "THE RURAL CITY OF MURRAY BRIDGE" = "Murray Bridge Rural City"
   | t ≡ "MOUNT BARKER DISTRICT COUNCIL" = "Mt Barker District"
   | " DC" `isSuffixOf` t = replace " DC" " District" t 
   | t ≡ "WUDINNA DISTRICT COUNCIL" = "Wudinna District"
   | t ≡ "LIGHT REGIONAL COUNCIL" = "Light Region"
   | t ≡ "WAKEFIELD REGIONAL COUNCIL" = "Wakefield Region"
   | t ≡ "PORT PIRIE REGIONAL COUNCIL" = "Port Pirie Region"
   | t ≡ "THE REGIONAL COUNCIL OF GOYDER" = "Goyder Region"
   | t ≡ "TOWN OF GAWLER" = "Gawler Town"
   | t ≡ "THE CORPORATION OF THE TOWN OF WALKERVILLE" = "Walkerville Town"
   | t ≡ "THE CORPORATION OF THE CITY OF WHYALLA" = "Whyalla City"
   | t ≡ "UIA RIVERLAND" = "The Riverland Unincorporated Area"
   | t ≡ "UIA WHYALLA" = "The Unincorporated Area in Whyalla"
   | t ≡ "MUNICIPAL COUNCIL OF ROXBY DOWNS" = "The Roxby Downs Municipality"
   | justCouncil t = removeCouncil t
   | noChange t = t
   | otherwise = error $ show t
   where
      justCouncil "YORKE PENINSULA COUNCIL" = True
      justCouncil "WATTLE RANGE COUNCIL" = True
      justCouncil "ADELAIDE HILLS COUNCIL" = True
      justCouncil "ADELAIDE CITY COUNCIL" = True
      justCouncil "NARACOORTE LUCINDALE COUNCIL" = True
      justCouncil "ALEXANDRINA COUNCIL" = True
      justCouncil "PORT AUGUSTA CITY COUNCIL" = True
      justCouncil "THE BAROSSA COUNCIL" = True
      justCouncil "THE BERRI BARMERA COUNCIL" = True
      justCouncil "CAMPBELLTOWN CITY COUNCIL" = True
      justCouncil "CLARE AND GILBERT VALLEYS COUNCIL" = True
      justCouncil "THE FLINDERS RANGES COUNCIL" = True
      justCouncil "KANGAROO ISLAND COUNCIL" = True
      justCouncil "MID MURRAY COUNCIL" = True
      justCouncil "RENMARK PARINGA COUNCIL" = True
      justCouncil _ = False
      noChange "ANANGU PITJANTJATJARA YANKUNYTJATJARA" = True
      noChange "PASTORAL UNINCORPORATED AREA" = True
      noChange _ = False

removeCouncil ∷ Text → Text
removeCouncil = remove " COUNCIL" 

remove ∷ Text → Text → Text
remove it = replace it ""

replaceRegional :: Text -> Text
replaceRegional = replace " REGIONAL" " REGION"

replaceMunicipal :: Text -> Text
replaceMunicipal = replace " MUNICIPAL" " MUNICIPALITY"

fixMc ∷ Text → Text
fixMc name = fixName stripped
   where
      stripped = stripPrefix "MC" name
      fixName (Just x) = "Mc" `mappend` x
      fixName Nothing = name
