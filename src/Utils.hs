module Utils
where

import Geometry.Shapefile.Types
import Prelude.Unicode
import Data.Complex

bigBoundingBox ∷ Maybe RecBBox → Maybe RecBBox → Maybe RecBBox
bigBoundingBox Nothing a = a
bigBoundingBox a Nothing = a
bigBoundingBox (Just b1) (Just b2) = Just $ bigBoundingBox' b1 b2

bigBoundingBox' ∷ RecBBox → RecBBox → RecBBox
bigBoundingBox' (RecBBox (x0a :+ y0a) (x1a :+ y1a)) (RecBBox (x0b :+ y0b) (x1b :+ y1b))
   = RecBBox ((min x0a x0b) :+ (min y0a y0b)) ((max x1a x1b) :+ (max y1a y1b))

data FilePaths = FilePaths {
   states            ∷ FilePath,
   nswLocalities     ∷ FilePath,
   nswMunicipalities ∷ FilePath,
   vicLocalities     ∷ FilePath,
   vicMunicipalities ∷ FilePath,
   qldLocalities     ∷ FilePath,
   qldMunicipalities ∷ FilePath,
   waLocalities      ∷ FilePath,
   waMunicipalities  ∷ FilePath,
   saLocalities      ∷ FilePath,
   saMunicipalities  ∷ FilePath,
   tasLocalities     ∷ FilePath,
   tasMunicipalities ∷ FilePath,
   actMunicipalities ∷ FilePath,
   actLocalities     ∷ FilePath,
   ntLocalities      ∷ FilePath,
   ntMunicipalities  ∷ FilePath,
   otLocalities      ∷ FilePath,
   otMunicipalities  ∷ FilePath,
   urbanAreas        ∷ FilePath,
   lakes ∷ FilePath,
   rivers ∷ FilePath
}

withFiles ∷ FilePath → FilePaths
withFiles sourceFolder = FilePaths {
    states            = sourceFolder ⧺ "/cth/GEODATA COAST 100k/australia/cstauscd_r.shp",
    nswMunicipalities = sourceFolder ⧺ "/cth/NSWLGAPOLYGON/NSW_LGA_POLYGON_shp.shp",
    vicMunicipalities = sourceFolder ⧺ "/cth/VICLGAPOLYGON/VIC_LGA_POLYGON_shp.shp",
    qldMunicipalities = sourceFolder ⧺ "/cth/QLDLGAPOLYGON/QLD_LGA_POLYGON_shp.shp",
    waMunicipalities  = sourceFolder ⧺ "/cth/WALGAPOLYGON/WA_LGA_POLYGON_shp.shp",
    saMunicipalities  = sourceFolder ⧺ "/cth/SALGAPOLYGON/SA_LGA_POLYGON_shp.shp",
    tasMunicipalities = sourceFolder ⧺ "/cth/TASLGAPOLYGON/TAS_LGA_POLYGON_shp.shp",
    ntMunicipalities  = sourceFolder ⧺ "/cth/NTLGAPOLYGON/NT_LGA_POLYGON_shp.shp",
    otMunicipalities  = sourceFolder ⧺ "/cth/OTLGAPOLYGON/OT_LGA_POLYGON_shp.shp",
    actMunicipalities = sourceFolder ⧺ "/cth/GEODATA COAST 100k/tristan/act munis.shp",
    nswLocalities     = sourceFolder ⧺ "/cth/NSWLOCALITYPOLYGON/NSW_LOCALITY_POLYGON_shp.shp",
    vicLocalities     = sourceFolder ⧺ "/cth/VICLOCALITYPOLYGON/VIC_LOCALITY_POLYGON_shp.shp",
    qldLocalities     = sourceFolder ⧺ "/cth/QLDLOCALITYPOLYGON/QLD_LOCALITY_POLYGON_shp.shp",
    waLocalities      = sourceFolder ⧺ "/cth/WALOCALITYPOLYGON/WA_LOCALITY_POLYGON_shp.shp",
    saLocalities      = sourceFolder ⧺ "/cth/SALOCALITYPOLYGON/SA_LOCALITY_POLYGON_shp.shp",
    tasLocalities     = sourceFolder ⧺ "/cth/TASLOCALITYPOLYGON/TAS_LOCALITY_POLYGON_shp.shp",
    actLocalities     = sourceFolder ⧺ "/cth/ACTLOCALITYPOLYGONshp/ACT_LOCALITY_POLYGON_shp.shp",
    ntLocalities      = sourceFolder ⧺ "/cth/NTLOCALITYPOLYGON/NT_LOCALITY_POLYGON_shp.shp",
    otLocalities      = sourceFolder ⧺ "/cth/OTLOCALITYPOLYGON/OT_LOCALITY_POLYGON_shp.shp",
    urbanAreas        = sourceFolder ⧺ "/abs/1270055004_sos_2011_aust_shape/SOS_2011_AUST.shp",
    lakes             = sourceFolder ⧺ "/misc/aus-oceania-natural-shape/natural.shp",
    rivers            = sourceFolder ⧺ "/misc/aus-oceania-waterways-shape/waterways.shp"
   }

