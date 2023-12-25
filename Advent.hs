module Advent where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.State.Strict as SS
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import           Data.Tuple

-- Day 25
data D25Wire =
  D25Wire String [String]
  deriving (Read, Show)

d25Graph :: [D25Wire] -> M.Map String (S.Set String)
d25Graph =
  foldl
    (\graph (D25Wire item wires) ->
       foldl
         (\gr item' ->
            M.alter (Just . S.insert item . fromMaybe S.empty) item' $ M.alter (Just . S.insert item' . fromMaybe S.empty) item gr)
         graph
         wires)
    M.empty

d25TestInput :: [D25Wire]
d25TestInput =
  [ D25Wire "jqt" ["rhn", "xhk", "nvd"]
  , D25Wire "rsh" ["frs", "pzl", "lsr"]
  , D25Wire "xhk" ["hfx"]
  , D25Wire "cmg" ["qnr", "nvd", "lhk", "bvb"]
  , D25Wire "rhn" ["xhk", "bvb", "hfx"]
  , D25Wire "bvb" ["xhk", "hfx"]
  , D25Wire "pzl" ["lsr", "hfx", "nvd"]
  , D25Wire "qnr" ["nvd"]
  , D25Wire "ntq" ["jqt", "hfx", "bvb", "xhk"]
  , D25Wire "nvd" ["lhk"]
  , D25Wire "lsr" ["lhk"]
  , D25Wire "rzs" ["qnr", "cmg", "lsr", "rsh"]
  , D25Wire "frs" ["qnr", "lhk", "lsr"]
  ]

d25TestGraph :: M.Map String (S.Set String)
d25TestGraph = d25Graph d25TestInput

d25PathsAvoiding :: M.Map String (S.Set String) -> String -> S.Set (String, String) -> M.Map String (S.Set (String, String))
d25PathsAvoiding graph start toAvoid = paths (M.singleton start S.empty) [start]
  where
    paths acc [] = acc
    paths acc (v:vs) =
      let vs' =
            S.toList $
            S.filter (\e -> not $ S.member e toAvoid || S.member (swap e) toAvoid) $
            S.map (v, ) $ S.filter (not . flip M.member acc) $ graph M.! v
          p = acc M.! v
       in paths (foldl (\a e -> M.insert (snd e) (S.insert e p) a) acc vs') $ vs ++ map snd vs'

d25Flow :: M.Map String (S.Set String) -> Int -> S.Set (String, String) -> String -> String -> Maybe (S.Set (String, String))
d25Flow graph count toAvoid start finish =
  let p = d25PathsAvoiding graph start toAvoid M.! finish
      plast = d14Repeat (count - 1) (\p' -> S.union p' $ d25PathsAvoiding graph start p' M.! finish) $ S.union p toAvoid
   in if M.member finish $ d25PathsAvoiding graph start plast
        then Nothing
        else Just p

-- >>> d25Flow d25TestGraph 3 S.empty "jqt" "frs"
-- Just (fromList [("jqt","nvd"),("lhk","frs"),("nvd","lhk")])
--
-- >>> d25Flow d25TestGraph 3 S.empty "jqt" "lhk"
-- Just (fromList [("jqt","nvd"),("nvd","lhk")])
--
-- >>> d25Flow d25TestGraph 3 S.empty "jqt" "nvd"
-- Just (fromList [("jqt","nvd")])
--
-- >>> d25Flow d25TestGraph 2 (S.fromList [("jqt","nvd")]) "jqt" "frs"
-- Just (fromList [("bvb","cmg"),("cmg","lhk"),("jqt","ntq"),("lhk","frs"),("ntq","bvb")])
--
-- >>> d25Flow d25TestGraph 2 (S.fromList [("jqt","nvd")]) "jqt" "lhk"
-- Just (fromList [("bvb","cmg"),("cmg","lhk"),("jqt","ntq"),("ntq","bvb")])
--
-- >>> d25Flow d25TestGraph 2 (S.fromList [("jqt","nvd")]) "jqt" "bvb"
-- Nothing
--
-- >>> d25Flow d25TestGraph 2 (S.fromList [("jqt","nvd")]) "bvb" "lhk"
-- Just (fromList [("bvb","cmg"),("cmg","lhk")])
--
-- >>> d25Flow d25TestGraph 2 (S.fromList [("jqt","nvd")]) "bvb" "cmg"
-- Just (fromList [("bvb","cmg")])
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "jqt" "frs"
-- Just (fromList [("hfx","pzl"),("jqt","ntq"),("lsr","frs"),("ntq","hfx"),("pzl","lsr")])
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "jqt" "ntq"
-- Nothing
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "ntq" "frs"
-- Just (fromList [("hfx","pzl"),("lsr","frs"),("ntq","hfx"),("pzl","lsr")])
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "ntq" "lsr"
-- Just (fromList [("hfx","pzl"),("ntq","hfx"),("pzl","lsr")])
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "ntq" "pzl"
-- Just (fromList [("hfx","pzl"),("ntq","hfx")])
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "ntq" "hfx"
-- Nothing
--
-- >>> d25Flow d25TestGraph 1 (S.fromList [("jqt","nvd"),("bvb","cmg")]) "pzl" "hfx"
-- Just (fromList [("pzl","hfx")])
--
-- >>> M.size d25TestGraph
-- 15
--
-- >>> M.size $ d25PathsAvoiding d25TestGraph "jqt" $ S.fromList [("jqt","nvd"),("bvb","cmg"),("pzl","hfx")]
-- 6
--
-- >>> 6 * (15 - 6)
-- 54
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "btz" "lph") $ readFile "d25.txt"
-- Just (fromList [("btz","cxn"),("cxn","znq"),("fxn","gpd"),("gpd","rzx"),("hkq","mtc"),("mbg","lph"),("mtc","ptq"),("ptq","fxn"),("rzx","mbg"),("znq","hkq")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "btz" "gpd") $ readFile "d25.txt"
-- Just (fromList [("btz","cxn"),("cxn","znq"),("fxn","gpd"),("hkq","mtc"),("mtc","ptq"),("ptq","fxn"),("znq","hkq")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "btz" "cxn") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "cxn" "gpd") $ readFile "d25.txt"
-- Just (fromList [("cxn","znq"),("fxn","gpd"),("hkq","mtc"),("mtc","ptq"),("ptq","fxn"),("znq","hkq")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "cxn" "mtc") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "mtc" "gpd") $ readFile "d25.txt"
-- Just (fromList [("fxn","gpd"),("mtc","ptq"),("ptq","fxn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "mtc" "fxn") $ readFile "d25.txt"
-- Just (fromList [("mtc","ptq"),("ptq","fxn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "mtc" "ptq") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 3 (S.fromList []) "ptq" "fxn") $ readFile "d25.txt"
-- Just (fromList [("ptq","fxn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "btz" "lph") $ readFile "d25.txt"
-- Just (fromList [("btz","qrn"),("fbd","hzl"),("hvl","zvg"),("hzl","hvl"),("lzd","fbd"),("mbg","lph"),("mtm","lzd"),("qrn","svv"),("svv","mtm"),("zvg","mbg")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "btz" "hzl") $ readFile "d25.txt"
-- Just (fromList [("btz","qrn"),("fbd","hzl"),("lzd","fbd"),("mtm","lzd"),("qrn","svv"),("svv","mtm")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "btz" "fbd") $ readFile "d25.txt"
-- Just (fromList [("btz","qrn"),("lzd","fbd"),("mtm","lzd"),("qrn","svv"),("svv","mtm")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "btz" "mtm") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "mtm" "fbd") $ readFile "d25.txt"
-- Just (fromList [("lzd","fbd"),("mtm","lzd")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "mtm" "lzd") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 2 (S.fromList [("ptq","fxn")]) "lzd" "fbd") $ readFile "d25.txt"
-- Just (fromList [("lzd","fbd")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "btz" "lph") $ readFile "d25.txt"
-- Just (fromList [("btz","cxn"),("ckc","gxv"),("cxn","ckc"),("ggd","szl"),("gxv","ggd"),("kcn","mhj"),("lsf","nsv"),("mbg","lph"),("mhj","lsf"),("nsv","ttd"),("szl","kcn"),("ttd","mbg")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "btz" "kcn") $ readFile "d25.txt"
-- Just (fromList [("btz","cxn"),("ckc","gxv"),("cxn","ckc"),("ggd","szl"),("gxv","ggd"),("szl","kcn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "btz" "cxn") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "cxn" "kcn") $ readFile "d25.txt"
-- Just (fromList [("ckc","gxv"),("cxn","ckc"),("ggd","szl"),("gxv","ggd"),("szl","kcn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "cxn" "ckc") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "ckc" "kcn") $ readFile "d25.txt"
-- Just (fromList [("ckc","gxv"),("ggd","szl"),("gxv","ggd"),("szl","kcn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "ckc" "ggd") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "ggd" "kcn") $ readFile "d25.txt"
-- Just (fromList [("ggd","szl"),("szl","kcn")])
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "ggd" "szl") $ readFile "d25.txt"
-- Nothing
--
-- >>> fmap (\input -> d25Flow (d25Graph $ map read $ lines input) 1 (S.fromList [("ptq","fxn"),("lzd","fbd")]) "szl" "kcn") $ readFile "d25.txt"
-- Just (fromList [("szl","kcn")])
--
-- >>> fmap (\input -> M.size $ d25Graph $ map read $ lines input) $ readFile "d25.txt"
-- 1514
--
-- >>> fmap (\input -> M.size $ d25PathsAvoiding (d25Graph $ map read $ lines input) "btz" $ S.fromList [("ptq","fxn"),("lzd","fbd"),("szl","kcn")]) $ readFile "d25.txt"
-- 793
--
-- >>> (1514 - 793) * 793
-- 571753
--
{--}
-- Day 24
data D24Stone =
  D24Stone (Integer, Integer, Integer) (Integer, Integer, Integer)
  deriving (Read, Show)

{-
x1 + dx1 * t1 = x2 + dx2 * t2
y1 + dy1 * t1 = y2 + dy2 * t2

D = dx1 * dy2 - dx2 * dy1

t1 = ((x2 - x1) * dy2 - (y2 - y1) * dx2) / D
t2 = ((x2 - x1) * dy1 - (y2 - y1) * dx1) / D

x = x1 + dx1 * t1 = (x1 * dx1 * dy2 - x1 * dx2 * dy1 + x2 * dx1 * dy2 - x1 * dx1 * dy2 - y2 * dx1 * dx2 + y1 * dx1 * dx2) / D = (- x1 * dx2 * dy1 + x2 * dx1 * dy2 - y2 * dx1 * dx2 + y1 * dx1 * dx2) / D
y = y1 + dy1 * t1 = (y1 * dx1 * dy2 - y1 * dx2 * dy1 + x2 * dy1 * dy2 - x1 * dy1 * dy2 - y2 * dx2 * dy1 + y1 * dx2 * dy1) / D = (y1 * dx1 * dy2 + x2 * dy1 * dy2 - x1 * dy1 * dy2 - y2 * dx2 * dy1) / D
-}
d24TestInput :: [D24Stone]
d24TestInput =
  [ D24Stone (19, 13, 30) (-2, 1, -2)
  , D24Stone (18, 19, 22) (-1, -1, -2)
  , D24Stone (20, 25, 34) (-2, -2, -4)
  , D24Stone (12, 31, 28) (-1, -2, -1)
  , D24Stone (20, 19, 15) (1, -5, -3)
  ]

d24TestMin :: Integer
d24TestMin = 7

d24TestMax :: Integer
d24TestMax = 27

d24Min :: Integer
d24Min = 200000000000000

d24Max :: Integer
d24Max = 400000000000000

-- A bit crude (some comparisons might return EQ), but works
d24PathsIntersectInside :: Integer -> Integer -> D24Stone -> D24Stone -> Bool
d24PathsIntersectInside mn mx (D24Stone (x1, y1, _) (dx1, dy1, _)) (D24Stone (x2, y2, _) (dx2, dy2, _)) =
  let d = dx1 * dy2 - dx2 * dy1
      s = compare d 0
      isFuture1 = compare ((x2 - x1) * dy2) ((y2 - y1) * dx2) == s
      isFuture2 = compare ((x2 - x1) * dy1) ((y2 - y1) * dx1) == s
      dtx = x2 * dx1 * dy2 - x1 * dx2 * dy1 - y2 * dx1 * dx2 + y1 * dx1 * dx2
      dty = y1 * dx1 * dy2 - y2 * dx2 * dy1 - x1 * dy1 * dy2 + x2 * dy1 * dy2
      dtmn = d * mn
      dtmx = d * mx
      xInBounds = compare dtx dtmn == s && compare dtmx dtx == s
      yInBounds = compare dty dtmn == s && compare dtmx dty == s
   in d /= 0 && isFuture1 && isFuture2 && xInBounds && yInBounds

-- >>> [(s1, s2) | s1:ss <- tails d24TestInput, s2 <- ss, d24PathsIntersectInside d24TestMin d24TestMax s1 s2]
-- [(D24Stone (19,13,30) (-2,1,-2),D24Stone (18,19,22) (-1,-1,-2)),(D24Stone (19,13,30) (-2,1,-2),D24Stone (20,25,34) (-2,-2,-4))]
--
-- >>> fmap ((\input -> length [(s1, s2) | s1:ss <- tails input, s2 <- ss, d24PathsIntersectInside d24Min d24Max s1 s2]) . map read . lines) $ readFile "d24.txt"
-- 21785
--
{-
x1 + t * dx1 = x + t * dx
y1 + t * dy1 = y + t * dy
z1 + t * dz1 = z + t * dz

t * (dx1 - dx) = x - x1
t * (dy1 - dy) = y - y1
t * (dz1 - dz) = z - z1

(dx1 - dx) * (y - y1) = (dy1 - dy) * (x - x1)
(dx1 - dx) * (z - z1) = (dz1 - dz) * (x - x1)
(dy1 - dy) * (z - z1) = (dz1 - dz) * (y - y1)
*** redundancy: 1 ***

D24Stone (262130794315133, 305267994111063, 163273807102793) (57, -252, 150)
D24Stone (290550702673836, 186986670515285, 231769402282435) (-74, 19, -219)
D24Stone (275698513286341, 162656001312879, 183065006152383) (-59, -24, -225)
D24Stone (300978671520025, 310644717172257, 264594178261059) (-12, -15, 13)
D24Stone (255863566365481, 175389174276099, 191768173516493) (111, 36, -70)

(dx - 57) * (y - 305267994111063) = (dy + 252) * (x - 262130794315133)
(dx + 74) * (y - 186986670515285) = (dy - 19) * (x - 290550702673836)
(dx + 59) * (y - 162656001312879) = (dy + 24) * (x - 275698513286341)
(dx + 12) * (y - 310644717172257) = (dy + 15) * (x - 300978671520025)
(dx - 111) * (y - 175389174276099) = (dy - 36) * (x - 255863566365481)
dx * y - dx * 305267994111063 - y * 57 + 17400275664330591 = dy * x - dy * 262130794315133 + x * 252 - 66056960167413516
dx * y - dx * 186986670515285 + y * 74 - 13837013618131090 = dy * x - dy * 290550702673836 - x * 19 + 5520463350802884
dx * y - dx * 162656001312879 + y * 59 - 9596704077459861 = dy * x - dy * 275698513286341 + x * 24 - 6616764318872184
dx * y - dx * 310644717172257 + y * 12 - 3727736606067084 = dy * x - dy * 300978671520025 + x * 15 - 4514680072800375
dx * y - dx * 175389174276099 - y * 111 + 19468198344646989 = dy * x - dy * 255863566365481 - x * 36 + 9211088389157316

[2 - 1] dx * 118281323595778 + y * 131 - 31237289282461681 = - dy * 28419908358703 - x * 271 + 71577423518216400
[3 - 1] dx * 142611992798184 + y * 116 - 26996979741790452 = - dy * 13567718971208 - x * 228 + 59440195848541332
[1 - 4] dx * 5376723061194 - y * 69 + 21128012270397675 = dy * 38847877204892 + x * 237 - 61542280094613141
[5 - 1] dx * 129878819834964 - y * 54 + 2067922680316398 = dy * 6267227949652 - x * 288 + 75268048556570832

dx * 118281323595778 + y * 131 + dy * 28419908358703 + x * 271 = 102814712800678081
dx * 142611992798184 + y * 116 + dy * 13567718971208 + x * 228 = 86437175590331784
dx * 5376723061194 - y * 69 - dy * 38847877204892 - x * 237 = - 82670292365010816
dx * 129878819834964 - y * 54 - dy * 6267227949652 + x * 288 = 73200125876254434

[1 x 2] dx * 26968141779837384 + y * 29868 + dy * 6479739105784284 + x * 61788 = 23441754518554602468
        dx * 38647850048307864 + y * 31436 + dy * 3676851841197368 + x * 61788 = 23424474584979913464
    dx * 11679708268470480 + y * 1568 - dy * 2802887264586916 = - 17279933574689004
[1 x 3] dx * 28032673692199386 + y * 31047 + dy * 6735518281012611 + x * 64227 = 24367086933760705197
        dx * 1457091949583574 - y * 18699 - dy * 10527774722525732 - x * 64227 = - 22403649230917931136
    dx * 29489765641782960 + y * 12348 - dy * 3792256441513121 = 1963437702842774061
[1 x 4] dx * 34065021195584064 + y * 37728 + dy * 8184933607306464 + x * 78048 = 29610637286595287328
        dx * 35197160175275244 - y * 14634 - dy * 1698418774355692 + x * 78048 = 19837234112464951614
    dx * 1132138979691180 - y * 52362 - dy * 9883352381662156 = - 9773403174130335714

dx * 11679708268470480 + y * 1568 - dy * 2802887264586916 = - 17279933574689004
dx * 29489765641782960 + y * 12348 - dy * 3792256441513121 = 1963437702842774061
dx * 1132138979691180 - y * 52362 - dy * 9883352381662156 = - 9773403174130335714

[1 x 2] dx * 144221037699073487040 + y * 19361664 - dy * 34610051943119238768 = - 213372619780259821392
        dx * 46239952526315681280 + y * 19361664 - dy * 5946258100292573728 = 3078670318057469727648
    dx * 97981085172757805760 - dy * 28663793842826665040 = - 3292042937837729549040
[1 x 3] dx * 611572884353651273760 + y * 82103616 - dy * 146764782948300095592 = - 904811881837865627448
        dx * 1775193920155770240 - y * 82103616 - dy * 15497096534446260608 = - 15324696177036366399552
    dx * 613348078273807044000 - dy * 162261879482746356200 = - 16229508058874232027000

dx * 97981085172757805760 - dy * 28663793842826665040 = - 3292042937837729549040
dx * 613348078273807044000 - dy * 162261879482746356200 = - 16229508058874232027000

dx * 92233117302468 - dy * 26982259434847 = - 3098918346484797
dx * 11316385208003820 - dy * 2993761614072811 = - 299437418060410185

dx * 276123966126403511367261997548 - dy * 80778452556998887854768644917 = - 9277422790852172436908262554367
dx * 305341641547024102774217115540 - dy * 80778452556998887854768644917 = - 8079498098606728189223102716695

dx * 29217675420620591406955117992 = 1197924692245444247685159837672

dx = 41

dy * 26982259434847 = 6880476155885985

dy = 255

y * 1568 = 2802887264586916 * 255 - 11679708268470480 * 41 - 17279933574689004 = 218588279887684896

y = 139405790744697

x * 271 = 102814712800678081 - 118281323595778 * 41 - 131 * 139405790744697 - 28419908358703 * 255 = 72455943314226611

x = 267365104480541

(dx - 57) * (z - 163273807102793) = (dz - 150) * (x - 262130794315133)
(dx + 74) * (z - 231769402282435) = (dz + 219) * (x - 290550702673836)

(z - 163273807102793) * 16 + (dz - 150) * 5234310165408 = 0
(z - 231769402282435) * 115 + (dz + 219) * 23185598193295 = 0

z * 16 + dz * 5234310165408 = 3397527438455888
z * 115 + dz * 23185598193295 = 21575835258148420

z * 1840 + dz * 601945669021920 = 390715655422427120
z * 1840 + dz * 370969571092720 = 345213364130374720

dz * 230976097929200 = 45502291292052400

dz = 197

z * 16 = 3397527438455888 - 197 * 5234310165408 = 2366368335870512

z = 147898020991907

x + y + z = 267365104480541 + 139405790744697 + 147898020991907 = 554668916217145
-}
-- >>> gcd (gcd 28663793842826665040 97981085172757805760) 3292042937837729549040
-- 1062320
--
-- >>> gcd (gcd 613348078273807044000 162261879482746356200) 16229508058874232027000
-- 54200
--
-- >>> let {x = 267365104480541; y = 139405790744697; dx = 41; dy = 255} in (dx - 57) * (y - 305267994111063) == (dy + 252) * (x - 262130794315133)
-- True
--
-- >>> let {x = 267365104480541; y = 139405790744697; dx = 41; dy = 255} in (dx + 74) * (y - 186986670515285) == (dy - 19) * (x - 290550702673836)
-- True
--
-- >>> let {x = 267365104480541; y = 139405790744697; dx = 41; dy = 255} in (dx + 59) * (y - 162656001312879) == (dy + 24) * (x - 275698513286341)
-- True
--
-- >>> let {x = 267365104480541; y = 139405790744697; dx = 41; dy = 255} in (dx + 12) * (y - 310644717172257) == (dy + 15) * (x - 300978671520025)
-- True
--
-- >>> let {x = 267365104480541; y = 139405790744697; dx = 41; dy = 255} in (dx - 111) * (y - 175389174276099) == (dy - 36) * (x - 255863566365481)
-- True
--
{--}
-- Day 23
d23Reachable :: [String] -> [[((Int, Int), Char, S.Set (Int, Int))]]
d23Reachable input =
  let maxX = length $ head input
      maxY = length input
      result =
        [((1, 0), '.', S.singleton (1, 0))] :
        map
          (\level ->
             [ (cs', c', S.insert cs' visited)
             | (cs@(x, y), c, visited) <- level
             , cs'@(x', y') <-
                 case c of
                   '.' -> [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                   '>' -> [(x + 1, y)]
                   '<' -> [(x - 1, y)]
                   '^' -> [(x, y - 1)]
                   'v' -> [(x, y + 1)]
             , x' >= 0
             , x' < maxX
             , y' >= 0
             , y' < maxY
             , let c' = input !! y' !! x'
             , c' /= '#'
             , not $ S.member cs' visited
             ])
          result
   in result

d23Reachable' :: [String] -> (Int, Int) -> [[((Int, Int), S.Set (Int, Int))]]
d23Reachable' input start =
  let maxX = length $ head input
      maxY = length input
      result =
        [(start, S.singleton start)] :
        map
          (\level ->
             [ (cs', S.insert cs' visited)
             | (cs@(x, y), visited) <- level
             , cs'@(x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
             , x' >= 0
             , x' < maxX
             , y' >= 0
             , y' < maxY
             , input !! y' !! x' /= '#'
             , not $ S.member cs' visited
             ])
          result
   in result

d23TestInput :: [String]
d23TestInput =
  [ "#.#####################"
  , "#.......#########...###"
  , "#######.#########.#.###"
  , "###.....#.>.>.###.#.###"
  , "###v#####.#v#.###.#.###"
  , "###.>...#.#.#.....#...#"
  , "###v###.#.#.#########.#"
  , "###...#.#.#.......#...#"
  , "#####.#.#.#######.#.###"
  , "#.....#.#.#.......#...#"
  , "#.#####.#.#.#########v#"
  , "#.#...#...#...###...>.#"
  , "#.#.#v#######v###.###v#"
  , "#...#.>.#...>.>.#.###.#"
  , "#####v#.#.###v#.#.###.#"
  , "#.....#...#...#.#.#...#"
  , "#.#########.###.#.#.###"
  , "#...###...#...#...#.###"
  , "###.###.#.###v#####v###"
  , "#...#...#.#.>.>.#.>.###"
  , "#.###.###.#.###.#.#v###"
  , "#.....###...###...#...#"
  , "#####################.#"
  ]

d23GetCrossings :: [String] -> [(Int, Int)]
d23GetCrossings input =
  let maxX = length $ head input
      maxY = length input
   in filter
        (\(x, y) ->
           input !! y !! x /= '#' &&
           length (filter (\(i, j) -> input !! j !! i /= '#') [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]) >= 3)
        [(x, y) | x <- [1 .. maxX - 2], y <- [1 .. maxY - 2]]

d23ReachableExcept :: [String] -> [(Int, Int)] -> (Int, Int) -> [[((Int, Int), S.Set (Int, Int))]]
d23ReachableExcept input excluded start =
  let maxX = length $ head input
      maxY = length input
      result =
        [(start, S.singleton start)] :
        map
          (\level ->
             [ (cs', S.insert cs' visited)
             | (cs@(x, y), visited) <- level
             , cs == start || not (elem cs excluded)
             , cs'@(x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
             , x' >= 0
             , x' < maxX
             , y' >= 0
             , y' < maxY
             , input !! y' !! x' /= '#'
             , not $ S.member cs' visited
             ])
          result
   in result

d23Distance :: [String] -> [(Int, Int)] -> Int -> Int -> Int
d23Distance input crossings start end =
  pred $
  length $
  dropWhile null $
  reverse $ map (filter $ (crossings !! end ==) . fst) $ takeWhile (not . null) $ d23ReachableExcept input crossings $ crossings !! start

-- >>> pred $ length $ takeWhile (not . null) $ d23Reachable d23TestInput
-- 94
--
-- >>> fmap (pred . length . takeWhile (not . null) . d23Reachable . lines) $ readFile "d23.txt"
-- 2214
--
-- >>> pred $ length $ dropWhile null $ reverse $ map (filter (((21, 22) ==) . fst)) $ takeWhile (not . null) $ d23Reachable' d23TestInput (1,0)
-- 154
--
-- >>> fmap (sum . map (length . filter ('#' /=)) . lines) $ readFile "d23.txt"
-- 9392
--
-- >>> d23GetCrossings d23TestInput
-- [(3,5),(5,13),(11,3),(13,13),(13,19),(19,19),(21,11)]
--
-- >>> d23Distance d23TestInput ((1,0):(21,22):d23GetCrossings d23TestInput) 0 2
-- 15
--
-- >>> d23Distance d23TestInput ((1,0):(21,22):d23GetCrossings d23TestInput) 0 1
-- -1
--
-- >>> let crossings = (1,0):(21,22):d23GetCrossings d23TestInput in length $ filter ((0 <=) . snd) [((i,j), d23Distance d23TestInput crossings i j) | i <- [0..length crossings-1], j <- [0..length crossings-1], j /= i]
-- 24
--
-- >>> let crossings = (1,0):(21,22):d23GetCrossings d23TestInput in mapM_ print $ filter ((0 <=) . snd) [((i,j), d23Distance d23TestInput crossings i j) | i <- [0..length crossings-1], j <- [0..i-1], j /= i]
-- ((2,0),15)
-- ((3,2),22)
-- ((4,2),22)
-- ((5,3),12)
-- ((5,4),24)
-- ((6,3),38)
-- ((6,5),10)
-- ((7,1),5)
-- ((7,6),10)
-- ((8,4),30)
-- ((8,5),18)
-- ((8,7),10)
--
{-
0 -[15]- 2 -[22]- 4 ----- [30]
         |        |        |
        [22]     [24]      |
         |        |        |
         3 -[12]- 5 -[18]- 8
         |        |        |
         |       [10]     [10]
         |        |        |
        [38] ---- 6 -[10]- 7 --[5]- 1

0 -[15]- 2        4 ----- [30]
         |        |        |
        [22]     [24]      |
         |        |        |
         3        5        8
         |        |        |
         |       [10]     [10]
         |        |        |
        [38] ---- 6        7 --[5]- 1

-}
-- >>> fmap (length . d23GetCrossings . lines) $ readFile "d23.txt"
-- 34
--
-- >>> fmap (d23GetCrossings . lines) $ readFile "d23.txt"
-- [(7,9),(7,35),(7,101),(9,77),(13,65),(29,89),(29,105),(31,67),(33,133),(41,5),(43,39),(57,11),(59,53),(59,127),(61,39),(61,81),(65,101),(75,83),(77,135),(79,109),(83,17),(83,37),(89,61),(105,83),(107,65),(107,135),(109,99),(111,15),(113,29),(125,137),(127,81),(131,65),(135,29),(135,101)]
--
-- >>> readFile "d23.txt" >>= (\input -> let crossings = (1,0):(139,140):d23GetCrossings input in mapM_ print $ filter ((0 <=) . snd) [((i,j), d23Distance input crossings i j) | i <- [0..length crossings-1], j <- [0..i-1], j /= i]) . lines
-- ((2,0),47)
-- ((3,2),198)
-- ((5,4),170)
-- ((6,3),240)
-- ((6,5),84)
-- ((7,5),112)
-- ((8,4),134)
-- ((8,7),76)
-- ((9,6),76)
-- ((9,7),68)
-- ((10,4),382)
-- ((10,8),220)
-- ((11,2),306)
-- ((12,3),168)
-- ((12,9),220)
-- ((12,11),232)
-- ((13,11),94)
-- ((14,9),202)
-- ((15,10),232)
-- ((16,12),90)
-- ((16,13),148)
-- ((16,14),80)
-- ((17,7),232)
-- ((17,14),142)
-- ((18,8),228)
-- ((18,15),116)
-- ((18,17),84)
-- ((19,17),52)
-- ((20,15),130)
-- ((21,18),46)
-- ((21,19),146)
-- ((21,20),212)
-- ((22,13),212)
-- ((23,16),124)
-- ((23,22),96)
-- ((24,14),154)
-- ((24,19),168)
-- ((24,23),174)
-- ((25,19),186)
-- ((26,24),50)
-- ((26,25),52)
-- ((27,20),170)
-- ((28,21),184)
-- ((28,25),68)
-- ((28,27),290)
-- ((29,22),282)
-- ((30,23),170)
-- ((30,26),290)
-- ((30,29),32)
-- ((31,1),57)
-- ((31,27),48)
-- ((32,25),128)
-- ((33,26),140)
-- ((33,32),100)
-- ((34,29),354)
-- ((34,30),54)
-- ((34,33),296)
-- ((35,28),104)
-- ((35,31),314)
-- ((35,32),152)
--
{-
0 --[47]- 2 -[306]- 11 --[94]- 13 -[212]- 22 -[282]- 29 ----- [354]
          |         |          |          |          |          |
        [198]     [232]      [148]       [96]       [32]        |
          |         |          |          |          |          |
          3 -[168]- 12 --[90]- 16 -[124]- 23 -[170]- 30 --[54]- 34
          |         |          |          |          |          |
        [240]     [220]       [80]      [174]      [290]      [296]
          |         |          |          |          |          |
          6 --[76]- 9 --[202]- 14 -[154]- 24 --[50]- 26 -[140]- 33
          |         |          |          |          |          |
         [84]      [68]      [142]      [168]       [52]      [100]
          |         |          |          |          |          |
          5 -[112]- 7 --[232]- 17 --[52]- 19 -[186]- 25 -[128]- 32
          |         |          |          |          |          |
        [170]      [76]       [84]      [146]       [68]      [152]
          |         |          |          |          |          |
          4 -[134]- 8 --[228]- 18 --[46]- 21 -[184]- 28 -[104]- 35
          |         |          |          |          |          |
          |       [220]      [116]      [212]      [290]      [314]
          |         |          |          |          |          |
        [382] ----- 10 -[232]- 15 -[130]- 20 -[170]- 27 --[48]- 31 --[57]- 1
-}
d23Matrix :: M.Map (Int, Int) Int
d23Matrix =
  M.fromList $
  concat $
  zipWith
    (\y -> zipWith (\x n -> ((x, y), n)) [0 ..])
    [0 ..]
    [ [2, 11, 13, 22, 29, 29]
    , [3, 12, 16, 23, 30, 34]
    , [6, 9, 14, 24, 26, 33]
    , [5, 7, 17, 19, 25, 32]
    , [4, 8, 18, 21, 28, 35]
    , [10, 10, 15, 20, 27, 31]
    ]

-- input should be read from d23.txt and nothing else
d23Distances :: [String] -> M.Map ((Int, Int), (Int, Int)) Int
d23Distances input =
  let crossings = (1, 0) : (139, 140) : d23GetCrossings input
   in M.fromList
        [ ((cs, cs'), d23Distance input crossings (d23Matrix M.! cs) (d23Matrix M.! cs'))
        | x <- [0 .. 5]
        , y <- [0 .. 5]
        , let cs = (x, y)
        , cs'@(x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        , x' >= 0
        , x' <= 5
        , y' >= 0
        , y' <= 5
        ]

d23ChessPaths :: M.Map ((Int, Int), (Int, Int)) Int -> [[((Int, Int), Int, S.Set (Int, Int))]]
d23ChessPaths distances = takeWhile (not . null) cp
  where
    cp =
      [((0, 0), 0, S.singleton (0, 0))] :
      map
        (\level ->
           [ (cs', l + (distances M.! (cs, cs')), S.insert cs' path)
           | (cs@(x, y), l, path) <- level
           , cs'@(x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
           , x' >= 0
           , x' <= 5
           , y' >= 0
           , y' <= 5
           , not $ S.member cs' path
           ])
        cp

d23FindMax :: [String] -> Int
d23FindMax input =
  let distances = d23Distances input
   in maximum $ map (\(_, n, _) -> n) $ filter (\(c, _, _) -> c == (5, 5)) $ concat $ d23ChessPaths distances

-- >>> :set +s
-- >>> fmap (d23FindMax . lines) $ readFile "d23.txt"
-- >>> :unset +s
-- (0.00 secs, 105,968 bytes)
-- 6490
-- (187.68 secs, 84,290,508,808 bytes)
--
-- >>> 6490 + 57 + 47
-- 6594
--
d23ChessPaths' :: M.Map ((Int, Int), (Int, Int)) Int -> [[((Int, Int), Int, S.Set (Int, Int), [(Int, Int)])]]
d23ChessPaths' distances = takeWhile (not . null) cp
  where
    cp =
      [((0, 0), 0, S.singleton (0, 0), [(0, 0)])] :
      map
        (\level ->
           [ (cs', l + (distances M.! (cs, cs')), S.insert cs' path, cs' : path')
           | (cs@(x, y), l, path, path') <- level
           , cs'@(x', y') <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
           , x' >= 0
           , x' <= 5
           , y' >= 0
           , y' <= 5
           , not $ S.member cs' path
           ])
        cp

d23FindMax' :: [String] -> [(Int, Int)]
d23FindMax' input =
  let distances = d23Distances input
   in reverse $
      head $ map (\(_, _, _, path) -> path) $ filter (\(c, n, _, _) -> c == (5, 5) && n == 6490) $ concat $ d23ChessPaths' distances

-- >>> :set +s
-- >>> fmap (d23FindMax' . lines) $ readFile "d23.txt"
-- >>> :unset +s
-- (0.00 secs, 106,752 bytes)
-- [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(5,1),(5,2),(4,2),(4,1),(3,1),(3,2),(2,2),(1,2),(1,1),(0,1),(0,2),(0,3),(1,3),(2,3),(2,4),(1,4),(0,4),(0,5),(1,5),(2,5),(3,5),(4,5),(4,4),(3,4),(3,3),(4,3),(5,3),(5,4),(5,5)]
-- (196.04 secs, 88,248,086,784 bytes)
--
{-
0 --[47]- 2 -[306]- 11 --[94]- 13 -[212]- 22 -[282]- 29 ----- [354]
                                                                |
                                                                |
                                                                |
          3 -[168]- 12                    23 -[170]- 30         34
          |         |                     |          |          |
        [240]     [220]                 [174]      [290]      [296]
          |         |                     |          |          |
          6         9 --[202]- 14 -[154]- 24         26 -[140]- 33
          |
         [84]
          |
          5 -[112]- 7 --[232]- 17         19 -[186]- 25 -[128]- 32
                               |          |                     |
                              [84]      [146]                 [152]
                               |          |                     |
          4 -[134]- 8 --[228]- 18         21 -[184]- 28         35
          |                                          |          |
          |                                        [290]      [314]
          |                                          |          |
        [382] ----- 10 -[232]- 15 -[130]- 20 -[170]- 27         31 --[57]- 1
-}
{--}
-- Day 22
data D22Brick =
  D22Brick (Int, Int, Int) (Int, Int, Int)
  deriving (Read, Show)

d22BrickLength :: D22Brick -> Int
d22BrickLength (D22Brick (x1, y1, z1) (x2, y2, z2)) =
  if x1 == x2
    then if y1 == y2
           then abs (z2 - z1) + 1
           else if z1 == z2
                  then abs (y2 - y1) + 1
                  else error "Faulty brick"
    else if y1 == y2 && z1 == z2
           then abs (x2 - x1) + 1
           else error "Faulty brick"

-- >>> fmap (sum . map d22BrickLength . map read . lines) $ readFile "d22.txt"
-- 4269
--
d22TestInput :: [D22Brick]
d22TestInput =
  [ D22Brick (1, 0, 1) (1, 2, 1)
  , D22Brick (0, 0, 2) (2, 0, 2)
  , D22Brick (0, 2, 3) (2, 2, 3)
  , D22Brick (0, 0, 4) (0, 2, 4)
  , D22Brick (2, 0, 5) (2, 2, 5)
  , D22Brick (0, 1, 6) (2, 1, 6)
  , D22Brick (1, 1, 8) (1, 1, 9)
  ]

-- >>> fmap (and . map (\(D22Brick (x1, y1, z1) (x2, y2, z2)) -> x1 <= x2 && y1 <= y2 && z1 <= z2) . map read . lines) $ readFile "d22.txt"
-- True
--
d22Area :: D22Brick -> [(Int, Int)]
d22Area (D22Brick (x1, y1, _) (x2, y2, _)) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

d22PlaceBrick :: D22Brick -> M.Map (Int, Int) Int -> (D22Brick, M.Map (Int, Int) Int)
d22PlaceBrick brick@(D22Brick (x1, y1, z1) (x2, y2, z2)) surface =
  let under = d22Area brick
      level = foldl1 max $ map (\cs -> fromMaybe 1 $ M.lookup cs surface) under
      newLevel = z2 - z1 + level + 1
   in (D22Brick (x1, y1, level) (x2, y2, z2 - z1 + level), foldl (\m coords -> M.insert coords newLevel m) surface under)

d22PlaceBricks :: [D22Brick] -> [D22Brick]
d22PlaceBricks bricks =
  let sorted = sortBy (on compare $ \(D22Brick (_, _, z1) _) -> z1) bricks
   in evalState (forM sorted $ state . d22PlaceBrick) M.empty

-- >>> d22PlaceBricks d22TestInput
-- [D22Brick (1,0,1) (1,2,1),D22Brick (0,0,2) (2,0,2),D22Brick (0,2,2) (2,2,2),D22Brick (0,0,3) (0,2,3),D22Brick (2,0,3) (2,2,3),D22Brick (0,1,4) (2,1,4),D22Brick (1,1,5) (1,1,6)]
--
d22Supports :: D22Brick -> D22Brick -> Bool
d22Supports (D22Brick (x1, y1, z1) (x2, y2, z2)) (D22Brick (x1', y1', z1') (x2', y2', z2')) =
  z1' == z2 + 1 && x1 <= x2' && x2 >= x1' && y1 <= y2' && y2 >= y1'

d22AllSupports :: [D22Brick] -> S.Set (Int, Int)
d22AllSupports bricks =
  S.fromList $ do
    (n, b):nbs <- tails $ zip [0 ..] bricks
    map ((n, ) . fst) $ filter (d22Supports b . snd) nbs

-- >>> d22AllSupports $ d22PlaceBricks d22TestInput
-- fromList [(0,1),(0,2),(1,3),(1,4),(2,3),(2,4),(3,5),(4,5),(5,6)]
--
d22Stable :: Int -> S.Set (Int, Int) -> [Int]
d22Stable l s = filter (\n -> (2 <=) $ S.size $ S.filter ((n ==) . snd) s) [0 .. l - 1]

d22CanBeRemoved :: Int -> S.Set (Int, Int) -> [Int]
d22CanBeRemoved l s =
  let stable = d22Stable l s
   in filter (\n -> all (\(m, k) -> m /= n || elem k stable) s) [0 .. l - 1]

-- >>> d22CanBeRemoved (length d22TestInput) $ d22AllSupports $ d22PlaceBricks d22TestInput
-- [1,2,3,4,6]
--
d22Answer :: [D22Brick] -> [Int]
d22Answer bricks = d22CanBeRemoved (length bricks) $ d22AllSupports $ d22PlaceBricks bricks

-- >>> length $ d22Answer d22TestInput
-- 5
--
-- >>> fmap (length . d22Answer . map read . lines) $ readFile "d22.txt"
-- 485
--
d22Bottom :: [D22Brick] -> [Int]
d22Bottom input = map fst $ filter (\(_, D22Brick (_, _, z1) _) -> z1 == 1) $ zip [0 ..] input

d22Supporting :: S.Set (Int, Int) -> M.Map Int [Int]
d22Supporting = foldl (\acc (m, k) -> M.alter (Just . (m :) . fromMaybe []) k acc) M.empty

d22WouldFall :: Int -> [Int] -> M.Map Int [Int] -> Int -> [Int]
d22WouldFall l b s i =
  foldl
    (\acc n ->
       if n /= i && not (elem n b) && all (flip elem acc) (fromMaybe [] $ M.lookup n s)
         then (n : acc)
         else acc)
    [i]
    [0 .. l - 1]

-- >>> d22WouldFall (length d22TestInput) (d22Bottom $ d22PlaceBricks d22TestInput) (d22Supporting $ d22AllSupports $ d22PlaceBricks d22TestInput) 0
-- [6,5,4,3,2,1,0]
--
-- >>> d22WouldFall (length d22TestInput) (d22Bottom $ d22PlaceBricks d22TestInput) (d22Supporting $ d22AllSupports $ d22PlaceBricks d22TestInput) 5
-- [6,5]
--
d22Result :: [D22Brick] -> [Int]
d22Result input =
  let bricks = d22PlaceBricks input
      bottom = d22Bottom bricks
      supports = d22Supporting $ d22AllSupports bricks
      len = length bricks
   in map (length . d22WouldFall len bottom supports) [0 .. len - 1]

-- >>> d22Result d22TestInput
-- [7,1,1,1,1,2,1]
--
-- >>> fmap ((\input -> sum (d22Result input) - length input) . map read . lines) $ readFile "d22.txt"
-- 74594
--
{--}
-- Day 21
d21Step :: [String] -> M.Map (Int, Int) (S.Set (Int, Int))
d21Step input =
  let maxX = length (head input) - 1
      maxY = length input - 1
   in M.fromList
        [ ((x, y), S.fromList cs)
        | x <- [0 .. maxX]
        , y <- [0 .. maxY]
        , let cs =
                filter
                  (\(x', y') ->
                     x' >= 0 &&
                     y' >= 0 &&
                     x' <= maxX &&
                     y' <= maxY &&
                     let c = input !! y' !! x'
                      in c == '.' || c == 'S')
                  [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        ]

d21Add :: M.Map (Int, Int) (S.Set (Int, Int)) -> M.Map (Int, Int) (S.Set (Int, Int)) -> M.Map (Int, Int) (S.Set (Int, Int))
d21Add m1 m2 = M.map (S.unions . S.map (m2 M.!)) m1

d21Double :: M.Map (Int, Int) (S.Set (Int, Int)) -> M.Map (Int, Int) (S.Set (Int, Int))
d21Double m = d21Add m m

d21Triple :: M.Map (Int, Int) (S.Set (Int, Int)) -> M.Map (Int, Int) (S.Set (Int, Int))
d21Triple m = d21Add m $ d21Double m

d21TestInput :: [String]
d21TestInput =
  [ "..........."
  , ".....###.#."
  , ".###.##..#."
  , "..#.#...#.."
  , "....#.#...."
  , ".##..S####."
  , ".##..#...#."
  , ".......##.."
  , ".##.#.####."
  , ".##..##.##."
  , "..........."
  ]

d21TestStart :: (Int, Int)
d21TestStart = (5, 5)

d21Start :: (Int, Int)
d21Start = (65, 65)

-- >>> S.size $ (d21Double $ d21Triple $ d21Step d21TestInput) M.! d21TestStart
-- 16
--
-- >>> fmap (\input -> S.size $ (d21Double $ d21Double $ d21Double $ d21Double $ d21Double $ d21Double $ d21Step $ lines input) M.! d21Start) $ readFile "d21.txt"
-- 3687
--
d21AmendPic :: [String] -> S.Set (Int, Int) -> [String]
d21AmendPic input result =
  zipWith
    (\y ->
       zipWith
         (\x c ->
            if S.member (x, y) result
              then '#'
              else c)
         [0 ..])
    [0 ..]
    input

d21Reachable :: [String] -> (Int, Int) -> Int -> S.Set (Int, Int)
d21Reachable _ coords 0 = S.singleton coords
d21Reachable input coords n =
  let maxX = length $ head input
      maxY = length input
   in S.unions $
      S.map
        (\(x, y) ->
           S.fromList $
           filter (\(x', y') -> input !! (mod y' maxY) !! (mod x' maxX) /= '#') [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]) $
      d21Reachable input coords (n - 1)

-- >>> divMod 26501365 131
-- (202300,65)
--
-- >>> fmap (S.size . (\input -> d21Reachable input d21Start 65) . lines) $ readFile "d21.txt"
-- 3778
--
-- >>> :set +s
-- >>> fmap (S.size . (\input -> d21Reachable input d21Start (65 + 131)) . lines) $ readFile "d21.txt"
-- >>> :unset +s
-- (0.00 secs, 105,968 bytes)
-- 33695
-- (18.68 secs, 19,577,828,600 bytes)
--
-- >>> :set +s
-- >>> fmap (S.size . (\input -> d21Reachable input d21Start (65 + 2 * 131)) . lines) $ readFile "d21.txt"
-- >>> :unset +s
-- (0.00 secs, 106,752 bytes)
-- 93438
-- (89.48 secs, 98,263,090,424 bytes)
--
-- >>> :set +s
-- >>> fmap (S.size . (\input -> d21Reachable input d21Start (65 + 3 * 131)) . lines) $ readFile "d21.txt"
-- >>> :unset +s
-- (0.00 secs, 105,968 bytes)
-- 183007
-- (251.76 secs, 283,976,496,048 bytes)
--
-- >>> :set +s
-- >>> fmap (S.size . (\input -> d21Reachable input d21Start (65 + 4 * 131)) . lines) $ readFile "d21.txt"
-- >>> :unset +s
-- (0.00 secs, 105,968 bytes)
-- 302402
-- (542.62 secs, 631,079,159,464 bytes)
--
{-
n=0    n=1     n=2     n=3      n=4
3778  33695   93438   183007   302402  3778 + 29917*n + 29826 * n * (n-1) / 2 = 3778 + (29917 - 14913) * n + 14913 * n^2 = 3778 + 15004 * n + 14913 * n^2
  29917   59743   89569   119395       29917 + 29826 * n
      29826   29826   29826            29826
            0       0                  0

Note: I did expect n=1..4 to make a nice quadratic pattern; I did not expect n=0 to fit there as well.
-}
d21Count :: Int -> Int
d21Count n = 3778 + 15004 * n + 14913 * n ^ 2

-- >>> map d21Count [0..4]
-- [3778,33695,93438,183007,302402]
--
-- >>> d21Count 202300
-- 610321885082978
--
{- -- >>> readFile "d21.txt" >>= (\input -> mapM_ putStrLn $ d21AmendPic input $ S.union (d21Reachable input d21Start 65) (d21Reachable input d21Start 64)) . lines -}
{--}
-- Day 20
data D20Category
  = D20Flip
  | D20Neg
  deriving (Read, Show)

data D20Module =
  D20Module D20Category String [String]
  deriving (Read, Show)

type D20System = M.Map String [String]

type D20State = (M.Map String Bool, M.Map String (S.Set String))

d20Process :: [D20Module] -> (D20System, D20State)
d20Process modules =
  let (s, (f, n)) =
        foldl
          (\(sys, (flips, negs)) (D20Module cat name outs) ->
             ( M.insert name outs sys
             , case cat of
                 D20Flip -> (M.insert name False flips, negs)
                 D20Neg  -> (flips, M.insert name S.empty negs)))
          (M.empty, (M.empty, M.empty))
          modules
   in (s, (f, foldl (\negs (D20Module _ name outs) -> foldl (\negs' out -> M.adjust (S.insert name) out negs') negs outs) n modules))

data D20Signal =
  D20Signal String Bool String
  deriving (Read, Show)

d20Handle :: D20System -> D20Signal -> D20State -> ([D20Signal], D20State)
d20Handle system (D20Signal from isHigh to) (flipState, negState) =
  case M.lookup to negState of
    Just sources ->
      let newSources =
            if isHigh
              then S.delete from sources
              else S.insert from sources
       in (map (D20Signal to $ not $ S.null newSources) $ system M.! to, (flipState, M.insert to newSources negState))
    Nothing ->
      if not isHigh
        then case M.lookup to flipState of
               Just isOn -> (map (D20Signal to $ not isOn) $ system M.! to, (M.insert to (not isOn) flipState, negState))
               Nothing   -> ([], (flipState, negState))
        else ([], (flipState, negState))

d20Step :: D20System -> ([D20Signal], D20State) -> ([D20Signal], D20State)
d20Step system (signals, st) = runState (fmap concat $ mapM (state . d20Handle system) signals) st

d20FullRun :: D20System -> D20State -> [D20Signal] -> ([D20Signal], D20State)
d20FullRun system state signals = fullRun (signals, state)
  where
    fullRun ([], st) = ([], st)
    fullRun other@(ss, _) =
      let (ss', st) = fullRun $ d20Step system other
       in (ss ++ ss', st)

d20Broadcast :: D20System -> D20State -> ([D20Signal], D20State)
d20Broadcast system state = d20FullRun system state [D20Signal "button" True "broadcaster"]

d20SignalsCount :: D20System -> State (D20State, (Int, Int)) ()
d20SignalsCount system =
  state $ \(st, (nt, nf)) ->
    let (signals, st') = d20Broadcast system st
        (t, f) = partition (\(D20Signal _ isHigh _) -> isHigh) signals
     in ((), (st', (nt + length t - 1, nf + length f + 1)))

d20TestInput :: [D20Module]
d20TestInput =
  [ D20Module D20Neg "broadcaster" ["a", "b", "c"]
  , D20Module D20Flip "a" ["b"]
  , D20Module D20Flip "b" ["c"]
  , D20Module D20Flip "c" ["inv"]
  , D20Module D20Neg "inv" ["a"]
  ]

d20TestInput2 :: [D20Module]
d20TestInput2 =
  [ D20Module D20Neg "broadcaster" ["a"]
  , D20Module D20Flip "a" ["inv", "con"]
  , D20Module D20Neg "inv" ["b"]
  , D20Module D20Flip "b" ["con"]
  , D20Module D20Neg "con" ["output"]
  ]

-- >>> fmap (length . map (read :: String -> D20Module) . lines) $ readFile "d20.txt"
-- 58
--
-- >>> d20Process d20TestInput
-- (fromList [("a",["b"]),("b",["c"]),("broadcaster",["a","b","c"]),("c",["inv"]),("inv",["a"])],(fromList [("a",False),("b",False),("c",False)],fromList [("broadcaster",fromList []),("inv",fromList ["c"])]))
--
-- >>> d20Process d20TestInput2
-- (fromList [("a",["inv","con"]),("b",["con"]),("broadcaster",["a"]),("con",["output"]),("inv",["b"])],(fromList [("a",False),("b",False)],fromList [("broadcaster",fromList []),("con",fromList ["a","b"]),("inv",fromList ["a"])]))
--
-- >>> let (sys, st) = d20Process d20TestInput in snd $ d20Broadcast sys st
-- (fromList [("a",False),("b",False),("c",False)],fromList [("broadcaster",fromList []),("inv",fromList ["c"])])
--
-- >>> let (sys, st) = d20Process d20TestInput2 in mapM_ print $ take 5 $ iterate (snd . d20Broadcast sys) st
-- (fromList [("a",False),("b",False)],fromList [("broadcaster",fromList []),("con",fromList ["a","b"]),("inv",fromList ["a"])])
-- (fromList [("a",True),("b",True)],fromList [("broadcaster",fromList []),("con",fromList []),("inv",fromList [])])
-- (fromList [("a",False),("b",True)],fromList [("broadcaster",fromList []),("con",fromList ["a"]),("inv",fromList ["a"])])
-- (fromList [("a",True),("b",False)],fromList [("broadcaster",fromList []),("con",fromList ["b"]),("inv",fromList [])])
-- (fromList [("a",False),("b",False)],fromList [("broadcaster",fromList []),("con",fromList ["a","b"]),("inv",fromList ["a"])])
--
-- >>> let (t, f) = let (sys, st) = d20Process d20TestInput in snd $ execState (replicateM_ 1000 $ d20SignalsCount sys) (st, (0, 0)) in t * f
-- 32000000
--
-- >>> let (t, f) = let (sys, st) = d20Process d20TestInput2 in snd $ execState (replicateM_ 1000 $ d20SignalsCount sys) (st, (0, 0)) in t * f
-- 11687500
--
-- >>> fmap (\input -> let (t, f) = let (sys, st) = d20Process $ map read $ lines input in snd $ execState (replicateM_ 1000 $ d20SignalsCount sys) (st, (0, 0)) in t * f) $ readFile "d20.txt"
-- 832957356
--
{-
broadcast -> fv, cr, rt, tk

fv -> nt -> rk -> tp -> gx -> lv -> sx -> kj -> tt -> mt -> xg -> dv
fv, nt, rk, sx, tt, mt, xg, dv -> [zp]
[zp] -> fv, tp, gx, lv, kj
111000101111 -> 111101000111 -> 1+2+4+64+256+512+1024+2048 = 3+68+768+3072 = 71+3840 = 3911

cr -> vz -> hf -> tl -> br -> fk -> gk -> xt -> gh -> dj -> fj -> sl
cr, hf, tl, br, fk, gk, xt, gh, dj, fj, sl -> [jn]
[jn] -> cr, vz
101111111111 -> 111111111101 -> 4096 - 3 = 4093

rt -> fn -> rj -> qx -> bx -> tx -> gb -> jp -> dd -> bf -> vx -> sk
rt, rj, bx, jp, bf, vx, sk -> [mf]
[mf] -> rt, fn, qx, tx, gb, dd
101010010111 -> 111010010101 -> 1+4+16+128+512+1024+2048 = 21+640+3072 = 3733

tk -> mg -> ps -> xz -> cm -> bj -> nn -> nh -> ls -> cf -> nz -> dp
tk, mg, cm, bj, nh, ls, cf, nz, dp -> [ph]
[ph] -> tk, ps, xz, nn
110011011111 -> 111110110011 -> 1+2+16+32+128+256+512+1024+2048 = 19+160+768+3072 = 179+3840 = 4019

[zp] -> [kz]
[jn] -> [km]
[mf] -> [xj]
[ph] -> [qs]

[kz], [km], [xj], [qs] -> [gq]
[gq] -> rx

rx gets low when [gq] sends low
[gq] sends low after [kz], [km], [xj], and [qs] all send high
that happens after [zp], [jn], [mf], [ph] all send low
-}
d20GetFlips :: D20System -> D20State -> String -> [String]
d20GetFlips sys (_, negState) = S.toList . getFlips S.empty
  where
    getFlips existing next =
      if S.member next existing
        then existing
        else if M.member next negState
               then S.insert next existing
               else foldl getFlips (S.insert next existing) $ sys M.! next

d20Limit :: [String] -> D20State -> D20State
d20Limit only (flipState, negState) =
  let os = S.fromList only
   in (M.restrictKeys flipState os, M.restrictKeys negState os)

-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in mapM_ (\item -> print $ (item,) $ d14Tortoises (snd . d20Broadcast sys) $ d20Limit ("broadcaster" : d20GetFlips sys st item) st) $ sys M.! "broadcaster"
-- ("fv",(3911,3911))
-- ("cr",(4093,4093))
-- ("rt",(3733,3733))
-- ("tk",(4019,4019))
--
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in let lst = d20Limit ("broadcaster" : d20GetFlips sys st "cr") st in print $ d14Repeat 4093 (snd . d20Broadcast sys) lst == lst
-- True
--
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in let lst = d20Limit ("broadcaster" : d20GetFlips sys st "rt") st in print $ d14Repeat 3733 (snd . d20Broadcast sys) lst == lst
-- True
--
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in let lst = d20Limit ("broadcaster" : d20GetFlips sys st "tk") st in print $ d14Repeat 4019 (snd . d20Broadcast sys) lst == lst
-- True
--
-- >>> foldl lcm 1 [3911,4093,3733,4019]
-- 240162699605221
--
{- Apparently, that's the answer. I've entered it on a hunch, and it worked. -}
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in print $ filter (not . and . snd) $ zip [0..] $ map (map (\(D20Signal _ isHigh _) -> isHigh) . filter (\(D20Signal from _ _) -> from == "zp")) $ evalState (replicateM 3911 $ state $ d20Broadcast sys) $ d20Limit ("broadcaster" : d20GetFlips sys st "fv") st
-- [(3910,[False,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True])]
--
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in print $ filter (not . and . snd) $ zip [0..] $ map (map (\(D20Signal _ isHigh _) -> isHigh) . filter (\(D20Signal from _ _) -> from == "jn")) $ evalState (replicateM 4093 $ state $ d20Broadcast sys) $ d20Limit ("broadcaster" : d20GetFlips sys st "cr") st
-- [(4092,[False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True])]
--
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in print $ filter (not . and . snd) $ zip [0..] $ map (map (\(D20Signal _ isHigh _) -> isHigh) . filter (\(D20Signal from _ _) -> from == "mf")) $ evalState (replicateM 3733 $ state $ d20Broadcast sys) $ d20Limit ("broadcaster" : d20GetFlips sys st "rt") st
-- [(3732,[False,False,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True])]
--
-- >>> readFile "d20.txt" >>= \input -> let (sys, st) = d20Process $ map read $ lines input in print $ filter (not . and . snd) $ zip [0..] $ map (map (\(D20Signal _ isHigh _) -> isHigh) . filter (\(D20Signal from _ _) -> from == "ph")) $ evalState (replicateM 4019 $ state $ d20Broadcast sys) $ d20Limit ("broadcaster" : d20GetFlips sys st "tk") st
-- [(4018,[False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True])]
--
{--}
-- Day 19
data D19Cat
  = D19x
  | D19m
  | D19a
  | D19s
  deriving (Eq, Ord, Read, Show)

d19ReadCat :: Char -> D19Cat
d19ReadCat 'x' = D19x
d19ReadCat 'm' = D19m
d19ReadCat 'a' = D19a
d19ReadCat 's' = D19s

data D19Cond
  = D19gt
  | D19lt
  deriving (Eq, Ord, Read, Show)

data D19Wfl =
  D19Wfl String [String]
  deriving (Read, Show)

data D19Part =
  D19Part
    { d19x :: Int
    , d19m :: Int
    , d19a :: Int
    , d19s :: Int
    }
  deriving (Read, Show)

d19get :: D19Cat -> D19Part -> Int
d19get D19x = d19x
d19get D19m = d19m
d19get D19a = d19a
d19get D19s = d19s

data D19Rule =
  D19Rule
    { d19cat  :: D19Cat
    , d19cond :: D19Cond
    , d19lim  :: Int
    , d19dest :: String
    }

d19ParseRule :: String -> D19Rule
d19ParseRule (c:c':cs) =
  let (n, ':':s) = span (':' /=) cs
   in D19Rule
        { d19cat = d19ReadCat c
        , d19cond =
            if c' == '<'
              then D19lt
              else D19gt
        , d19lim = read n
        , d19dest = s
        }

d19Parse :: String -> ([D19Wfl], [D19Part])
d19Parse input =
  let (f, _:t) = span (not . null) $ lines input
   in (map read f, map read t)

d19Build :: [D19Wfl] -> M.Map String ([D19Rule], String)
d19Build = M.fromList . map (\(D19Wfl name rules) -> (name, (map d19ParseRule $ init rules, last rules)))

d19Eval :: D19Part -> D19Rule -> Maybe String
d19Eval part (D19Rule cat D19gt n s)
  | d19get cat part > n = Just s
d19Eval part (D19Rule cat D19lt n s)
  | d19get cat part < n = Just s
d19Eval _ _ = Nothing

d19Accepted :: M.Map String ([D19Rule], String) -> D19Part -> Bool
d19Accepted wfls part = accepted "in"
  where
    accepted name =
      let (rules, dflt) = wfls M.! name
       in case catMaybes $ map (d19Eval part) rules of
            "R":_ -> False
            "A":_ -> True
            name':_ -> accepted name'
            [] ->
              case dflt of
                "R"   -> False
                "A"   -> True
                name' -> accepted name'

d19AllAccepted :: ([D19Wfl], [D19Part]) -> [D19Part]
d19AllAccepted (wfls, parts) =
  let built = d19Build wfls
   in filter (d19Accepted built) parts

d19TestInput :: ([D19Wfl], [D19Part])
d19TestInput =
  ( [ D19Wfl "px" ["a<2006:qkq", "m>2090:A", "rfg"]
    , D19Wfl "pv" ["a>1716:R", "A"]
    , D19Wfl "lnx" ["m>1548:A", "A"]
    , D19Wfl "rfg" ["s<537:gd", "x>2440:R", "A"]
    , D19Wfl "qs" ["s>3448:A", "lnx"]
    , D19Wfl "qkq" ["x<1416:A", "crn"]
    , D19Wfl "crn" ["x>2662:A", "R"]
    , D19Wfl "in" ["s<1351:px", "qqz"]
    , D19Wfl "qqz" ["s>2770:qs", "m<1801:hdj", "R"]
    , D19Wfl "gd" ["a>3333:R", "R"]
    , D19Wfl "hdj" ["m>838:A", "pv"]
    ]
  , [ D19Part {d19x = 787, d19m = 2655, d19a = 1222, d19s = 2876}
    , D19Part {d19x = 1679, d19m = 44, d19a = 2067, d19s = 496}
    , D19Part {d19x = 2036, d19m = 264, d19a = 79, d19s = 2244}
    , D19Part {d19x = 2461, d19m = 1339, d19a = 466, d19s = 291}
    , D19Part {d19x = 2127, d19m = 1623, d19a = 2188, d19s = 1013}
    ])

d19FullValue :: D19Part -> Int
d19FullValue part = d19x part + d19m part + d19a part + d19s part

-- >>> sum $ map d19FullValue $ d19AllAccepted d19TestInput
-- 19114
--
-- >>> fmap (sum . map d19FullValue . d19AllAccepted . d19Parse) $ readFile "d19.txt"
-- 362930
--
d19CollectBounds :: M.Map String ([D19Rule], String) -> D19Cat -> S.Set Int
d19CollectBounds built cat = S.fromList $ M.toList built >>= \(_, (rules, _)) -> map d19lim $ filter ((cat ==) . d19cat) rules

d19TestBuilt :: M.Map String ([D19Rule], String)
d19TestBuilt = d19Build $ fst d19TestInput

data D19Interval =
  D19Interval
    { d19min :: Int
    , d19max :: Int
    }
  deriving (Read, Show)

data D19Cube =
  D19Cube
    { d19cx :: D19Interval
    , d19cm :: D19Interval
    , d19ca :: D19Interval
    , d19cs :: D19Interval
    }
  deriving (Read, Show)

d19cget :: D19Cat -> D19Cube -> D19Interval
d19cget D19x = d19cx
d19cget D19m = d19cm
d19cget D19a = d19ca
d19cget D19s = d19cs

d19cset :: D19Cat -> D19Interval -> D19Cube -> D19Cube
d19cset D19x interval cube = cube {d19cx = interval}
d19cset D19m interval cube = cube {d19cm = interval}
d19cset D19a interval cube = cube {d19ca = interval}
d19cset D19s interval cube = cube {d19cs = interval}

d19Pass :: D19Cube -> D19Rule -> Maybe D19Cube
d19Pass cube rule =
  case d19cond rule of
    D19gt ->
      let cat = d19cat rule
          interval = d19cget cat cube
          lim = d19lim rule
       in if d19max interval <= lim
            then Nothing
            else Just $ d19cset cat interval {d19min = max (d19min interval) (lim + 1)} cube
    D19lt ->
      let cat = d19cat rule
          interval = d19cget cat cube
          lim = d19lim rule
       in if d19min interval >= lim
            then Nothing
            else Just $ d19cset cat interval {d19max = min (d19max interval) (lim - 1)} cube

d19Fail :: D19Cube -> D19Rule -> Maybe D19Cube
d19Fail cube rule =
  case d19cond rule of
    D19gt ->
      let cat = d19cat rule
          interval = d19cget cat cube
          lim = d19lim rule
       in if d19min interval > lim
            then Nothing
            else Just $ d19cset cat interval {d19max = min (d19max interval) lim} cube
    D19lt ->
      let cat = d19cat rule
          interval = d19cget cat cube
          lim = d19lim rule
       in if d19max interval < lim
            then Nothing
            else Just $ d19cset cat interval {d19min = max (d19min interval) lim} cube

d19Length :: D19Interval -> Int
d19Length interval = d19max interval - d19min interval + 1

d19Volume :: D19Cube -> Int
d19Volume cube = d19Length (d19cx cube) * d19Length (d19cm cube) * d19Length (d19ca cube) * d19Length (d19cs cube)

d19Filter :: [(String, [D19Rule], String)] -> D19Cube -> Int
d19Filter wfls = filt "A"
  where
    filt :: String -> D19Cube -> Int
    filt "in" cube = d19Volume cube
    filt target cube =
      sum $
      wfls >>= \(dflt, rrev, name) ->
        map (filt name) $
        catMaybes $
        (guard (dflt == target) >> foldM d19Fail cube rrev) :
        map (\(r:rs) -> guard (d19dest r == target) >> d19Pass cube r >>= \c -> foldM d19Fail c rs) (init $ tails rrev)

d19RevRules :: M.Map String ([D19Rule], String) -> [(String, [D19Rule], String)]
d19RevRules input = map (\(name, (rules, dflt)) -> (dflt, reverse rules, name)) $ M.toList input

d19FullCube :: D19Cube
d19FullCube = D19Cube {d19cx = fullInt, d19cm = fullInt, d19ca = fullInt, d19cs = fullInt}
  where
    fullInt = D19Interval {d19min = 1, d19max = 4000}

-- >>> map length $ map S.toList $ map (d19CollectBounds d19TestBuilt) [D19x, D19m, D19a, D19s]
-- [3,4,3,4]
--
-- >>> fmap (\input -> map (length . S.toList . d19CollectBounds (d19Build $ fst $ d19Parse input)) [D19x, D19m, D19a, D19s]) $ readFile "d19.txt"
-- [269,280,258,256]
--
-- >>> d19Filter (d19RevRules d19TestBuilt) d19FullCube
-- 167409079868000
--
-- >>> fmap (\input -> d19Filter (d19RevRules $ d19Build $ fst $ d19Parse input) d19FullCube) $ readFile "d19.txt"
-- 116365820987729
--
{--}
-- Day 18
data D18Dir
  = D18U
  | D18R
  | D18D
  | D18L
  deriving (Eq, Ord, Read, Show)

d18Step :: (Int, Int) -> D18Dir -> Int -> (Int, Int)
d18Step (x, y) D18U n = (x, y - n)
d18Step (x, y) D18R n = (x + n, y)
d18Step (x, y) D18D n = (x, y + n)
d18Step (x, y) D18L n = (x - n, y)

data D18Command =
  D18Command D18Dir Int String
  deriving (Read, Show)

d18dir :: D18Command -> D18Dir
d18dir (D18Command d _ _) = d

d18cnt :: D18Command -> Int
d18cnt (D18Command _ n _) = n

d18TestInput :: [D18Command]
d18TestInput =
  [ D18Command D18R 6 "70c710"
  , D18Command D18D 5 "0dc571"
  , D18Command D18L 2 "5713f0"
  , D18Command D18D 2 "d2c081"
  , D18Command D18R 2 "59c680"
  , D18Command D18D 2 "411b91"
  , D18Command D18L 5 "8ceee2"
  , D18Command D18U 2 "caa173"
  , D18Command D18L 1 "1b58a2"
  , D18Command D18U 2 "caa171"
  , D18Command D18R 2 "7807d2"
  , D18Command D18U 3 "a77fa3"
  , D18Command D18L 2 "015232"
  , D18Command D18U 2 "7a21e3"
  ]

type D18Pic = S.Set (Int, Int)

d18cmd :: ((Int, Int), D18Pic) -> D18Command -> ((Int, Int), D18Pic)
d18cmd ((x, y), pic) (D18Command D18U n _) = ((x, y - n), foldl (\p z -> S.insert (x, z) p) pic [y - n .. y])
d18cmd ((x, y), pic) (D18Command D18R n _) = ((x + n, y), foldl (\p z -> S.insert (z, y) p) pic [x .. x + n])
d18cmd ((x, y), pic) (D18Command D18D n _) = ((x, y + n), foldl (\p z -> S.insert (x, z) p) pic [y .. y + n])
d18cmd ((x, y), pic) (D18Command D18L n _) = ((x - n, y), foldl (\p z -> S.insert (z, y) p) pic [x - n .. x])

d18Show :: (Int, Int) -> (Int, Int) -> D18Pic -> IO ()
d18Show (xul, yul) (xbr, ybr) pic =
  forM_ [yul .. ybr] $ \y ->
    putStrLn $
    map
      (\x ->
         if S.member (x, y) pic
           then '#'
           else '.')
      [xul .. xbr]

d18Calc :: (Int, Int) -> D18Command -> (Int, Int)
d18Calc (h, s) (D18Command D18U n _) = (h - n, s)
d18Calc (h, s) (D18Command D18R n _) = (h, s - n * h)
d18Calc (h, s) (D18Command D18D n _) = (h + n, s + n)
d18Calc (h, s) (D18Command D18L n _) = (h, s + n * (h + 1))

d18Parse :: String -> D18Command
d18Parse input =
  let dir =
        case last input of
          '0' -> D18R
          '1' -> D18D
          '2' -> D18L
          '3' -> D18U
      num =
        foldl
          (\acc c ->
             let cval = fromMaybe 0 $ elemIndex c "0123456789abcdef"
              in 16 * acc + cval)
          0 $
        init input
   in D18Command dir num ""

-- >>> fst $ foldl d18cmd ((0,0), S.empty) d18TestInput
-- (0,0)
--
-- >>> fmap (fst . foldl d18cmd ((0,0), S.empty) . map read . lines) $ readFile "d18.txt"
-- (0,0)
--
-- >>> d18Show (0,0) (6,9) $ snd $ foldl d18cmd ((0,0), S.empty) d18TestInput
-- #######
-- #.....#
-- ###...#
-- ..#...#
-- ..#...#
-- ###.###
-- #...#..
-- ##..###
-- .#....#
-- .######
--
{-
-- #>>> (>>= (d18Show (-54,-235) (388,223) . snd . foldl d18cmd ((0,0), S.empty) . map read . lines)) $ readFile "d18.txt"
-}
-- >>> foldl d18Calc (0, 1) d18TestInput
-- (0,62)
--
-- >>> fmap (foldl d18Calc (0, 1) . map read . lines) $ readFile "d18.txt"
-- (0,76387)
--
-- >>> take 5 $ map (\(D18Command _ _ s) -> d18Parse s) d18TestInput
-- [D18Command D18R 461937 "",D18Command D18D 56407 "",D18Command D18R 356671 "",D18Command D18D 863240 "",D18Command D18R 367720 ""]
--
-- >>> foldl d18Calc (0, 1) $ map (\(D18Command _ _ s) -> d18Parse s) d18TestInput
-- (0,952408144115)
--
-- >>> fmap (foldl d18Calc (0, 1) . map (\(D18Command _ _ s) -> d18Parse s) . map read . lines) $ readFile "d18.txt"
-- (0,250022188522074)
--
{--}
-- Day 17
d17Parse :: [String] -> M.Map (Int, Int) Int
d17Parse input =
  M.fromList $ do
    (y, ns) <- zip [0 ..] $ input
    (x, n) <- zip [0 ..] ns
    return ((x, y), read (n : ""))

type D17State = M.Map D17ChangeElem Int

d17Back :: (Int, Int) -> D16Dir -> (Int, Int)
d17Back (x, y) D16N = (x, y + 1)
d17Back (x, y) D16E = (x - 1, y)
d17Back (x, y) D16S = (x, y - 1)
d17Back (x, y) D16W = (x + 1, y)

d17Opp :: D16Dir -> D16Dir
d17Opp D16N = D16S
d17Opp D16E = D16W
d17Opp D16S = D16N
d17Opp D16W = D16E

d17AllDirs :: [D16Dir]
d17AllDirs = [D16N, D16E, D16S, D16W]

data D17ChangeElem =
  D17ChangeElem (Int, Int) D16Dir Int
  deriving (Eq, Ord)

d17Coords :: D17ChangeElem -> (Int, Int)
d17Coords (D17ChangeElem c _ _) = c

d17Dir :: D17ChangeElem -> D16Dir
d17Dir (D17ChangeElem _ d _) = d

d17Count :: D17ChangeElem -> Int
d17Count (D17ChangeElem _ _ c) = c

d17Step :: D16Dir -> (Int, Int) -> (Int, Int)
d17Step D16N (x, y) = (x, y - 1)
d17Step D16E (x, y) = (x + 1, y)
d17Step D16S (x, y) = (x, y + 1)
d17Step D16W (x, y) = (x - 1, y)

d17Next :: Int -> Int -> D17ChangeElem -> [D17ChangeElem]
d17Next minSteps maxSteps (D17ChangeElem cs d n) =
  if n < minSteps
    then [D17ChangeElem (d17Step d cs) d (n + 1)]
    else [ D17ChangeElem (d17Step d' cs) d' n'
         | d' <- d17AllDirs
         , d' /= d17Opp d
         , let n' =
                 if d' == d
                   then n + 1
                   else 1
         , n' <= maxSteps
         ]

d17AdvanceFrom :: Int -> Int -> M.Map (Int, Int) Int -> D17ChangeElem -> SS.State D17State [D17ChangeElem]
d17AdvanceFrom minSteps maxSteps input elem@(D17ChangeElem cs d n) = do
  current <- SS.gets $ M.lookup elem
  case current of
    Nothing -> return []
    Just curr ->
      fmap catMaybes $
      forM (d17Next minSteps maxSteps elem) $ \elem' ->
        case M.lookup (d17Coords elem') input of
          Nothing -> return Nothing
          Just add -> do
            let new = curr + add
            existing <- SS.gets $ M.lookup elem'
            if maybe False (new >=) existing
              then return Nothing
              else SS.modify (M.insert elem' new) >> return (Just elem')

d17Advance :: Int -> Int -> M.Map (Int, Int) Int -> ([D17ChangeElem], D17State) -> ([D17ChangeElem], D17State)
d17Advance minSteps maxSteps input (elems, state) = SS.runState (fmap concat $ mapM (d17AdvanceFrom minSteps maxSteps input) elems) state

d17Init :: Int -> D17State
d17Init maxSteps = M.fromList $ map (\d -> (D17ChangeElem (0, 0) d maxSteps, 0)) $ d17AllDirs

d17Best :: Int -> Int -> Int -> Int -> D17State -> Maybe Int
d17Best minSteps maxSteps x y state =
  case (do d <- d17AllDirs
           n <- [minSteps .. maxSteps]
           maybeToList (M.lookup (D17ChangeElem (x, y) d n) state)) of
    []  -> Nothing
    all -> Just $ minimum all

d17PrintBest :: Int -> Int -> (Int, Int) -> D17State -> IO ()
d17PrintBest minSteps maxSteps (maxX, maxY) state =
  forM_ [0 .. maxY] $ \y ->
    putStrLn $ concat $ intersperse "," $ map (\x -> maybe "*" show $ d17Best minSteps maxSteps x y state) [0 .. maxX]

d17TestInput :: [String]
d17TestInput =
  [ "2413432311323"
  , "3215453535623"
  , "3255245654254"
  , "3446585845452"
  , "4546657867536"
  , "1438598798454"
  , "4457876987766"
  , "3637877979653"
  , "4654967986887"
  , "4564679986453"
  , "1224686865563"
  , "2546548887735"
  , "4322674655533"
  ]

d17Parsed :: M.Map (Int, Int) Int
d17Parsed = d17Parse d17TestInput

-- >>> d17Best 1 3 12 12 $ snd $ head $ dropWhile (not . null . fst) $ iterate (d17Advance 1 3 d17Parsed) $ ([D17ChangeElem (0,0) d 3 | d <- d17AllDirs], d17Init 3)
-- Just 102
--
-- >>> :set +s
-- >>> fmap (\input -> d17Best 1 3 140 140 $ snd $ head $ dropWhile (not . null . fst) $ iterate (d17Advance 1 3 $ d17Parse $ lines input) ([D17ChangeElem (0, 0) d 3 | d <- d17AllDirs], d17Init 3)) $ readFile "d17.txt"
-- >>> :unset +s
-- (0.00 secs, 106,760 bytes)
-- Just 791
-- (119.54 secs, 39,717,980,952 bytes)
--
-- >>> d17Best 4 10 12 12 $ snd $ head $ dropWhile (not . null . fst) $ iterate (d17Advance 4 10 d17Parsed) $ ([D17ChangeElem (0,0) d 10 | d <- d17AllDirs], d17Init 10)
-- Just 94
--
d17TestInput2 :: [String]
d17TestInput2 = ["111111111111", "999999999991", "999999999991", "999999999991", "999999999991"]

-- >>> d17Best 4 10 11 4 $ snd $ head $ dropWhile (not . null . fst) $ iterate (d17Advance 4 10 $ d17Parse d17TestInput2) $ ([D17ChangeElem (0,0) d 10 | d <- d17AllDirs], d17Init 10)
-- Just 71
--
-- >>> :set +s
-- >>> fmap (\input -> d17Best 4 10 140 140 $ snd $ head $ dropWhile (not . null . fst) $ iterate (d17Advance 4 10 $ d17Parse $ lines input) ([D17ChangeElem (0, 0) d 10 | d <- d17AllDirs], d17Init 10)) $ readFile "d17.txt"
-- >>> :unset +s
-- (0.00 secs, 106,760 bytes)
-- Just 900
-- (183.70 secs, 56,106,132,568 bytes)
--
{--}
-- Day 16
data D16Dir
  = D16N
  | D16E
  | D16S
  | D16W
  deriving (Eq, Ord, Show)

data D16Beam =
  D16Beam Int Int D16Dir
  deriving (Eq, Ord)

d16x :: D16Beam -> Int
d16x (D16Beam x _ _) = x

d16y :: D16Beam -> Int
d16y (D16Beam _ y _) = y

d16d :: D16Beam -> D16Dir
d16d (D16Beam _ _ d) = d

d16Advance :: D16Beam -> D16Beam
d16Advance (D16Beam x y d) =
  case d of
    D16N -> D16Beam x (y - 1) d
    D16E -> D16Beam (x + 1) y d
    D16S -> D16Beam x (y + 1) d
    D16W -> D16Beam (x - 1) y d

d16Turn :: [String] -> D16Beam -> [D16Dir]
d16Turn input (D16Beam x y d) =
  if y < 0 || y >= length input
    then []
    else let line = input !! y
          in if x < 0 || x >= length line
               then []
               else case line !! x of
                      '.' -> [d]
                      '/' ->
                        case d of
                          D16N -> [D16E]
                          D16E -> [D16N]
                          D16S -> [D16W]
                          D16W -> [D16S]
                      '\\' ->
                        case d of
                          D16N -> [D16W]
                          D16E -> [D16S]
                          D16S -> [D16E]
                          D16W -> [D16N]
                      '|' ->
                        case d of
                          D16N -> [D16N]
                          D16E -> [D16N, D16S]
                          D16S -> [D16S]
                          D16W -> [D16N, D16S]
                      '-' ->
                        case d of
                          D16N -> [D16E, D16W]
                          D16E -> [D16E]
                          D16S -> [D16E, D16W]
                          D16W -> [D16W]

d16Suitable :: [String] -> D16Beam -> Bool
d16Suitable input (D16Beam x y _) = y >= 0 && x >= 0 && y < length input && x < length (input !! y)

d16Propagate :: [String] -> D16Beam -> [D16Beam]
d16Propagate input beam = [d16Advance (D16Beam (d16x beam) (d16y beam) d) | d <- d16Turn input beam]

d16Fill :: [String] -> D16Beam -> [(Int, Int)]
d16Fill input initial = S.toList $ S.map (\beam -> (d16x beam, d16y beam)) $ S.filter (d16Suitable input) $ fill S.empty [initial]
  where
    fill :: S.Set D16Beam -> [D16Beam] -> S.Set D16Beam
    fill set [] = set
    fill set (beam:beams) =
      if S.member beam set
        then fill set beams
        else fill (S.insert beam set) $ d16Propagate input beam ++ beams

d16TestInput :: [String]
d16TestInput =
  [ ".|...\\...."
  , "|.-.\\....."
  , ".....|-..."
  , "........|."
  , ".........."
  , ".........\\"
  , "..../.\\\\.."
  , ".-.-/..|.."
  , ".|....-|.\\"
  , "..//.|...."
  ]

d16Possibilities :: [String] -> [D16Beam]
d16Possibilities input =
  let x = length (head input) - 1
      y = length input - 1
   in [D16Beam n 0 D16S | n <- [0 .. x]] ++
      [D16Beam n y D16N | n <- [0 .. x]] ++ [D16Beam 0 n D16E | n <- [0 .. y]] ++ [D16Beam x n D16W | n <- [0 .. y]]

-- >>> length $ d16Fill d16TestInput $ D16Beam 0 0 D16E
-- 46
--
-- >>> fmap (length . flip d16Fill (D16Beam 0 0 D16E) . lines) $ readFile "d16.txt"
-- 6605
--
-- >>> length $ d16Fill d16TestInput $ D16Beam 3 0 D16S
-- 51
--
-- >>> maximum $ map (length . d16Fill d16TestInput) $ d16Possibilities d16TestInput
-- 51
--
-- >>> :set +s
-- >>> fmap (maximum . (\input -> map (length . d16Fill input) $ d16Possibilities input) . lines) $ readFile "d16.txt"
-- >>> :unset +s
-- (0.00 secs, 105,976 bytes)
-- 6766
-- (30.52 secs, 10,429,985,872 bytes)
--
{--}
-- Day 15
d15Hash :: String -> Int
d15Hash = foldl (\acc c -> mod (17 * (acc + ord c)) 256) 0

d15Split :: (a -> Bool) -> [a] -> [[a]]
d15Split p =
  reverse .
  map reverse .
  foldl
    (\css@(cs:cst) c ->
       if p c
         then [] : css
         else (c : cs) : cst)
    [[]]

d15TestInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

data D15Cmd
  = D15Remove String
  | D15Add String Int

d15Label :: D15Cmd -> String
d15Label (D15Remove l) = l
d15Label (D15Add l _)  = l

d15Parse :: String -> D15Cmd
d15Parse s =
  case span (\c -> c /= '=' && c /= '-') s of
    (l, '=':n) -> D15Add l (read n)
    (l, "-")   -> D15Remove l

d15Run :: D15Cmd -> State (M.Map Int [(String, Int)]) ()
d15Run cmd = do
  let box = d15Hash (d15Label cmd)
  existing <- gets $ fromMaybe [] . M.lookup box
  let resulting =
        case cmd of
          D15Remove s -> filter ((s /=) . fst) existing
          D15Add s n ->
            case span ((s /=) . fst) existing of
              (_, [])    -> (s, n) : existing
              (s1, _:s2) -> s1 ++ (s, n) : s2
  modify $ M.insert box resulting

d15FullRun :: String -> [(Int, [(String, Int)])]
d15FullRun program =
  map (\(n, lenses) -> (n, reverse lenses)) $ M.toList $ execState (mapM_ d15Run $ map d15Parse $ d15Split (',' ==) program) M.empty

d15FocusingPower :: [(Int, [(String, Int)])] -> Int
d15FocusingPower = sum . map (\(box, lenses) -> (1 + box) * sum (zipWith (*) [1 ..] $ map snd lenses))

-- >>> d15Hash "HASH"
-- 52
--
-- >>> sum $ map d15Hash $ d15Split (',' ==) d15TestInput
-- 1320
--
-- >>> fmap (sum . map d15Hash . d15Split (',' ==) . head . lines) $ readFile "d15.txt"
-- 506891
--
-- >>> d15Hash ""
-- 3
--
-- >>> d15FullRun d15TestInput
-- [(0,[("rn",1),("cm",2)]),(1,[]),(3,[("ot",7),("ab",5),("pc",6)])]
--
-- >>> d15FocusingPower $ d15FullRun d15TestInput
-- 145
--
-- >>> fmap (d15FocusingPower . d15FullRun . head . lines) $ readFile "d15.txt"
-- 230462
--
{--}
-- Day 14
d14TestInput :: [String]
d14TestInput =
  [ "O....#...."
  , "O.OO#....#"
  , ".....##..."
  , "OO.#O....O"
  , ".O.....O#."
  , "O.#..O.#.#"
  , "..O..#O..O"
  , ".......O.."
  , "#....###.."
  , "#OO..#...."
  ]

d14SlideRow :: String -> String
d14SlideRow = slide 0 0
  where
    slide :: Int -> Int -> String -> String
    slide r e ""      = replicate r 'O' ++ replicate e '.'
    slide r e ('#':t) = replicate r 'O' ++ replicate e '.' ++ '#' : slide 0 0 t
    slide r e ('.':t) = slide r (e + 1) t
    slide r e ('O':t) = slide (r + 1) e t

d14Value :: String -> Int
d14Value s =
  let l = length s
   in sum $
      zipWith
        (\n c ->
           case c of
             'O' -> n
             _   -> 0)
        [l,l - 1 .. 1]
        s

d14Spin :: [String] -> [String]
d14Spin input =
  let i1 = transpose $ map d14SlideRow $ transpose input
      i2 = map d14SlideRow i1
      i3 = transpose $ map reverse $ map d14SlideRow $ map reverse $ transpose i2
      i4 = map reverse $ map d14SlideRow $ map reverse i3
   in i4

d14Repeat :: Int -> (a -> a) -> (a -> a)
d14Repeat 0 _ = id
d14Repeat n f = f . d14Repeat (n - 1) f

d14Tortoises :: Eq a => (a -> a) -> a -> (Int, Int)
d14Tortoises f a = tort 1 (f a) (f $ f a)
  where
    tort n x y =
      if x /= y
        then tort (n + 1) (f x) (f $ f y)
        else (n, 1 + length (takeWhile (x /=) $ drop 1 $ iterate f x))

-- >>> sum $ map d14Value $ map d14SlideRow $ transpose d14TestInput
-- 136
--
-- >>> fmap (sum . map d14Value . map d14SlideRow . transpose . lines) $ readFile "d14.txt"
-- 108614
--
-- >>> d14Spin d14TestInput
-- [".....#....","....#...O#","...OO##...",".OO#......",".....OOO#.",".O#...O#.#","....O#....","......OOOO","#...O###..","#..OO#...."]
--
-- >>> d14Tortoises d14Spin d14TestInput
-- (7,7)
--
-- >>> fmap (d14Tortoises d14Spin . lines) $ readFile "d14.txt"
-- (156,78)
--
-- >>> mod (1000000000 - 7) 7
-- 6
--
-- >>> sum $ map d14Value $ transpose $ d14Repeat (7+6) d14Spin d14TestInput
-- 64
--
-- >>> mod (1000000000 - 156) 78
-- 64
--
-- >>> fmap (sum . map d14Value . transpose . d14Repeat (156 + 64) d14Spin . lines) $ readFile "d14.txt"
-- 96447
--
{--}
-- Day 13
d13TestInput1 :: [String]
d13TestInput1 = ["#.##..##.", "..#.##.#.", "##......#", "##......#", "..#.##.#.", "..##..##.", "#.#.##.#."]

d13TestInput2 :: [String]
d13TestInput2 = ["#...##..#", "#....#..#", "..##..###", "#####.##.", "#####.##.", "..##..###", "#....#..#"]

d13DetectH :: [String] -> Maybe Int
d13DetectH input =
  let l = length input - 1
      p i = all (\(m, n) -> input !! m == input !! n) $ zip [i - 1,i - 2 .. 0] [i .. l]
   in case filter p [1 .. l] of
        r:_ -> Just r
        []  -> Nothing

d13DetectV :: [String] -> Maybe Int
d13DetectV input =
  let l = length (head input) - 1
      p i = all (\(m, n) -> all (\line -> line !! m == line !! n) input) $ zip [i - 1,i - 2 .. 0] [i .. l]
   in case filter p [1 .. l] of
        r:_ -> Just r
        []  -> Nothing

d13DetectHSmudge :: [String] -> Maybe Int
d13DetectHSmudge input =
  let l = length input - 1
      p i = [1] == filter (> 0) (zipWith (\m n -> length $ filter id $ zipWith (/=) (input !! m) (input !! n)) [i - 1,i - 2 .. 0] [i .. l])
   in case filter p [1 .. l] of
        r:_ -> Just r
        []  -> Nothing

d13DetectVSmudge :: [String] -> Maybe Int
d13DetectVSmudge input =
  let l = length (head input) - 1
      p i =
        [1] ==
        filter (> 0) (zipWith (\m n -> length $ filter id $ zipWith (/=) (map (!! m) input) (map (!! n) input)) [i - 1,i - 2 .. 0] [i .. l])
   in case filter p [1 .. l] of
        r:_ -> Just r
        []  -> Nothing

d13Value :: [String] -> Int
d13Value input =
  let h =
        case d13DetectH input of
          Nothing -> 0
          Just r  -> r
      v =
        case d13DetectV input of
          Nothing -> 0
          Just r  -> r
   in 100 * h + v

d13ValueSmudge :: [String] -> Int
d13ValueSmudge input =
  let h =
        case d13DetectHSmudge input of
          Nothing -> 0
          Just r  -> r
      v =
        case d13DetectVSmudge input of
          Nothing -> 0
          Just r  -> r
   in 100 * h + v

-- >>> fmap length $ fmap (read :: String -> [[String]]) $ readFile "d13.txt"
-- 100
--
-- >>> d13Value d13TestInput1
-- 5
--
-- >>> d13Value d13TestInput2
-- 400
--
-- >>> fmap (sum . map d13Value . read) $ readFile "d13.txt"
-- 29213
--
-- >>> d13ValueSmudge d13TestInput1
-- 300
--
-- >>> d13ValueSmudge d13TestInput2
-- 100
--
-- >>> fmap (sum . map d13ValueSmudge . read) $ readFile "d13.txt"
-- 37453
--
{--}
-- Intermission
type DDEndo a = a -> a

ddCached ::
     Ord c
  => (a -> c)
  -> (forall m. (Monad m) =>
                  DDEndo (a -> m b))
  -> a
  -> b
ddCached h f x = evalState (dd x) M.empty
  where
    dd a = do
      let c = h a
      md <- gets (M.lookup c)
      case md of
        Just d -> return d
        Nothing -> do
          d <- f dd a
          modify (M.insert c d)
          return d

ddUncached :: ((a -> b) -> a -> b) -> a -> b
ddUncached = fix

d12CountWaysM :: Monad m => DDEndo ((Int, (Int, String), (Int, [Int])) -> m Int)
d12CountWaysM _ (0, (_, ""), (_, []))             = return 1
d12CountWaysM _ (0, (_, '#':_), (_, []))          = return 0
d12CountWaysM f (0, (ls, _:s), (_, []))           = f (0, (ls - 1, s), (0, []))
d12CountWaysM _ (0, (_, ""), _)                   = return 0
d12CountWaysM f (0, (ls, '.':s), rest)            = f (0, (ls - 1, s), rest)
d12CountWaysM f (0, (ls, '#':s), (lr, r:rs))      = f (r, (ls - 1, s), (lr - 1, rs))
d12CountWaysM f (0, (ls, '?':s), rest@(lr, r:rs)) = (+) <$> f (0, (ls - 1, s), rest) <*> f (r, (ls - 1, s), (lr - 1, rs))
d12CountWaysM _ (1, (_, '#':_), _)                = return 0
d12CountWaysM f (1, (ls, _:s), rest)              = f (0, (ls - 1, s), rest)
d12CountWaysM _ (1, (_, ""), (_, []))             = return 1
d12CountWaysM _ (_, (_, '.':_), _)                = return 0
d12CountWaysM f (n, (ls, _:s), rest)              = f (n - 1, (ls - 1, s), rest)
d12CountWaysM _ (_, (_, ""), _)                   = return 0

d12CountWaysM' :: (String, [Int]) -> Int
d12CountWaysM' (s, rs) = ddCached (\(n, (ls, _), (lr, _)) -> (n, ls, lr)) d12CountWaysM (0, (length s, s), (length rs, rs))

-- >>> map d12CountWaysM' d12TestInput
-- [1,4,1,1,4,10]
--
-- >>> map (d12CountWaysM' . d12Unfold) d12TestInput
-- [1,16384,1,16,2500,506250]
--
-- >>> fmap (sum . map (d12CountWaysM' . d12Unfold) . map read . lines) $ readFile "d12.txt"
-- 4964259839627
--
{--}
-- Day 12
type D12Cache = M.Map (Int, Int, Int) Int

d12CountWays :: Int -> String -> [Int] -> Int
d12CountWays 0 "" []             = 1
d12CountWays 0 ('#':_) []        = 0
d12CountWays 0 (_:s) []          = d12CountWays 0 s []
d12CountWays 0 "" (_:_)          = 0
d12CountWays 0 ('.':s) rest      = d12CountWays 0 s rest
d12CountWays 0 ('#':s) (r:rs)    = d12CountWays r s rs
d12CountWays 0 ('?':s) rs@(r:rt) = d12CountWays 0 s rs + d12CountWays r s rt
d12CountWays 1 ('#':s) _         = 0
d12CountWays 1 (_:s) rest        = d12CountWays 0 s rest
d12CountWays 1 "" []             = 1
d12CountWays n ('.':_) _         = 0
d12CountWays n (_:s) rest        = d12CountWays (n - 1) s rest
d12CountWays _ "" _              = 0

d12CountWays' :: Int -> (Int, String) -> (Int, [Int]) -> State D12Cache Int
d12CountWays' n (ls, s) (lr, rest) = do
  cached <- gets (M.lookup (n, ls, lr))
  case cached of
    Just c -> return c
    Nothing -> do
      c <-
        case n of
          0 ->
            case rest of
              [] ->
                case s of
                  ""      -> return 1
                  ('#':_) -> return 0
                  (_:s')  -> d12CountWays' 0 (ls - 1, s') (0, [])
              (r:rs) ->
                case s of
                  ""       -> return 0
                  ('.':s') -> d12CountWays' 0 (ls - 1, s') (lr, rest)
                  ('#':s') -> d12CountWays' r (ls - 1, s') (lr - 1, rs)
                  ('?':s') -> (+) <$> d12CountWays' 0 (ls - 1, s') (lr, rest) <*> d12CountWays' r (ls - 1, s') (lr - 1, rs)
          1 ->
            case s of
              "" ->
                return
                  (if null rest
                     then 1
                     else 0)
              ('#':_) -> return 0
              (_:s') -> d12CountWays' 0 (ls - 1, s') (lr, rest)
          _ ->
            case s of
              ""    -> return 0
              '.':_ -> return 0
              _:s'  -> d12CountWays' (n - 1) (ls - 1, s') (lr, rest)
      modify (M.insert (n, ls, lr) c)
      return c

d12CountWays'' :: Int -> String -> [Int] -> Int
d12CountWays'' n s rest = evalState (d12CountWays' n (length s, s) (length rest, rest)) M.empty

d12TestInput :: [(String, [Int])]
d12TestInput =
  [ ("???.###", [1, 1, 3])
  , (".??..??...?##.", [1, 1, 3])
  , ("?#?#?#?#?#?#?#?", [1, 3, 1, 6])
  , ("????.#...#...", [4, 1, 1])
  , ("????.######..#####.", [1, 6, 5])
  , ("?###????????", [3, 2, 1])
  ]

d12Sanity :: (String, [Int]) -> Bool
d12Sanity (s, rs) = d12CountWays 0 s rs == d12CountWays 0 (reverse s) (reverse rs)

d12Unfold :: (String, [Int]) -> (String, [Int])
d12Unfold (s, rs) = (drop 1 $ concat $ replicate 5 $ '?' : s, concat $ replicate 5 rs)

-- >>> map (\(s, rs) -> d12CountWays 0 s rs) d12TestInput
-- [1,4,1,1,4,10]
--
-- >>> map (\(s, rs) -> d12CountWays'' 0 s rs) d12TestInput
-- [1,4,1,1,4,10]
--
-- >>> fmap (sum . map (\(s, rs) -> d12CountWays 0 s rs) . map read . lines) $ readFile "d12.txt"
-- 7622
--
-- >>> fmap (sum . map (\(s, rs) -> d12CountWays'' 0 s rs) . map read . lines) $ readFile "d12.txt"
-- 7622
--
-- >>> fmap (filter (not . d12Sanity) . map read . lines) $ readFile "d12.txt"
-- []
--
-- >>> map (\(s, rs) -> let (s', rs') = d12Unfold (s, rs) in d12CountWays'' 0 s' rs') d12TestInput
-- [1,16384,1,16,2500,506250]
--
-- >>> fmap (sum . map (uncurry (d12CountWays'' 0) . d12Unfold) . map read . lines) $ readFile "d12.txt"
-- 4964259839627
--
{--}
-- Day 11
d11Distance :: (Int, Int) -> (Int, Int) -> Int
d11Distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

d11EmptyRows :: [(Int, Int)] -> Int -> [Int]
d11EmptyRows input numRows = filter (\n -> all (\(_, y) -> n /= y) input) [0 .. numRows - 1]

d11EmptyCols :: [(Int, Int)] -> Int -> [Int]
d11EmptyCols input numCols = filter (\n -> all (\(x, _) -> n /= x) input) [0 .. numCols - 1]

d11Expand :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
d11Expand input (x, y) =
  let rows = d11EmptyRows input y
      cols = d11EmptyCols input x
   in map (\(u, v) -> (u + length (filter (u >) cols), v + length (filter (v >) rows))) input

d11SuperExpand :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
d11SuperExpand input (x, y) =
  let rows = d11EmptyRows input y
      cols = d11EmptyCols input x
   in map (\(u, v) -> (u + 999999 * length (filter (u >) cols), v + 999999 * length (filter (v >) rows))) input

d11SumDist :: [(Int, Int)] -> Int
d11SumDist input = sum $ init (tails input) >>= \(p:ps) -> map (d11Distance p) ps

d11Parse :: [String] -> [(Int, Int)]
d11Parse input = zip [0 ..] input >>= (\(y, s) -> map ((, y) . fst) $ filter (('#' ==) . snd) $ zip [0 ..] s)

d11TestInput :: [String]
d11TestInput =
  [ "...#......"
  , ".......#.."
  , "#........."
  , ".........."
  , "......#..."
  , ".#........"
  , ".........#"
  , ".........."
  , ".......#.."
  , "#...#....."
  ]

d11Parsed :: [(Int, Int)]
d11Parsed = d11Parse d11TestInput

d11TestX :: Int
d11TestX = length $ head d11TestInput

d11TestY :: Int
d11TestY = length d11TestInput

-- >>> d11Parse d11TestInput
-- [(3,0),(7,1),(0,2),(6,4),(1,5),(9,6),(7,8),(0,9),(4,9)]
--
-- >>> d11EmptyRows d11Parsed d11TestY
-- [3,7]
--
-- >>> d11EmptyCols d11Parsed d11TestX
-- [2,5,8]
--
-- >>> d11Expand d11Parsed (d11TestX, d11TestY)
-- [(4,0),(9,1),(0,2),(8,5),(1,6),(12,7),(9,10),(0,11),(5,11)]
--
-- >>> d11SumDist $ d11Expand d11Parsed (d11TestX, d11TestY)
-- 374
--
-- >>> fmap (\s -> (\i -> let x = length $ head i in let y = length  i in d11SumDist $ d11Expand (d11Parse i) (x, y)) $ lines s) $ readFile "d11.txt"
-- 9599070
--
-- >>> fmap (\s -> (\i -> let x = length $ head i in let y = length  i in d11SumDist $ d11SuperExpand (d11Parse i) (x, y)) $ lines s) $ readFile "d11.txt"
-- 842645913794
--
{--}
-- Day 10
data D10Dir
  = D10N
  | D10E
  | D10S
  | D10W
  deriving (Eq, Ord, Show)

d10Reverse :: D10Dir -> D10Dir
d10Reverse D10N = D10S
d10Reverse D10E = D10W
d10Reverse D10S = D10N
d10Reverse D10W = D10E

d10ParseChar :: Char -> [D10Dir]
d10ParseChar '|' = [D10N, D10S]
d10ParseChar '-' = [D10E, D10W]
d10ParseChar 'L' = [D10N, D10E]
d10ParseChar 'J' = [D10N, D10W]
d10ParseChar '7' = [D10S, D10W]
d10ParseChar 'F' = [D10S, D10E]
d10ParseChar 'S' = [D10N, D10S]
d10ParseChar '.' = []

d10Move :: D10Dir -> State (Int, Int) ()
d10Move D10N = modify $ \(x, y) -> (x, y - 1)
d10Move D10E = modify $ \(x, y) -> (x + 1, y)
d10Move D10S = modify $ \(x, y) -> (x, y + 1)
d10Move D10W = modify $ \(x, y) -> (x - 1, y)

type D10Sketch = M.Map (Int, Int) [D10Dir]

d10Parse :: [String] -> D10Sketch
d10Parse input = M.fromList $ concat $ zipWith (\y s -> zipWith (\x c -> ((x, y), d10ParseChar c)) [0 ..] s) [0 ..] input

d10Step :: D10Sketch -> D10Dir -> State (Int, Int) D10Dir
d10Step sketch dir = do
  d10Move dir
  coords <- get
  let moves = sketch M.! coords
  return $ head $ filter (d10Reverse dir /=) moves

d10Loop :: D10Sketch -> (Int, Int) -> D10Dir -> [(Int, Int)]
d10Loop sketch to = lp to
  where
    lp from dir =
      let (nextDir, next) = runState (d10Step sketch dir) from
       in if next == to
            then [from]
            else from : lp next nextDir

d10Enclosed :: D10Sketch -> S.Set (Int, Int) -> (Int, Int) -> Bool
d10Enclosed sketch loop coords@(x, y) =
  not (S.member coords loop) && odd (length $ filter (\y' -> S.member (x, y') loop && elem D10E (sketch M.! (x, y'))) [0 .. y])

d10Contained :: D10Sketch -> S.Set (Int, Int) -> (Int, Int) -> Int
d10Contained sketch loop (maxX, maxY) = length $ filter (d10Enclosed sketch loop) [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]

d10TestInput :: [String]
d10TestInput = ["-L|F7", "7S-7|", "L|7||", "-L-J|", "L|-JF"]

d10TestParsed :: D10Sketch
d10TestParsed = d10Parse d10TestInput

d10TestInput2 :: [String]
d10TestInput2 =
  [ ".F----7F7F7F7F-7...."
  , ".|F--7||||||||FJ...."
  , ".||.FJ||||||||L7...."
  , "FJL7L7LJLJ||LJ.L-7.."
  , "L--J.L7...LJS7F-7L7."
  , "....F-J..F7FJ|L7L7L7"
  , "....L7.F7||L7|.L7L7|"
  , ".....|FJLJ|FJ|F7|.LJ"
  , "....FJL-7.||.||||..."
  , "....L---J.LJ.LJLJ..."
  ]

d10TestParsed2 :: D10Sketch
d10TestParsed2 = d10Parse d10TestInput2

-- >>> d10Loop d10TestParsed (1,1) D10E
-- [(1,1),(2,1),(3,1),(3,2),(3,3),(2,3),(1,3),(1,2)]
--
-- >>> length $ d10Loop d10TestParsed2 (12,4) D10E
-- 140
--
-- >>> fmap (\s -> length $ d10Loop (d10Parse $ lines s) (79,64) D10S) $ readFile "d10.txt"
-- 13640
--
-- >>> 13640/2
-- 6820.0
--
-- >>> fmap (\s -> let p = d10Parse $ lines s in d10Contained p (S.fromList $ d10Loop p (79,64) D10S) (139,139)) $ readFile "d10.txt"
-- 337
--
{--}
-- Day 9
d9Predict :: [Int] -> Int
d9Predict input =
  if all (0 ==) input
    then 0
    else last input + d9Predict (zipWith (-) (tail input) input)

d9Depredict :: [Int] -> Int
d9Depredict input =
  if all (0 ==) input
    then 0
    else head input - d9Depredict (zipWith (-) (tail input) input)

d9TestInput :: [[Int]]
d9TestInput = [[0, 3, 6, 9, 12, 15], [1, 3, 6, 10, 15, 21], [10, 13, 16, 21, 30, 45]]

-- >>> sum $ map d9Predict d9TestInput
-- 114
--
-- >>> map d9Depredict d9TestInput
-- [-3,0,5]
--
-- >>> fmap (sum . map d9Predict . map read . lines) $ readFile "d9.txt"
-- 2105961943
--
-- >>> fmap (sum . map d9Depredict . map read . lines) $ readFile "d9.txt"
-- 1019
--
{--}
-- Day 8
d8TestProgram :: String
d8TestProgram = "RL"

d8TestInput :: M.Map String (String, String)
d8TestInput =
  M.fromList
    [ ("AAA", ("BBB", "CCC"))
    , ("BBB", ("DDD", "EEE"))
    , ("CCC", ("ZZZ", "GGG"))
    , ("DDD", ("DDD", "DDD"))
    , ("EEE", ("EEE", "EEE"))
    , ("GGG", ("GGG", "GGG"))
    , ("ZZZ", ("ZZZ", "ZZZ"))
    ]

d8TestProgram2 :: String
d8TestProgram2 = "LLR"

d8TestInput2 :: M.Map String (String, String)
d8TestInput2 = M.fromList [("AAA", ("BBB", "BBB")), ("BBB", ("AAA", "ZZZ")), ("ZZZ", ("ZZZ", "ZZZ"))]

d8Program :: String
d8Program =
  "LLLLRLRLRRLRRRLRRLRLRRLRLLRRRLRRLRRRLRLLLRLRRLRLLRRRLRRLRLRRLLRRRLRRRLRLRRLRRRLRRLRRLLRRRLLLLRRLRRLRRLRRRLLLRLRLRLRRLRRRLRLRRRLRLRRRLRRLRRLLRRLLRLRRRLRLRRRLLLRLRRRLRLRRRLRRLRLRRLRRRLRRRLRRLLLRRRLRRLRRLRRLRRRLLLRRLRLRRRLLLLRRRLRRLRRRLLRLRLRRLLRRRLLRLRLRLRRLRRLRRRLRRLLRLRRLRRLLLLRRLRLRRLLRRLLRRLRRLRRRLLLRRRR"

d8Step :: M.Map String (String, String) -> State (Int, String, String) ()
d8Step input = do
  ~(n, p:ps, s) <- get
  let (l, r) = input M.! s
  let s' =
        if p == 'L'
          then l
          else r
  put (n + 1, ps, s')

d8Walk :: (String -> Bool) -> M.Map String (String, String) -> State (Int, String, String) (Int, String)
d8Walk predicate input = walk
  where
    walk = do
      d8Step input
      (n, _, s) <- get
      if predicate s
        then return (n, s)
        else walk

d8Count :: (String -> Bool) -> String -> String -> M.Map String (String, String) -> (Int, String)
d8Count predicate start program input = evalState (d8Walk predicate input) (0, cycle program, start)

d8Predicate :: String -> Bool
d8Predicate = ("ZZZ" ==)

d8Predicate2 :: String -> Bool
d8Predicate2 = isSuffixOf "Z"

d8Parse :: String -> M.Map String (String, String)
d8Parse = M.fromList . map read . lines

-- >>> d8Count d8Predicate "AAA" d8TestProgram d8TestInput
-- (2,"ZZZ")
--
-- >>> d8Count d8Predicate "AAA" d8TestProgram2 d8TestInput2
-- (6,"ZZZ")
--
-- >>> fmap (d8Count d8Predicate "AAA" d8Program . d8Parse) $ readFile "d8.txt"
-- (14429,"ZZZ")
--
-- >>> fmap ((\input -> map (\z -> (z, d8Count d8Predicate2 z d8Program input)) $ filter (isSuffixOf "Z") $ M.keys input) . d8Parse) $ readFile "d8.txt"
-- [("CFZ",(22411,"CFZ")),("CXZ",(18113,"CXZ")),("HPZ",(18727,"HPZ")),("LNZ",(20569,"LNZ")),("SGZ",(13201,"SGZ")),("ZZZ",(14429,"ZZZ"))]
--
-- >>> fmap ((\input -> map (\z -> (z, d8Count d8Predicate2 z d8Program input)) $ filter (isSuffixOf "A") $ M.keys input) . d8Parse) $ readFile "d8.txt"
-- [("AAA",(14429,"ZZZ")),("DNA",(20569,"LNZ")),("HNA",(18727,"HPZ")),("LLA",(22411,"CFZ")),("LMA",(13201,"SGZ")),("VGA",(18113,"CXZ"))]
--
-- >>> length d8Program
-- 307
--
{-
Note: walking from any starting point to some ending point takes exactly the same time as walking from that ending point to the next one, which is always the same one.
Also, it requires running the program fully, multiple times.

22411 = 307 * 73
18113 = 307 * 59
18727 = 307 * 61
20569 = 307 * 67
13201 = 307 * 43
14429 = 307 * 47
-}
-- >>> 307 * 73 * 59 * 61 * 67 * 43 * 47
-- 10921547990923
--
{--}
-- Day 7
data D7Card
  = D70
  | D72
  | D73
  | D74
  | D75
  | D76
  | D77
  | D78
  | D79
  | D7T
  | D7J
  | D7Q
  | D7K
  | D7A
  deriving (Bounded, Enum, Eq, Ord, Show)

d7FromChar :: Char -> D7Card
d7FromChar c = [minBound .. maxBound] !! fromMaybe undefined (elemIndex c "023456789TJQKA")

data D7Hand =
  D7Hand [D7Card] Int

d7cards :: D7Hand -> [D7Card]
d7cards (D7Hand cards _) = cards

d7bid :: D7Hand -> Int
d7bid (D7Hand _ bid) = bid

data D7Class
  = D7None
  | D7Pair
  | D7TwoPair
  | D7Three
  | D7FullHouse
  | D7Four
  | D7Five
  deriving (Eq, Ord, Show)

d7Classify :: [D7Card] -> D7Class
d7Classify hand =
  case sort $ map length $ group $ sort $ filter (D70 /=) hand of
    []           -> D7Five
    [_]          -> D7Five
    [1, _]       -> D7Four
    [2, _]       -> D7FullHouse
    [1, 1, _]    -> D7Three
    [1, 2, 2]    -> D7TwoPair
    [1, 1, 1, _] -> D7Pair
    _            -> D7None

d7RankedValue :: [D7Hand] -> Int
d7RankedValue hands =
  sum $
  zipWith (*) [1 ..] $
  map d7bid $
  sortBy
    (on
       compare
       (\h ->
          let cs = d7cards h
           in (d7Classify cs, cs)))
    hands

d7Read :: String -> D7Hand
d7Read line =
  let [cs, bd] = words line
   in D7Hand (map d7FromChar cs) (read bd)

d7Turn :: D7Hand -> D7Hand
d7Turn (D7Hand cards bid) =
  D7Hand
    (map
       (\c ->
          case c of
            D7J -> D70
            _   -> c)
       cards)
    bid

d7TestInput :: [String]
d7TestInput = ["32T3K 765", "T55J5 684", "KK677 28", "KTJJT 220", "QQQJA 483"]

-- >>> d7Classify $ map d7FromChar "QQQJA"
-- D7Three
--
-- >>> d7RankedValue $ map d7Read d7TestInput
-- 6440
--
-- >>> fmap (d7RankedValue . map d7Read . lines) $ readFile "d7.txt"
-- 246424613
--
-- >>> d7RankedValue $ map d7Turn $ map d7Read d7TestInput
-- 5905
--
-- >>> fmap (d7RankedValue . map d7Turn . map d7Read . lines) $ readFile "d7.txt"
-- 248256639
--
{--}
-- Day 6
-- Are you kidding? I solved it in 'bc'.
{-
  >>> sqrt(7^2/4-9) + 1/2
  2.30277563773199464655
  >>> sqrt(15^2/4-40) + 1/2
  4.53112887414927482618
  >>> sqrt(30^2/4-200)
  5.00000000000000000000
  >>> sqrt(34^2/4-204)
  9.21954445729288731000
  >>> sqrt(90^2/4-1713)
  17.66352173265569370952
  >>> sqrt(89^2/4-1210)+1/2
  28.25337817275583480604
  >>> sqrt(86^2/4-1780)
  8.30662386291807485258
  >>> 19*35*56*17
  633080
  >>> sqrt(71530^2/4-940200)
  35751.85344845774565706636
  >>> 35751*2+1
  71503
  >>> sqrt(34908986^2/4-204171312101780)
  10024370.99200089461653076851
  >>> 10024370*2+1
  20048741
-}
{--}
-- Day 5
d5IntMap :: Int -> [(Int, Int, Int)] -> Int
d5IntMap n [] = n
d5IntMap n ((d, s, l):rest)
  | n >= s && n < s + l = n + d - s
  | otherwise = d5IntMap n rest

d5SeedNums :: [Int] -> [(Int, Int)]
d5SeedNums (a:b:rest) = (a, b) : d5SeedNums rest
d5SeedNums _          = []

d5InverseIntMap :: Int -> [(Int, Int, Int)] -> [Int]
d5InverseIntMap n ms =
  catMaybes $
  (if any (\(d, s, l) -> n >= s && n < s + l) ms
     then Nothing
     else Just n) :
  (map
     (\(d, s, l) ->
        if n >= d && n < d + l
          then Just (n + s - d)
          else Nothing)
     ms)

d5Candidates :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [Int]
d5Candidates ns ms =
  map fst ns ++
  filter
    (\n -> any (\(s, l) -> n >= s && n < s + l) ns)
    (foldr (\m acc -> 0 : (m >>= \(_, s, l) -> [s, s + l]) ++ (acc >>= flip d5InverseIntMap m)) [] ms)

d5TestSeeds :: [Int]
d5TestSeeds = [79, 14, 55, 13]

d5TestMaps :: [[(Int, Int, Int)]]
d5TestMaps =
  [ [(50, 98, 2), (52, 50, 48)]
  , [(0, 15, 37), (37, 52, 2), (39, 0, 15)]
  , [(49, 53, 8), (0, 11, 42), (42, 0, 7), (57, 7, 4)]
  , [(88, 18, 7), (18, 25, 70)]
  , [(45, 77, 23), (81, 45, 19), (68, 64, 13)]
  , [(0, 69, 1), (1, 0, 69)]
  , [(60, 56, 37), (56, 93, 4)]
  ]

d5Seeds :: [Int]
d5Seeds =
  [ 2041142901
  , 113138307
  , 302673608
  , 467797997
  , 1787644422
  , 208119536
  , 143576771
  , 99841043
  , 4088720102
  , 111819874
  , 946418697
  , 13450451
  , 3459931852
  , 262303791
  , 2913410855
  , 533641609
  , 2178733435
  , 26814354
  , 1058342395
  , 175406592
  ]

d5Seeds2 :: [(Int, Int)]
d5Seeds2 = d5SeedNums d5Seeds

-- >>> fmap ((\ms -> minimum $ map (\n -> foldl d5IntMap n ms) $ d5Candidates d5Seeds2 ms) . read) $ readFile "d5.txt"
-- 24261545
--
-- >>> minimum $ map (\n -> foldl d5IntMap n d5TestMaps) $ d5Candidates (d5SeedNums d5TestSeeds) d5TestMaps
-- 46
--
-- >>> d5Candidates (d5SeedNums d5TestSeeds) d5TestMaps
-- [79,55,59,82,82,92,62,62,66]
--
-- >>> map (\n -> foldl d5IntMap n d5TestMaps) d5TestSeeds
-- [82,43,86,35]
--
-- >>> fmap (minimum . (\ms -> map (\n -> foldl d5IntMap n ms) d5Seeds) . read) $ readFile "d5.txt"
-- 261668924
--
{--}
-- Day 4
data D4Card =
  D4Card [Int] [Int]
  deriving (Read, Show)

d4winning :: D4Card -> [Int]
d4winning (D4Card is _) = is

d4owned :: D4Card -> [Int]
d4owned (D4Card _ is) = is

d4TestInput :: [D4Card] =
  [ D4Card [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53]
  , D4Card [13, 32, 20, 16, 61] [61, 30, 68, 82, 17, 32, 24, 19]
  , D4Card [1, 21, 53, 59, 44] [69, 82, 63, 72, 16, 21, 14, 1]
  , D4Card [41, 92, 73, 84, 69] [59, 84, 76, 51, 58, 5, 54, 83]
  , D4Card [87, 83, 26, 28, 32] [88, 30, 70, 12, 93, 22, 82, 36]
  , D4Card [31, 18, 13, 56, 72] [74, 77, 10, 23, 35, 67, 36, 11]
  ]

d4Points :: D4Card -> Int
d4Points card = length (filter (\n -> elem n (d4winning card)) (d4owned card))

d4Worth :: D4Card -> Int
d4Worth card = (0 : iterate (2 *) 1) !! d4Points card

d4OneCard :: State [(Int, D4Card)] Int
d4OneCard = do
  ~((n, card):rest) <- get
  put $ zipWith (\a (m, c) -> (a + m, c)) (replicate (d4Points card) n ++ repeat 0) rest
  return n

-- >>> sum $ map d4Worth d4TestInput
-- 13
--
-- >>> fmap (sum . map (d4Worth . read) . lines) $ readFile "d4.txt"
-- 21138
--
-- >>> sum $ evalState (replicateM (length d4TestInput) d4OneCard) $ zip (repeat 1) d4TestInput
-- 30
--
-- >>> fmap (sum . evalState(replicateM 198 d4OneCard) . zip (repeat 1) . map read . lines) $ readFile "d4.txt"
-- 7185540
--
{--}
-- Day 3
d3LineNumbers :: (Int -> Bool) -> String -> [Int]
d3LineNumbers =
  let ln :: Int -> (Int -> Bool) -> String -> [Int]
      ln _ _ [] = []
      ln n p cs@(c:ct)
        | not (isDigit c) = ln (n + 1) p ct
        | otherwise =
          let (ds, cs') = span isDigit cs
              i = read ds
              l = length ds
           in if any p [n .. (n + l - 1)]
                then i : ln (n + l) p cs'
                else ln (n + l) p cs'
   in ln 0

d3SymPos :: String -> [Int]
d3SymPos = findIndices (\c -> not (isDigit c || c == '.'))

d3Close :: [Int] -> Int -> Bool
d3Close ns n =
  case dropWhile (\k -> k < n - 1) ns of
    []    -> False
    (k:_) -> k <= n + 1

d3AllNumbers :: [String] -> [[Int]]
d3AllNumbers input =
  zipWith3
    (\pr cr nx ->
       let prns = d3SymPos pr
           crns = d3SymPos cr
           nxns = d3SymPos nx
        in d3LineNumbers (\n -> d3Close prns n || d3Close crns n || d3Close nxns n) cr)
    ("" : input)
    input
    (tail input ++ [""])

d3Solution :: [String] -> Int
d3Solution = sum . map sum . d3AllNumbers

d3TestInput =
  [ "467..114.."
  , "...*......"
  , "..35..633."
  , "......#..."
  , "617*......"
  , ".....+.58."
  , "..592....."
  , "......755."
  , "...$.*...."
  , ".664.598.."
  ]

d3Adjacent :: [(Int, String)] -> (Int, Int) -> [Int]
d3Adjacent input (x, y) = concat $ map (\(_, s) -> d3LineNumbers (\j -> abs (j - y) <= 1) s) $ filter (\(i, _) -> abs (i - x) <= 1) input

d3AllGears :: [String] -> [(Int, Int)]
d3AllGears input = concat $ zipWith (\n -> map (n, )) [0 ..] $ map (findIndices ('*' ==)) input

d3Solution2 :: [String] -> Int
d3Solution2 input =
  let indicedInput = zip [0 ..] input
   in sum $
      map
        (\xy ->
           case d3Adjacent indicedInput xy of
             [n1, n2] -> n1 * n2
             _        -> 0) $
      d3AllGears input

-- >>> fmap (d3Solution2 . lines) $ readFile "d3.txt"
-- 81296995
--
-- >>> fmap (d3Solution . lines) $ readFile "d3.txt"
-- 517021
--
{--}
-- Day 2
d2FitMax :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
d2FitMax (mr, mg, mb) = all (\(r, g, b) -> r <= mr && g <= mg && b <= mb)

d2SumIndices :: (Int, Int, Int) -> [[(Int, Int, Int)]] -> Int
d2SumIndices maxes input = sum $ map fst $ filter (\(_, triples) -> d2FitMax maxes triples) $ zip [1 ..] input

d2TestInput :: [[(Int, Int, Int)]]
d2TestInput =
  [ [(4, 0, 3), (1, 2, 6), (0, 2, 0)]
  , [(0, 2, 1), (1, 3, 4), (0, 1, 1)]
  , [(20, 8, 6), (4, 13, 5), (1, 5, 0)]
  , [(3, 1, 6), (6, 3, 0), (14, 3, 15)]
  , [(6, 3, 1), (1, 2, 2)]
  ]

d2Maxes :: (Int, Int, Int)
d2Maxes = (12, 13, 14)

d2Minimize :: [(Int, Int, Int)] -> (Int, Int, Int)
d2Minimize = foldl (\(ar, ag, ab) (ir, ig, ib) -> (max ar ir, max ag ig, max ab ib)) (0, 0, 0)

d2Power :: (Int, Int, Int) -> Int
d2Power (r, g, b) = r * g * b

d2TotalPower :: [[(Int, Int, Int)]] -> Int
d2TotalPower = sum . map (d2Power . d2Minimize)

-- >>> d2SumIndices d2Maxes d2TestInput
-- 8
--
-- >>> fmap (d2SumIndices d2Maxes . map read . lines) $ readFile "d2.txt"
-- 2006
--
-- >>> d2TotalPower d2TestInput
-- 2286
--
-- >>> fmap (d2TotalPower . map read . lines) $ readFile "d2.txt"
-- 84911
--
{--}
-- Day 1
d1CalibValue :: String -> Int
d1CalibValue input =
  let onlyDigits = filter isDigit input
   in read $ head onlyDigits : last onlyDigits : []

d1FullCalibValue :: [String] -> Int
d1FullCalibValue input = sum $ map d1CalibValue $ filter (not . null) input

d1ReplaceSubstring :: String -> String -> String -> String
d1ReplaceSubstring what with =
  let len = length what
      d1ReplaceSubstring' [] = []
      d1ReplaceSubstring' within@(w:ws) =
        if isPrefixOf what within
          then with ++ d1ReplaceSubstring' (drop len within)
          else w : d1ReplaceSubstring' ws
   in d1ReplaceSubstring'

d1ReplaceSubstrings :: [(String, String)] -> String -> String
d1ReplaceSubstrings _ [] = []
d1ReplaceSubstrings replacements input@(i:is) =
  case find (\(what, _) -> isPrefixOf what input) replacements of
    Nothing           -> i : d1ReplaceSubstrings replacements is
    Just (what, with) -> with ++ d1ReplaceSubstrings replacements (drop (length what) input)

d1TestInput :: [String]
d1TestInput = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

d1TestInput2 :: [String]
d1TestInput2 = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"]

d1Replacements :: [(String, String)]
d1Replacements =
  [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

d1FindFirstDigit :: String -> String
d1FindFirstDigit input@(i:is) =
  if isDigit i
    then [i]
    else case find (\(what, _) -> isPrefixOf what input) d1Replacements of
           Nothing        -> d1FindFirstDigit is
           Just (_, with) -> with

d1FindLastDigit :: String -> String
d1FindLastDigit = d1FindLastDigit' . reverse
  where
    d1FindLastDigit' input@(i:is) =
      if isDigit i
        then [i]
        else case find (\(what, _) -> isPrefixOf (reverse what) input) d1Replacements of
               Nothing        -> d1FindLastDigit' is
               Just (_, with) -> with

d1CalibValue2 :: String -> Int
d1CalibValue2 input = read $ d1FindFirstDigit input ++ d1FindLastDigit input

d1FullCalibValue2 :: [String] -> Int
d1FullCalibValue2 input = sum $ map d1CalibValue2 $ filter (not . null) input
-- >>> d1FullCalibValue d1TestInput
-- 142
--
-- >>> fmap (d1FullCalibValue . lines) $ readFile "d1.txt"
-- 54605
--
-- >>> d1FullCalibValue2 d1TestInput2
-- 281
--
-- >>> fmap (d1FullCalibValue2 . lines) $ readFile "d1.txt"
-- 55429
--
