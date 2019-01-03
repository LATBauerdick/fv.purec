module Test.Main where

import Prelude
{--   (Unit, bind, discard, map, pure, show, unit --}
{--   , ($), (*), (<<<), (<>), (=<<), (/=), (-) ) --}
import Prelude.Extended ( iflt, to1fix, to5fix, words, uJust, debug, fromIntegral, normals, boxMuller )

import Effect
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console ( log, logShow )
{-- import Node.FS.Sync ( readTextFile ) --}
{-- import Node.Encoding ( Encoding(..) ) --}
import Control.Monad.ST ( ST )
import Control.Monad.ST ( run, for ) as ST
import Data.Array.ST ( STArray )
import Data.Array.ST ( peek, poke, unsafeFreeze, thaw ) as STA
import Data.Int ( toNumber, fromString, floor )
import Math ( abs, sqrt, floor ) as M
import Global (infinity)
import Data.Enum (fromEnum, toEnum)

import Data.Monoid ( mempty )
import Data.Tuple ( Tuple(..) )
import Data.Array ( length, zip, foldl, fromFoldable, replicate, zipWith, uncons, index, drop, range, concat, take )
import Data.Unfoldable ( replicateA )
import Data.List.Lazy ( replicateM )
import Data.List ( List(..), range ) as L
import Data.Maybe  ( Maybe(..))
import Data.Foldable (sum, traverse_)

import Data.Cov (testCov2, Cov(..), Jac(..), Dim3, Vec3, Jac33, Cov3, fromArray, (*.), inv)
import Data.String ( replace, contains, Pattern(..), Replacement(..) ) as S
import Data.String.CodeUnits ( singleton, drop, dropWhile, fromCharArray, toCharArray, take, takeWhile ) as CU
import Data.Char ( toCharCode )
import Data.Number ( fromString ) as DN
import Text.Format ( format, precision, width )

import FV.Types
  ( VHMeas, HMeas, QMeas
  , XMeas, Prong (Prong), Chi2 (Chi2)
  , vertex, helices, hFilter, fromHMeas, fromQMeas
  , vBlowup, distance, invMass
  )
import FV.Fit ( fit )

import Test.Input ( hSlurp, hSlurpMCtruth )
import Test.Random ( testRandom )
{-- import Test.Cluster ( doCluster ) --}



{-- readData :: String -> Effect String --}
{-- readData = readTextFile UTF8 --}

main :: Effect Unit
main = do
  log $ show $ CU.toCharArray $ "Ʈest" <> "ℌ"
  log $ show $ CU.fromCharArray $ CU.toCharArray "test test"
  log $ show $ DN.fromString "1234.5"
  log $ show $ DN.fromString "infinite"
  log $ show $ CU.singleton '>' <> CU.singleton '♜' <> CU.singleton '<'
  log $ show $ S.replace (S.Pattern "<=") (S.Replacement "≤") "a <= b <= c"
  log $ show $ S.replace (S.Pattern "≤") (S.Replacement "|   <=    |") "a♜ ≤ b <= c"
  log $ show $ CU.drop 0 "a♜ ≤ b <= c"
  log $ show $ CU.drop 1 "a♜ ≤ b <= c"
  log $ show $ CU.drop 2 "a♜ ≤ b <= c"
  log $ show $ CU.drop 3 "a♜ ≤ b <= c"
  log $ show $ CU.dropWhile (_ /= 'c') "a♜ <= b ≤ c"
  log $ show $ CU.dropWhile (_ /= 'b') "a♜ <= b ≤ c"
  log $ show $ CU.dropWhile (_ /= 'a') "a♜ <= b ≤ c"
  log $ show $ CU.take 1 "a♜ ≤ b <= c"
  log $ show $ CU.take 100 "a♜ ≤ b <= c"
  log $ show $ CU.take (-100) "a♜ ≤ b <= c"
  log $ show $ CU.takeWhile (_ /= 'b') "a♜ <= b ≤ c"
  log $ show $ CU.takeWhile (_ /= 'z') "a♜ <= b ≤ c"
  let ds :: String
      ds = """
   text 1234
   3.355679512023926       3.489715576171875       7.110095977783203    
  4.5451703E-03
"""
  logShow $ words ds
  let cc :: Int
      cc = toCharCode 'x'
  logShow $ cc
  let cc :: Int
      cc =  fromEnum 'x'
  log $ "fromEnum Char does NOT work: " <> show cc
  logShow $ fromString "12.3456"
  logShow $ fromString "12"
  {-- log $ show $ format (width 8 <> precision 3) 12.34567 --}
  {-- log $ show $ format (width 8 <> precision 3) (-0.815) --}
  log $ show (-0.815)
  log $ show (-0.072)
  log $ show $ format (width 8 <> precision 3) (-0.815999999999999999999999999)
  log $ show 12.3456789123456789
  log $ show 12.3456789e-3
  log $ show $ to5fix 12.3456789e-3
  log $ show 12.3456789e-6
  log $ show $ to5fix 12.3456789e-6
  log $ show 12.3456789e-9
  log $ show $ to5fix 12.3456789e-9
  log $ show 12.3456789e3
  log $ show 12.3456789e6
  log $ show 12.3456789e9
  log $ show 12.3456789e12
  log $ show 12.3456789e15
  log $ show 12.3456789e18
  log $ show 12.3456789e21
  log $ show 3e-03
  log $ show 3e-04
  log $ show 3e-05
  log $ show 31e-07
  log $ show 3e-09
  log $ show $ S.contains (S.Pattern "e") (show 3e-5)
  log $ show $ fromFoldable (Just 1)
  log $ show $ fromFoldable [1,2,3,4,5]
  log $ show $ fromFoldable [Just 1, Just 2, Just 3, Nothing, Just 5]
  let lll :: L.List Int
      lll = L.range 1 5
  log $ show $ lll
  log $ show $ fromFoldable lll
  log $ show $ fromFoldable (Just lll)
  log $ show $ replicate 2 "Hi"
  log $ show $ zipWith (*) [1, 2, 3] [4, 5, 6, 7]
  let
      xc3 :: Cov Dim3
      xc3 = Cov {v: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]}
      v3 :: Vec3
      v3 = fromArray [10.0,11.0,12.0]
  log $ show xc3 `debug` show v3
  log $ "Vec *. Vec = " <> show (v3 *. v3)
  let c3a = (fromArray[1.0,2.0,3.0,4.0,5.0,6.0])::Cov3
      c3b = (fromArray [0.0,0.0,1.0,1.0,0.0,0.0])::Cov3
  log $ "Cov *. Cov = " <> show (c3a *. c3b)
  let j3 :: Jac33
      j3 = c3a *. c3b
      c3c = inv (one::Cov3)
  log $ "Jac = Cov * Cov = " <> show j3
  log $ "inv Cov = " <> show c3c
  let j33 = Jac {v: [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0], nr: 3} :: Jac33
  log $ "Jac = Cov * Cov = " <> show j33
  let j = j3 *. c3c
  log $ "Jac * Cov = " <> show j
  logShow $ (fromArray [1.0,2.0,3.0,4.0,5.0,6.0])::Cov3 -- cov matrix --}
  let arr :: Array Int
      arr = do
          let n=5
          i0 <- range 0 (n-1)
          j0 <- range i0 (n-1)
          pure $ 10*i0+j0
  logShow $ arr
  let lll :: L.List Int
      lll = do
          let n=5
          i0 <- L.range 0 (n-1)
          j0 <- L.range i0 (n-1)
          pure $ 10*i0+j0
  logShow $ lll
  logShow $ floor <<< M.sqrt <<< fromIntegral $ 9
  logShow $ floor 9.9
  let x = 9.0
  logShow $ x
  logShow $ infinity
  logShow $ x == infinity
  logShow $ M.floor 9.9
  logShow $ M.sqrt 9.0
  {-- ST.run (ST.for 0 5 (\i0 -> do (log $ "for " <> show i0))) --}
  let
      run :: forall a. (forall r. ST r (STArray r a)) -> Array a
      run act = ST.run (act >>= STA.unsafeFreeze)
  let l= run (do
        arr <- STA.thaw (replicate (5) 99.0)
        ST.for 0 5 \i -> do
          _ <- STA.poke (i) (toNumber i) arr
          mai <- STA.peek i arr
          let works = mai == (Just (toNumber i)) `debug` ("xxxxxxxxxx" <> show mai)
          pure unit
        pure arr)
  log $ show l
  let sarr :: Array String
      sarr =   ["t1","t2","t3","t4"]
  {-- log $ joinxxx ", " sarr --}
  let rec = { field1: "field 1", field2: sarr }
  log $ "record " <> show rec
  let arr :: Array Int
      arr = []
  log $ show $ uncons arr
  log $ show $ uncons arr
  log $ show $ uncons [99]
  log $ show $ uncons [0,1,2,3,4]
  log $ show $ drop 0 [0,1,2,3,4]
  log $ show $ drop 2 [0,1,2,3,4]
  log $ show $ drop 20 [0,1,2,3,4]
  let sentence = ["Hello", "World", "!"]
  logShow $ index sentence 0
  logShow $ index sentence 7
  logShow $ index arr 0
  log $ show $ concat [[1, 2, 3], [], [4, 5, 6]]
  {-- let ers = do --}
  {--       rs <- (replicateA 5 (log "mmm")) --}
  {--       pure unit --}
  let ers :: Int -> Effect (Array Number)
      ers n = do
        ls0 <- boxMuller
        ls1 <- boxMuller
        ls2 <- boxMuller
        ls3 <- boxMuller
        ls4 <- boxMuller
        {-- lss <- replicateA ((n+1)/2) boxMuller --}
        lss <- replicateA 5 boxMuller
        let lls = take n $ concat [ls0, ls1, ls2, ls3, ls4]
        let ls = take n $ concat lss
        let xx = 12 `debug` (show lls <> "  randoms")
        let x0 = 12 `debug` (show ls <> "  randoms")
        pure ls
      rs :: Int -> Effect Unit
      rs n = do
         ls <- ers n
         let x0 = 12 `debug` (show ls <> "xxxxxxxxx")
         pure unit
  uuu <- forE 1 6 (\i -> rs i)
  rr1 <- normals 5
  {-- ms <- replicateM 5 $ rs 5 --}
  log $ "xxxxxxxxxxxxx"
  log "FVT Test Suite"
  log "--Test Cov"
  log $ testCov2
  log "--Test hSlurp"
  {-- log "Test hSlurp dat/tr05129e001412.dat" --}
  {-- testHSlurp =<< readData "dat/tr05129e001412.dat" --}
  logShow $ hSlurp tr05129e001412
{--   logShow $ hSlurp tav4 --}
{--   logShow $ hSlurpMCtruth tav4 --}
  log "--Test FVT 1"
  -- send the list of tau tracks and a VHMeas to testFVT
  {-- testFVT [0,2,3,4,5] <<< uJust <<< hSlurp =<< readData "dat/tr05129e001412.dat" --}
  testFVT [0,2,3,4,5] <<< uJust <<< hSlurp $ tr05129e001412
  {-- log "--Test FVT 2" --}
  {-- testFVT [0,1,2,4,5] <<< uJust <<< hSlurp =<< readData "dat/tr05158e004656.dat" --}
  {-- log "--Test FVT 3" --}
  {-- testFVT [0,2,3,4,5] <<< uJust <<< hSlurp =<< readData "dat/tr07849e007984.dat" --}
  log "--Test Cluster"
  {-- ds <- readData "dat/tr05129e001412.dat" --}
  --ds <- readData "dat/tav-0.dat"
  {-- let ds = tr05129e001412 --}
  {-- let vm = uJust $ hSlurp ds --}
  {-- traverse_ showHelix $ helices vm --}
  {-- traverse_ showMomentum $ helices vm --}
  {-- doCluster vm --}
  log "--Test Random"
  testRandom 50 <<< hFilter [0,2,3,4,5] <<< vBlowup 10000.0
                 <<< uJust <<< hSlurp $ tr05129e001412
  pure unit

{-- foreign import joinxxx :: String -> Array String -> String --}

showMomentum :: HMeas -> Effect Unit
showMomentum h = log $ "pt,pz,fi,E ->" <> (show <<< fromHMeas) h
showHelix :: HMeas -> Effect Unit
showHelix h = log $ "Helix ->" <> (show h)
showProng :: Prong -> Effect Prong
showProng (Prong pr@{nProng: n, fitVertex: v, fitMomenta: ql, fitChi2s: cl, measurements: m}) = do
  let
      showCl :: String -> Array Chi2 -> String
      showCl = foldl (\s (Chi2 x) -> s <> to1fix x)
      Chi2 chi2tot = sum cl
      sc = "chi2tot ->" <> to1fix chi2tot <> ", ndof " <> show (n*2)
      sd = ", r ->" <> (show $ distance v mempty)
      scl = showCl ", chi2s ->" cl
      sm = ", Mass ->" <> show (invMass (map fromQMeas ql))
  log $ sc <> sd <> scl <> sm
  pure $ Prong pr

testFVT :: Array Int -> VHMeas -> Effect Unit
testFVT l5 vm = do
  let hel = helices vm
  traverse_ showHelix hel
  traverse_ showMomentum hel
  doFitTest vm l5
  _ <- showProng <<< fit <<< hFilter l5 <<< vBlowup 10000.0 $ vm
  pure unit

doFitTest :: VHMeas 
            -> Array Int
            -> Effect Unit
doFitTest vm' l5 = do
  let vm = vBlowup 10000.0 vm'
  let showLen xs = show $ length xs
      showQChi2 :: (Tuple QMeas Chi2) -> Effect Unit
      showQChi2 (Tuple qm (Chi2 chi2)) = log $ "q"
                                <> " chi2 ->" <> to1fix chi2
                                <> " pt,pz,fi,E ->"
                                <> show qm

  log $           "initial vertex position -> " <> show ((vertex vm)::XMeas)

  let pl         = map (fromQMeas <<< fromHMeas) $ helices vm
  log $ "Inv Mass " <> showLen pl <> " helix" <> show (invMass pl)
  let pl5        = map (fromQMeas <<< fromHMeas) (helices <<< hFilter l5 $ vm)
  log $ "Inv Mass " <> showLen pl5 <> " helix" <> show (invMass pl5)

  log             "Fitting Vertex --------------------"
  let pr = fit vm
      Prong {fitVertex: vf, fitMomenta: ql, fitChi2s: cl} = fit vm
  log $           "Fitted vertex -> " <> show vf
  traverse_ showQChi2 $ zip ql cl
  log $ "Inv Mass " <> show (length ql) <> " fit" 
                    <> show (invMass (map fromQMeas ql))

  let m5 = invMass <<< map fromQMeas <<< iflt l5 $ ql
  log $ "Inv Mass " <> show (length l5) <> " fit" <> show m5

  log $           "Refitting Vertex-----------------"
  let Prong {fitVertex, fitMomenta, fitChi2s, nProng} = fit <<< hFilter l5 $ vm
  log $           "Refitted vertex -> " <> show fitVertex
  traverse_ showQChi2 $ zip fitMomenta fitChi2s
  log $           "Inv Mass " <> show nProng <> " refit" <> (show <<< invMass <<< map fromQMeas $ fitMomenta)
  log $           "Final vertex -> " <> show fitVertex
  log $           "end of doFitTest------------------------------------------"



tr05129e001412 :: String
tr05129e001412 = """
   3.355679512023926       3.489715576171875       7.110095977783203    
  0.2884106636047363      0.2967556118965149      0.4457152485847473    
  0.2967556118965149      0.3057302236557007      0.4589158892631531    
  0.4457152485847473      0.4589158892631531      0.7007381319999695    
  4.5451703E-03
           6
  9.0513890609145164E-04   1.174186706542969      0.7913663387298584    
 -5.4129425436258316E-02   1.309153556823730    
  3.0409931517372257E-11  3.0817798313265143E-10 -2.6150961396353978E-09
 -6.2086684238238377E-08  1.9006475560079394E-10  3.0817798313265143E-10
  3.5358195873413933E-06 -5.5664237663677341E-09 -4.7704439509743679E-08
 -3.5389247932471335E-04 -2.6150961396353978E-09 -5.5664237663677341E-09
  3.9334932466772443E-07  9.2603177108685486E-06 -4.2692363422247581E-07
 -6.2086684238238377E-08 -4.7704439509743679E-08  9.2603177108685486E-06
  2.7857377426698804E-04 -1.2511900422396138E-05  1.9006475560079394E-10
 -3.5389247932471335E-04 -4.2692363422247581E-07 -1.2511900422396138E-05
  4.6403184533119202E-02
 -3.2948562875390053E-04  -1.287435531616211       3.964143753051758    
 -5.5920504033565521E-02   2.172087669372559    
  1.0773015292342425E-11  1.0870629917059116E-11 -9.4798713323740458E-10
 -2.6224558524745589E-08  5.1304871462320989E-10  1.0870629917059116E-11
  1.3991236755828140E-06  6.1739335865951261E-11  3.9363889925425610E-09
 -1.3362320896703750E-04 -9.4798713323740458E-10  6.1739335865951261E-11
  1.0642112613368226E-07  3.0040880574233597E-06 -5.7571856615368233E-08
 -2.6224558524745589E-08  3.9363889925425610E-09  3.0040880574233597E-06
  1.0815335554070771E-04 -1.6780244322944782E-06  5.1304871462320989E-10
 -1.3362320896703750E-04 -5.7571856615368233E-08 -1.6780244322944782E-06
  1.5890464186668396E-02
  8.6099491454660892E-04   1.190025329589844      0.7718949913978577    
  -1.004449844360352       4.974927902221680    
  7.8076378695612902E-10 -2.4755367200590683E-10 -1.0359136126680824E-07
 -6.7278465394338127E-06  4.4596313841793744E-07 -2.4755367200590683E-10
  6.6328821048955433E-06  2.8732655366070503E-08  1.5816522136447020E-06
 -8.9828821364790201E-04 -1.0359136126680824E-07  2.8732655366070503E-08
  1.3829509043716826E-05  9.0345303760841489E-04 -5.9563441027421504E-05
 -6.7278465394338127E-06  1.5816522136447020E-06  9.0345303760841489E-04
  5.9390719980001450E-02 -3.8860931526869535E-03  4.4596313841793744E-07
 -8.9828821364790201E-04 -5.9563441027421504E-05 -3.8860931526869535E-03
  0.1251238286495209    
 -1.7263018526136875E-03   1.039703369140625      0.8659646511077881    
  0.2599024176597595       2.128120422363281    
  1.5148657328545312E-10 -7.3402152411805588E-11 -1.4714315987873761E-08
 -6.3192055677063763E-07 -3.4522088299127063E-08 -7.3402152411805588E-11
  1.5436929743373184E-06 -5.5447091362736955E-10 -8.1613094948806975E-08
 -1.5131152758840472E-04 -1.4714315987873761E-08 -5.5447091362736955E-10
  1.5367089645224041E-06  6.8635607021860778E-05  4.2090109673154075E-06
 -6.3192055677063763E-07 -8.1613094948806975E-08  6.8635607021860778E-05
  3.2065853010863066E-03  1.9913408323191106E-04 -3.4522088299127063E-08
 -1.5131152758840472E-04  4.2090109673154075E-06  1.9913408323191106E-04
  1.7373077571392059E-02
  1.2108741793781519E-03   1.282915115356445      0.8532057404518127    
  8.5045360028743744E-03   1.965600013732910    
  3.6512477069594595E-11  8.9357354848829118E-10 -3.3482463468459400E-09
 -8.1875484170268464E-08  9.6036401053822829E-10  8.9357354848829118E-10
  3.0787202831561444E-06 -2.2171841251861224E-08 -2.7003440550288360E-07
 -1.5695679758209735E-04 -3.3482463468459400E-09 -2.2171841251861224E-08
  5.5774097518224153E-07  1.3075616152491421E-05 -4.9851792027766351E-07
 -8.1875484170268464E-08 -2.7003440550288360E-07  1.3075616152491421E-05
  3.5224124439992011E-04 -1.4417236343433615E-05  9.6036401053822829E-10
 -1.5695679758209735E-04 -4.9851792027766351E-07 -1.4417236343433615E-05
  1.7541546374559402E-02
 -7.3608336970210075E-04   1.297574043273926      0.8316786885261536    
  -1.011060714721680      -2.867138862609863    
  2.0176718074083055E-09  9.1418789205377493E-10 -2.5551665316925209E-07
 -1.5318933947128244E-05 -3.4175937457803229E-07  9.1418789205377493E-10
  7.4829795266850851E-06 -1.1038221003900617E-07 -6.1672653828281909E-06
 -9.3757675494998693E-04 -2.5551665316925209E-07 -1.1038221003900617E-07
  3.2483072573086247E-05  1.9545238465070724E-03  4.3123862269567326E-05
 -1.5318933947128244E-05 -6.1672653828281909E-06  1.9545238465070724E-03
  0.1181144416332245      2.5763250887393951E-03 -3.4175937457803229E-07
 -9.3757675494998693E-04  4.3123862269567326E-05  2.5763250887393951E-03
  0.1227073818445206    
"""
tav4 :: String
tav4 = """PU_zpositions:  190 4.06972837448 2.44204807281 7.82136058807 -0.621172726154 -6.80061435699 -1.73116350174 -5.42739343643 -7.10662841797 -6.32562208176 -3.72315001488 1.66695046425 6.55822181702 -7.12538957596 -0.389555871487 -2.8334877491 3.09819436073 -5.65534687042 12.068236351 -1.79448211193 5.73383188248 1.68428444862 2.1804420948 8.66328144073 -12.8040647507 -1.1730145216 -3.57441878319 6.21948480606 -1.26211774349 -3.4871032238 -9.48501300812 -8.33902263641 -1.71619582176 -1.56027853489 1.49686825275 -1.69698286057 1.69038307667 5.10251283646 -2.57128977776 0.749759852886 -2.58463263512 -9.792719841 -8.84095287323 -0.131224393845 -1.56865620613 -5.81232976913 4.21827507019 -4.92665529251 -5.84215211868 -5.74135446548 3.38353490829 -3.13945651054 4.30185222626 -12.6121692657 1.54116880894 1.38944470882 -6.84423398972 2.88845825195 -4.16181087494 6.3093957901 -1.70226609707 3.62256598473 -1.38095474243 1.69552695751 -9.44017601013 2.82410240173 -2.21053552628 2.34878325462 -8.67048835754 1.25067412853 9.49777984619 8.16330623627 -0.870663702488 -4.79498910904 1.78941035271 -7.03154611588 1.68979644775 -0.484967201948 -4.18258905411 0.0788396298885 -4.69477128983 2.32463097572 -2.10498857498 -5.34199571609 3.32180857658 -5.39752531052 -2.84948658943 -2.68618583679 1.0778503418 0.443690419197 -3.29635429382 0.936188876629 -4.41851854324 -3.29131436348 2.12316703796 -10.6452322006 -14.0393047333 3.74121594429 -8.4497051239 -5.68886137009 8.31489753723 -4.49255418777 -7.92309999466 -7.26154613495 -2.43943715096 2.87128973007 -8.41958713531 -5.04697036743 -2.6269865036 -3.01578998566 5.666908741 4.7386713028 4.83959341049 -12.2599534988 6.80844593048 -7.59651374817 1.77152347565 -3.49425053596 4.14569759369 2.39712738991 0.695241510868 0.351206511259 -1.00542604923 -0.592145264149 8.05185890198 1.35937333107 -3.23685288429 1.82836604118 -1.08040130138 -4.06748771667 -1.22976350784 -5.24559354782 4.77764129639 -7.92655897141 6.87241268158 8.90295886993 -10.4462614059 5.51054620743 4.28739690781 -0.413518726826 -2.84266161919 -4.82323074341 -3.47484374046 -6.56179046631 -5.6174902916 2.68036007881 -4.87207984924 -3.47317409515 -1.94823920727 -11.0047950745 -6.04952716827 -12.1523780823 -0.171474739909 1.82068359852 -11.1572389603 -2.97859430313 -3.65392804146 1.67614769936 -4.62239599228 4.72258663177 -3.13622426987 -9.94389533997 -13.6851511002 1.98555517197 4.60026597977 -10.9611978531 -1.63044011593 8.50263690948 -9.76078033447 0.933302462101 6.68330335617 -2.94098043442 -8.59897899628 -0.908704698086 -5.6248884201 -9.19552707672 -6.67034435272 3.34288668633 -2.66896915436 -5.85388660431 -6.08788156509 -9.28157234192 -3.39719057083 -2.08446788788 3.61256814003 4.3055267334 -3.20882606506 -1.37032854557 6.3657708168 -7.99672412872 7.93814659119
0.104794 0.168646 -1.00377 0.0015033299569 0.0 0.0 0.0 0.00151841994375 0.0 0.0 0.0 5.21037006378
1.0
4
-0.450663641447 1.35035226203 1.18063795337 0.0660153061812 -1.23642665653 1.75648085587e-06 1.09397257919e-09 2.97465732046e-07 -4.61079963543e-07 -2.34128183507e-08 1.09397257919e-09 1.44003013247e-06 8.51150190329e-08 2.84054323174e-07 -1.69398772414e-05 2.97465732046e-07 8.51150190329e-08 3.07370719383e-05 -7.83819778007e-05 -3.34401283908e-06 -4.61079963543e-07 2.84054323174e-07 -7.83819778007e-05 0.000201181290322 2.55465602095e-06 -2.34128183507e-08 -1.69398772414e-05 -3.34401283908e-06 2.55465602095e-06 0.000202862953302
0.425837572723 -1.33656916571 -0.200853553729 0.186440212098 0.447653308602 4.94152754982e-06 -1.85234905192e-09 7.72127236814e-07 -1.24222322029e-06 -3.99287580777e-09 -1.85234905192e-09 1.2965380165e-06 1.72925282982e-08 4.64915046905e-07 -1.68021942955e-05 7.72127236814e-07 1.72925282982e-08 2.40928802668e-05 -7.24880374037e-05 -2.46094759859e-06 -1.24222322029e-06 4.64915046905e-07 -7.24880374037e-05 0.000219607783947 7.73318163283e-07 -3.99287580777e-09 -1.68021942955e-05 -2.46094759859e-06 7.73318163283e-07 0.000218317465624
0.292514034965 1.39665579944 -0.993975573833 0.141543926193 0.40182813437 6.18352771653e-07 2.45095588269e-09 2.34424803125e-07 -8.39045242174e-07 -8.35598825688e-08 2.45095588269e-09 8.66051891535e-07 4.61750582215e-10 -3.98558910319e-07 -1.4314922737e-05 2.34424803125e-07 4.61750582215e-10 2.8809732612e-05 -8.27241237857e-05 2.18257969209e-06 -8.39045242174e-07 -3.98558910319e-07 -8.27241237857e-05 0.000238582899328 4.61562564169e-07 -8.35598825688e-08 -1.4314922737e-05 2.18257969209e-06 4.61562564169e-07 0.000236972875427
-0.29652562498 1.37864870921 -1.02387889924 0.192178996941 0.0749212341852 1.4916007558e-06 5.99006577673e-09 7.09483913397e-07 -2.9230166092e-06 -3.92529898363e-07 5.99006577673e-09 7.67293840909e-07 5.73736658183e-09 2.58630308281e-07 -1.14107451736e-05 7.09483913397e-07 5.73736658183e-09 2.14707033592e-05 -6.15742173977e-05 -1.75877414677e-06 -2.9230166092e-06 2.58630308281e-07 -6.15742173977e-05 0.000178089467227 1.27894770685e-06 -3.92529898363e-07 -1.14107451736e-05 -1.75877414677e-06 1.27894770685e-06 0.000170099316165
"""

