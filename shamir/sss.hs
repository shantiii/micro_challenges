import System.IO
import System.Random
import Data.Maybe (fromJust)
-- Galois Field includes
import Data.List (findIndex)
import Data.Int
import Data.Word -- unsigned integral types
import Data.Bits -- bitwise manipulation
import qualified Data.ByteString as BS

split pieces threshold secret = ()
join :: [String] -> Maybe String
join [] = Nothing
join [piece] = Just piece

prompt :: Read a => String -> IO a
prompt str = do
  putStr str
  hFlush stdout
  a <- readLn
  return a

promptStr :: String -> IO String
promptStr str = do
  putStr str
  hFlush stdout
  a <- getLine
  return a

-- Galois Field (finite field) math!
type GFInt = Word8
gfAdd :: GFInt -> GFInt -> GFInt
gfAdd a b = xor a b

gfSub = gfAdd

gfSum [] = 0
gfSum (x:xs) = gfAdd x $ gfSum xs

gfMul :: GFInt -> GFInt -> GFInt
gfMul 0 _ = 0
gfMul _ 0 = 0
gfMul a b = gfMulLoop a b 0 8
  where
    gfMulLoop x y p 0 = p
    gfMulLoop x y p l = let
      new_p = if testBit y 0
        then xor p x
        else p
      rot_x = rotateL x 1
      new_x = if testBit rot_x 0
        then xor (clearBit rot_x 0) 0x1b
        else clearBit rot_x 0
      new_y = rotateR y 1
      in
      gfMulLoop new_x new_y new_p (l-1)

gfMulLoop
gfFastMul :: GFInt -> GFInt -> GFInt
gfFastMul 0 _ = 0
gfFastMul _ 0 = 0
gfFastMul a b = gfAntiLog $ mod (gfLog a + gfLog b) 255

gfDiv :: GFInt -> GFInt -> GFInt
gfDiv a b = gfMul a $ gfInv b

-- Multiplicative Inverse
gfInv :: GFInt -> GFInt
gfInv 0 = 0
gfInv a = gfAntiLog $ 255 - gfLog a

gfAntiLogTable :: [GFInt]
gfAntiLogTable = take 256 $ iterate (gfMul 3) 1

gfLogTable :: [GFInt]
gfLogTable = take 256 $ 0 : map (\x -> fromIntegral $ fromJust $ findIndex (==x) $ gfAntiLogTable) [1..]

gfExp :: (Integral a) => GFInt -> a -> GFInt
gfExp 1 _ = 1
gfExp _ 0 = 1
gfExp a 1 = a
gfExp a b = gfMul a $ gfExp a b-1

gfAntiLog :: GFInt -> GFInt
gfAntiLog a = gfAntiLogTable !! fromIntegral a

gfLog :: GFInt -> GFInt
gfLog a = gfLogTable !! fromIntegral a

-- End Galois Field functions
--getCoefficients :: Int -> [Int]
getCoefficients n = do
  gen <- newStdGen
  return (take n $ randoms gen)

--lagrangeTerm :: Integral a => a -> a -> GFInt -> GFInt -> GFInt
--lagrangeTerm i j u_j u_i = if i == j
  --then 1
  --else gfDiv u_j $ gfAdd u_j u_i

--lagrangeInterpolation :: Integral a => a -> a -> [GFInt] -> GFInt
--lagrangeInterpolation i m u =

-- given a share index x and a byte array a of length m
-- a[0] = byte to encode
generateShareByte :: GFInt -> [GFInt] -> GFInt
generateShareByte x a = gfSum $ zipWith (gfMul) a $ iterate (gfMul x) 1

genSB :: GFInt -> [GFInt] -> GFInt -> GFInt
genSB shareIndex randos byte = generateShareByte shareIndex (byte:randos)

generateShare :: GFInt -> GFInt -> [GFInt] -> [GFInt] -> [GFInt]
generateShare threshold shareIndex coefficients secret =
  zipWith coefficients

-- generates numShares shares, each of which consists of a number of bytes
generateShares :: GFInt -> GFInt -> [GFInt] -> [[GFInt]]
generateShares threshold numShares secret = let
  shareIndices = [1..numShares] -- sharewise
  coefficients = take threshold-1 $ repeat $ getCoefficients (length secret) -- sharewise
  in
  map (\shareIndex -> generateShare ) shareIndices

createShare :: (Integral a) => BS.ByteString -> a -> () --ByteString
createShare string shareIndex = ()

--createShareByte :: (Integral a) => Word8 -> a -> a -> ()--Word8
--createShareByte byte shareIndex coeffs = let (c:cf) <- coeffs
--  in gfSum $ gfMul c $ gfExp byte iter

main = do
  generator <- getStdGen
  --print $ take 10 (randoms generator :: [Int])
  pieces <- prompt "# of shards:"
  threshold <- prompt "# required:"
  --secret <- promptStr "Secret:"
  --print (pieces :: Int, threshold :: Int, secret)
  a <- prompt "a:"
  b <- prompt "b:"
  print (gfMul a b)
  putStrLn "Finished."
