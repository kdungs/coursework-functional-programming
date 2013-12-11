import Data.Char(ord)

intToReverseBinary :: Int -> [Int]
intToReverseBinary 0 = []
intToReverseBinary x = x `mod` 2 : intToReverseBinary (x `div` 2)

intTo8BitBinary :: Int -> [Int]
intTo8BitBinary x = reverse(take 8 (intToReverseBinary x ++ repeat 0))

binaryEncoding :: String -> [Int]
binaryEncoding s = concat $ map (intTo8BitBinary . ord) s

main = print $ binaryEncoding "Hallo"
