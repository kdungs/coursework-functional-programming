data RGB = RGB Int Int Int

instance Eq RGB where
    RGB r1 g1 b1 == RGB r2 g2 b2 = and [r1 == r2, g1 == g2, b1 == b2]

main = do
    print $ RGB 1 2 3 == RGB 1 2 3  -- True
    print $ RGB 1 2 3 == RGB 1 2 4  -- False
