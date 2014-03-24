data Nat = Zero | Succ Nat

instance Show Nat where
    show = show . convertNat


instance Eq Nat where
    Zero == Zero = True
    Zero == Succ _ = False
    Succ _ == Zero = False
    Succ x == Succ y = x == y

instance Enum Nat where
    succ = Succ
    pred (Succ x) = x
    toEnum 0 = Zero
    toEnum a = Succ . toEnum $ a - 1
    fromEnum = convertNat
    enumFrom a = map toEnum (enumFrom (fromEnum a))
    enumFromTo a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))
    enumFromThen a b = map toEnum (enumFromThen (fromEnum a) (fromEnum b))
    enumFromThenTo a b c = map toEnum (
        enumFromThenTo (fromEnum a) (fromEnum b) (fromEnum c))


convertNat :: Nat -> Int
convertNat Zero = 0
convertNat (Succ x) = 1 + convertNat x 


main = do
    print $ (Succ (Succ (Succ Zero)))
    print $ (Succ Zero) == (Succ Zero)
    print $ Zero == (Succ Zero)
    print $ (Succ Zero) == Zero
    print $ [Zero .. (Succ (Succ (Succ (Succ Zero))))]
    print $ [Zero, (Succ (Succ Zero)) .. (Succ (Succ (Succ (Succ Zero))))]
    --print $ [Zero ..]
