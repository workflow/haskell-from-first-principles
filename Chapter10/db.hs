import Data.Time

data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase:: [DatabaseItem]
theDatabase =
    [ DbDate
        (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123)
        ),
        DbDate
                (UTCTime
                   (fromGregorian 1912 5 1)
                   (secondsToDiffTime 34123)
                )
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = [t | x@(DbDate t) <- xs]

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' xs = foldr (\x y -> filterOnlyTime x ++ y) [] xs where
    filterOnlyTime (DbDate time) = time : []
    filterOnlyTime _ = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = [n | x@(DbNumber n) <- xs]

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' xs = foldr (\x y -> filterOnlyNumbers x ++ y) [] xs where
    filterOnlyNumbers (DbNumber n) = n : []
    filterOnlyNumbers _ = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = maximum (filterDbDate xs)

sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum $ filterDbNumber xs

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length . filterDbNumber $ xs)


