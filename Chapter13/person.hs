import Data.Either (isRight,fromRight,fromLeft)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String
    deriving (Eq, Show)
    

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "Enter da name:"
    name <- getLine
    putStrLn "Enter dem ages:"
    age <- getLine
    let person = mkPerson name (read age) in
        if isRight person then do
            putStrLn "Yay! Successfully got a person: " 
            putStrLn $ show $ fromRight (Person "NoName" 0) person
        else do
            putStrLn "An error occured!"
            putStrLn $ show $ fromLeft NameEmpty person

