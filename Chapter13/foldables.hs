maybeAll :: (a -> Bool) -> Maybe a -> Bool
maybeAll = all

eitherAll :: (a -> Bool) -> Either b a -> Bool
eitherAll = all

