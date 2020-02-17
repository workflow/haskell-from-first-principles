data FlowerType = Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
    deriving Show

data Garden' = GardeniaGarden Gardener
    | DaisyGarden Gardener
    | RoseGarden Gardener
    | LilacGarden Gardener