module EitherChain where


data User = User { fName :: String, lName :: String, age :: Int } deriving (Eq, Ord, Show)

validateFName :: User -> Either String User
validateFName (User "" _ _) = Left "first name is invalid"
validateFName user          = Right user

validateLName :: User -> Either String User
validateLName (User _ "" _) = Left "last name is invalid"
validateLName user          = Right user

validateAge :: User -> Either String User
validateAge user@(User _ _ age) | age < 0   = Left "age is invalid"
                                | otherwise = Right user

validateAll :: User -> Either String User
validateAll user = validateFName user >>= validateLName >>= validateAge
