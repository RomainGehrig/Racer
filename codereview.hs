    import Data.List
    
    type Vec2D = (Int, Int)
    type Position = Vec2D
    type Circuit = [[Tile]]
    type Tile = Char
    
    basicCircuit :: Circuit
    basicCircuit = ["------------",
                    "-b        e-",
                    "------------"]
    
    circuitFind :: Circuit -> Tile -> Maybe Position
    circuitFind c t = unpack . find (\ p -> fst p /= Nothing) . enumerate . map findTile $ c
        where findTile = findIndex (==t)
              enumerate = flip zip [0..]
              unpack a = case a of 
                 Just (Just x, y) -> Just (x,y) 
                 _ -> Nothing
    
    {- Tests
    
    Main> circuitFind basicCircuit 'e'
    Just (10,1)
    Main> circuitFind basicCircuit 'o'
    Nothing
    
    -}
