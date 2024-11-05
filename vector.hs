-- we havent seen for a while, sorry for that

type RotationMatrix a = ((a, a), (a, a))

getRotationMatrix :: Float -> RotationMatrix Float
getRotationMatrix d = (
    (cos d, -sin d),
    (sin d, cos d)
    )

rotateVec :: (Float, Float) -> Float -> (Float, Float)
rotateVec (x, y) d = let ((i, j), (k, l)) = getRotationMatrix d in
    (x * i + y * j, x * k + y * l) 

split :: String -> Char -> [String]
split "" _ = []
split x d = let (a, b) = span (/= d) x in a : split (drop 1 b) d

decToRad :: Float -> Float
decToRad x = (x / 180) * pi

main :: IO ()
main = getLine >>= (\l -> let [x, y, d] = map (\s -> read s :: Float) (split l ',') in print (rotateVec (x, y) $ decToRad d))

