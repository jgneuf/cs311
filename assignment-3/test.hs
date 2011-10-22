allones 0 = True
allones n = if n `mod` 10 == 1 
                then allones (n `div` 10) 
                else False
result = filter allones (map (\x -> x * x - 1) [1..1000000000])

