data World = World

type MIO a = World -> (a, World)

done World = ((), World)

putStrLn xs = foldr (>>) done (map putChar xs) >> putChar '\n'

