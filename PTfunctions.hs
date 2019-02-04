module PTfunctions where
quadraticVertex a b c = - b / (2 * a)
quadraticDiscriminant a b c = b ^ 2 - 4 * a * c
quadraticRealSolutionQty a b c = if quadraticDiscriminant a b c > 0 then 2 else if quadraticDiscriminant a b c == 0 then 1 else 0
convertFtoC f = (f - 32) / 1.8
convertCtoF c = (1.8 * c) + 32
convertTemp t k = if k == "f" then convertFtoC t else if k == "c" then convertCtoF t else 0
