bmi :: Float -> Float -> String
bmi peso altura = 
    let x = peso
        y = altura^2
        imc = x/y
    in  if imc <= 18.5
        then "ABAIXO"
        else if imc >= 30 
             then "ACIMA"
             else "NORMAL"

bmi' :: Float -> Float -> String
bmi' peso altura
    | imc <= 18.5 = "ABAIXO"
    | imc >= 30 = "ACIMA"
    | otherwise = "NORMAL"
    where x = peso
          y = altura^2
          imc = x/y

andTable :: [(Bool, Bool, Bool)]
andTable = zipWith (\x y -> (x,y,x && y)) p q
    where p = [x | x <- [True,True,False,False]]
          q = [x | x <- take qtd $ cycle[True,False]]
          qtd = 4