module Lib
    ( futter_main )
    where


data Beutentyp = Dadant
               | Normalmass
               | Warre

data Teil = Zarge
          | Rahmen
          | Deckel
          | Boden
          | Fuetterer
          | Kissen

data Beutenteil = Beutenteil Beutentyp Teil

showTeil :: Teil -> String
showTeil Boden     = "Boden"
showTeil Zarge     = "Zarge"
showTeil Rahmen    = "Rahmen"
showTeil Fuetterer = "Fütterer"
showTeil Kissen    = "Kissen"
showTeil Deckel    = "Deckel"

showBeutentyp :: Beutentyp -> String
showBeutentyp Warre      = "Warré"
showBeutentyp Normalmass = "Normalmass"
showBeutentyp Dadant     = "Dadant"

showBeutenteil :: Beutenteil -> String
showBeutenteil (Beutenteil typ teil) =
  showBeutentyp typ ++ "-" ++ showTeil teil

gewicht :: Beutenteil -> Float
gewicht (Beutenteil Warre      Boden    ) = 1.5
gewicht (Beutenteil Warre      Zarge    ) = 2.44
gewicht (Beutenteil Warre      Rahmen   ) = 0.115
gewicht (Beutenteil Warre      Fuetterer) = 1.32
gewicht (Beutenteil Warre      Kissen   ) = 2.0
gewicht (Beutenteil Warre      Deckel   ) = 5.0

gewicht (Beutenteil Normalmass Boden    ) = 1.75
gewicht (Beutenteil Normalmass Zarge    ) = 2.08
gewicht (Beutenteil Normalmass Rahmen   ) = 0.130
gewicht (Beutenteil Normalmass Fuetterer) = 1.6
gewicht (Beutenteil Normalmass Kissen   ) = 0
gewicht (Beutenteil Normalmass Deckel   ) = 5.0

gewicht (Beutenteil Dadant     Boden    ) = 0
gewicht (Beutenteil Dadant     Zarge    ) = 3.7
gewicht (Beutenteil Dadant     Rahmen   ) = 0.275
gewicht (Beutenteil Dadant     Fuetterer) = 2.5
gewicht (Beutenteil Dadant     Kissen   ) = 2.0
gewicht (Beutenteil Dadant     Deckel   ) = 5.0

anzahl :: Beutenteil -> Float
anzahl (Beutenteil Warre      Boden    ) = 1
anzahl (Beutenteil Warre      Zarge    ) = 3
anzahl (Beutenteil Warre      Rahmen   ) = 8
anzahl (Beutenteil Warre      Fuetterer) = 1
anzahl (Beutenteil Warre      Kissen   ) = 1
anzahl (Beutenteil Warre      Deckel   ) = 1

anzahl (Beutenteil Normalmass Boden    ) = 1
anzahl (Beutenteil Normalmass Zarge    ) = 2
anzahl (Beutenteil Normalmass Rahmen   ) = 11
anzahl (Beutenteil Normalmass Fuetterer) = 1
anzahl (Beutenteil Normalmass Kissen   ) = 0
anzahl (Beutenteil Normalmass Deckel   ) = 1

anzahl (Beutenteil Dadant     Boden    ) = 1
anzahl (Beutenteil Dadant     Zarge    ) = 1
anzahl (Beutenteil Dadant     Rahmen   ) = 10
anzahl (Beutenteil Dadant     Fuetterer) = 1
anzahl (Beutenteil Dadant     Kissen   ) = 1
anzahl (Beutenteil Dadant     Deckel   ) = 1

typKurz :: String -> Beutentyp
typKurz "d" = Dadant
typKurz "n" = Normalmass
typKurz "w" = Warre

fuetterung :: Int -> Bool
fuetterung f | f == 1 = True
             | f == 0 = False

sollgewicht :: Beutentyp -> Int -> Float
sollgewicht bt ft = nettogewicht bt ft + 22.0

nettogewicht :: Beutentyp -> Int -> Float
nettogewicht bt ft = boden + zarge + fuetterer + kissen + deckel
 where
  boden = gewicht (Beutenteil bt Boden)
  zarge =
    (gewicht (Beutenteil bt Zarge) * anzahl (Beutenteil bt Zarge))
      + (gewicht (Beutenteil bt Rahmen) * anzahl (Beutenteil bt Rahmen) * anzahl
          (Beutenteil bt Zarge)
        )
  fuetterer | fuetterung ft == True = gewicht (Beutenteil bt Fuetterer)
            | otherwise             = 0
  kissen = gewicht (Beutenteil bt Kissen)
  deckel = gewicht (Beutenteil bt Deckel)

vorhanden :: Float -> Beutentyp -> Int -> Float
vorhanden aktuell bt ft = aktuell - (nettogewicht bt ft) - 2.0

futter :: Float -> Beutentyp -> Int -> Float
futter aktuell bt ft = 1.3 * ((sollgewicht bt ft) - aktuell)

myRound :: Float -> Float
myRound x = fromIntegral (round (x * 10)) / 10

muster :: [Char] -> [Char] -> Bool
muster _  []       = True
muster mu (x : xs) = if x `elem` mu then (True && (muster mu xs)) else False

richtigeEingabe :: [Char] -> [Char] -> Bool
richtigeEingabe _  [] = False
richtigeEingabe mu x  = muster mu x

eingabeFloat :: IO Float
eingabeFloat = do
  eingabe <- getLine
  if not (richtigeEingabe "0123456789." eingabe)
    then eingabeFloat
    else do
      let ergebnis = read (eingabe) :: Float
      return ergebnis

eingabeMuster :: String -> IO String
eingabeMuster mu = do
  eingabe <- getLine
  if not (richtigeEingabe mu eingabe)
    then eingabeMuster mu
    else do
      let ergebnis = eingabe
      return ergebnis

futter_main :: IO ()
futter_main = do
  putStrLn ("Beutentyp [d]adant / [n]ormalmass / [w]arré)?")
  btt <- eingabeMuster "dnw"
  let bt = typKurz (btt)
  putStrLn ("aktuelles Gewicht?")
  ak <- eingabeFloat
  putStrLn ("Fütterer [j]a / [n]ein)?")
  ft <- eingabeMuster "jn"
  let f | ft == "j" = 1
        | otherwise = 0
  let ftt = ("Fütterer: " ++ txt)
       where
        txt | ft == "j" = "ja"
            | otherwise = "nein"
  putStrLn ("Beutentyp: " ++ (showBeutentyp bt))
  putStrLn ftt
  putStr "Sollgewicht: "
  let tweight = show (myRound (sollgewicht bt f))
  putStrLn (tweight ++ " kg")
  putStr "Nettogewicht: "
  let mweight = show (myRound (nettogewicht bt f))
  putStrLn (mweight ++ " kg")
  let vorh = show (myRound v)
       where
        v | (vorhanden ak bt f) >= 0 = (vorhanden ak bt f)
          | otherwise                = 0
  putStrLn ("Futter vorhanden: " ++ vorh ++ " kg")
  let futt = show (myRound fut)
       where
        fut | (futter ak bt f) >= 0 = (futter ak bt f)
            | otherwise             = 0
  putStrLn ("noch zu füttern: " ++ futt ++ " Liter 3:2")
  putStrLn ("neue Berechnung [j]a / [n]ein)?")
  b <- getLine
  if b == "j" then futter_main else return ()
