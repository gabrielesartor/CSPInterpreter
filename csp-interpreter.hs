-- PARSER E INTERPRETE CSP
-- 
--
-- NP
-- Pfixing .
-- ExternalChoice +
-- InternalChioce $
-- ParallelComposition |
--
--------------------------------------------------------------------
-- ESEMPIO SLIDE
-- parser "(-coin.(coffee.0&tea.0)&0)|coin.(-coffee.0+-tea.0)"
-- > [IC (Pfix "-coin" (IC (Pfix "coffee" NP) (Pfix "tea" NP))) NP,Pfix "coin" (EC (Pfix "-coffee" NP) (Pfix "-tea" NP))]
-- interpreter [Pfix "-coin" (IC (Pfix "coffee" NP) (Pfix "tea" NP)),Pfix "coin" (EC (Pfix "-coffee" NP) (Pfix "-tea" NP))]
--     OPPURE
-- interpreter (parser "-coin.(coffee.0&tea.0)&0|coin.(-coffee.0+-tea.0)")

-- ESEMPIO LIBRAIO, CLIENTE E SPEDIZIONIERE
-- parser "(-titolo1.-paga.-ind.libro.0&-titolo2.-paga.-ind.libro.0)|(titolo1.paga.ind.-ind2.0+titolo2.paga.ind.-ind2.0)|ind2.-libro.0"
-- CLIENTE = IC (Pfix "-titolo1" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP)))) (Pfix "-titolo2" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP))))
-- LIBRAIO = EC (Pfix "titolo1" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP)))) (Pfix "titolo2" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP))))
-- SPEDIZIONIERE = Pfix "ind2" (Pfix "-libro" NP)

-- PER UN TEST (Funziona)
-- interpreterDef [IC (Pfix "-titolo1" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP)))) (Pfix "-titolo2" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP)))),EC (Pfix "titolo1" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP)))) (Pfix "titolo2" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP)))),Pfix "ind2" (Pfix "-libro" NP)]

--------------------------------------------------------------------
--                   PARSER
-- Data una stringa del formato "P1|P2|P3|.."
-- ne restituisce la sua parsificazione [P1,P2,P3,..]
-- N.B. Ogni operazione di + e & deve essere scritta tra parentesi (P1+P2) o (P1&P2)
--      Il parser non riconosce gli spazi

import Text.ParserCombinators.Parsec hiding (spaces)  -- Utilizzata nel parser
import System.Environment                             -- Utilizzata nel parser
import Text.ParserCombinators.Parsec.Expr             -- Utilizzata nel parser
import System.Random  -- Utilizzata nell'interprete
import System.Time    -- Utilizzata nell'interprete


type Program = [Process]   -- Processi messi in parallelo

data Process = Pfix Action Process    -- Prefixing
             | EC Process Process     -- Scelta esterna
             | IC Process Process     -- Scelta interna
             | NP                     -- Processo nullo
             deriving Show

type Action = String

-- Mi serve per fare il debug di ogni parser
run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "" input of
    Left err -> do{ putStr "parse error at "
                  ; print err
                  }

    Right val -> print val

-- Restituisce i processi paralleli
parser :: String -> [Process]
parser input = map parserCsp (parProcess input)

-- Restituisce la parsificazione di un processo
parserCsp :: String -> Process
parserCsp input = case parse process "" input of
    Left err -> error "Parsing error"
    Right val -> val

-- Parsifica ogni singolo processo
-- Le scelte interne ed esterne possono essere scritte tra parentesi
process :: Parser Process
process = do{ char '(' 
            ; x <- process
            ; do{ char '+'
                ;  y <- process
                ; char ')'
                ;  return (EC x y)
                }
                <|>do{ char '&'
                     ;  y <- process
                     ; char ')'
                     ;  return (IC x y)
                     }                     
            }
      <|> do{ x <- action
            ; char '.'
            ; y <- process
            ; return (Pfix x y)
            }
      <|> do{ char '0'
            ; return NP
            }
      <?> "wrong expr"
       
-- Parser delle azioni
action :: Parser String
action = do{ c <- oneOf "-"
           ; do { cs <- word
                ;  do { n <- number
                      ; return ((c:cs) ++ n)
                      }
                      <|> return (c:cs)
                }
           }
      <|>do{ cs <- word
           ; do{ n <- number
               ; return (cs ++ n)
               }
               <|> return cs
           }
        <?> "wrong action"

-- Parser di parole
word :: Parser String
word = do{ c <- letter
         ; do { cs <- word
              ; return (c:cs)
              }
              <|> return [c]
         }
       <?> "word"

-- Parser di numeri
number :: Parser String
number = do{ n <- many1 digit
           ; do { ns <- number
                ; return (n++ns)
                }
                <|> return n
           }
        <?> "number"

-- Prende la stringa programma e la divide in una serie di stringhe
-- che rappresentano i singoli processi
parProcess :: String -> [String]
parProcess [] = []
parProcess program | occurs program '|' = (take ((position program '|') -1) program) : (parProcess (drop (position program '|') program))
                   | otherwise = [program]

-- Restituisce se l'elemento e' presente nella lista
occurs :: [Char] -> Char -> Bool
occurs [] _ = False
occurs (x:xs) y | x == y = True
                | otherwise = occurs xs y

-- Restituisce la posizione del carattere nella stringa
position :: [Char] -> Char -> Int
position [] _ = 0
position (x:xs) c | x == c = 1
                  | otherwise = 1 + (position xs c)

-----------------------------------------------------------------------------------------------------------------
--                         INTERPRETE CSP
-- Data una lista di processi paralleli [P1,P2,P3,..]
-- ne restituisce la lista di una possibile riduzione.
-- Esegue solo comunicazioni interne!

interpreter :: Program -> IO ()
interpreter [] = putStrLn "Nessun processo da eseguire."
interpreter program = do reducProc <- (runReduction program)
                         putStrLn "Interpretazione finita."

-- Esecuzione parallela dei processi  (se si possono ridurre, riduco e riprovo)
runReduction :: Program -> IO [Process]
runReduction [] = error "Nessun processo da eseguire."
runReduction processes = do putStrLn (show processes)
                            (p1, p2) <- checkProcesses processes
                            if p1 /= -1 && p2 /= -1 then do putStrLn ("Riduco pos :" ++ (show p1) ++ " e " ++  (show p2) ++ ".")
                                                            procRed <- reduce2 p1 p2 processes -- calcolo la riduzione se e' una coppia
                                                            runReduction procRed -- restituisco la riduzione dei processi ridotti
                            else if p1 /= -1 && p2 == -1 then do putStrLn ("Riduco pos :" ++ (show p1) ++ ".")
                                                                 procRed2 <- reduce1 p1 processes 
                                                                 runReduction procRed2
                            else return processes


-- Controlla prima se ci sono scelte interne (se si riduce la prima), altrimenti controlla se ci sono processi
-- riducibili in coppia
checkProcesses :: [Process] -> IO (Int,Int)
checkProcesses [] = return (-1,-1)
checkProcesses processes = do let z = [z | z <- [0..length processes -1], reducible (processes !! z)] 
                              if (not.null) z then do posRnd <- randomMax (length z)
                                                      return ((z !! posRnd),-1) 
                              else let listToReduce = [ (x,y) | x <- [0..length processes -1], y <- [x..length processes -1], reducibles (processes !! x) ( processes !! y) ] in -- solo quelli riducibili internamente
                                   if (not.null) listToReduce then do posRnd2 <- randomMax (length listToReduce)
                                                                      return (listToReduce !! posRnd2) 
                                   else return (-1,-1)

-- Controllo se i 2 processi sono riducibili con un'operazione interna 
-- (IC da' false perche' deve essere ridotto da solo)
reducibles :: Process -> Process -> Bool
reducibles NP _ = False
reducibles _ NP = False
reducibles (IC p1 p2) _ = False
reducibles _ (IC p1 p2) = False
reducibles (EC p1 p2) (EC p3 p4) = if (reducibles p1 p3) || (reducibles p1 p4) || (reducibles p2 p3) || (reducibles p2 p4) then True else False
reducibles (EC p1 p2) p3 = if (reducibles p1 p3) || (reducibles p2 p3) then True else False
reducibles p3 (EC p1 p2) = if (reducibles p1 p3) || (reducibles p2 p3) then True else False
reducibles (Pfix a1 p1) (Pfix a2 p2) | a1 == ("-"++a2) || a2 == ("-"++a1) = True
                                         | otherwise = False

-- Controlla se un processo e' riducibile da solo
-- Solo IC puo' ridursi da solo
reducible :: Process -> Bool
reducible (IC _ _) = True
reducible _ = False

-- Riduce un processo solo (la head dovrebbe essere sempre giusta perche' la lista non e' mai vuota)
reduce1 :: Int -> [Process] -> IO [Process]
reduce1 pos processes = let (part1,part2) = splitAt pos processes in
                        do x <- (reduction (head part2))
                           return ((part1 ++ [x]) ++ (safetail part2))


-- Riduce il processo in posizione pos1 e dopo pos2
-- (in input abbiamo indici che sappiamo essere riducibili!)
reduce2 :: Int -> Int -> [Process] -> IO [Process]
reduce2 pos1 pos2 processes = let pp1 = processes !! pos1 in
                              let pp2 = processes !! pos2 in
                              case pp1 of (EC p1 p2) -> case pp2 of (Pfix az p3) -> if reducibles p1 pp2 then do procs <- substitution pos1 p1 processes
                                                                                                                 reduce2 pos1 pos2 procs
                                                                                    else if reducibles p2 pp2 then do procs <- substitution pos1 p2 processes
                                                                                                                      reduce2 pos1 pos2 procs
                                                                                    else error "Case EC Pfix sbagliato."
                                                                    (EC p3 p4) -> do x <- randomMax 4
                                                                                     if x == 0 && reducibles p1 p3 then do procs <- substitution pos1 p1 processes
                                                                                                                           procs2 <- substitution pos2 p3 procs
                                                                                                                           reduce2 pos1 pos2 procs2
                                                                                     else if x == 1 && reducibles p2 p3 then do procs <- substitution pos1 p2 processes
                                                                                                                                procs2 <- substitution pos2 p3 procs
                                                                                                                                reduce2 pos1 pos2 procs2
                                                                                     else if x == 2 && reducibles p1 p4 then do procs <- substitution pos1 p1 processes
                                                                                                                                procs2 <- substitution pos2 p4 procs
                                                                                                                                reduce2 pos1 pos2 procs2
                                                                                     else if x == 3 && reducibles p2 p4 then do procs <- substitution pos1 p2 processes
                                                                                                                                procs2 <- substitution pos2 p4 procs
                                                                                                                                reduce2 pos1 pos2 procs2
                                                                                     else reduce2 pos1 pos2 processes
                                          (Pfix az p3) -> case pp2 of (EC p1 p2) -> reduce2 pos2 pos1 processes 
                                                                      _ ->  do procs <- reduce1 pos1 processes
                                                                               reduce1 pos2 procs                   
                                          _ -> do procs <- reduce1 pos1 processes    -- dovrebbe essere un ramo inutilizzato
                                                  reduce1 pos2 procs 


-- Vado a sostituire il processo da in input, nella posizione pos
substitution :: Int -> Process -> [Process] -> IO [Process]
substitution pos proc processes = let (part1,part2) = splitAt pos processes in
                                  return (part1++[proc]++(safetail part2))

-- Va a ridurre una singola azione
reduction :: Process -> IO Process
reduction (Pfix a p) = do --putStrLn ("Consumo : "++a)
                          return p
reduction (IC p1 p2) = do r <- randomMax 2 -- Uno a caso tra i 2
                          if r == 0 then return p1 
                          else if r == 1 then  return p2
                          else error "Errore nella funzione reduction (IC)."
reduction p = return p -- Per la scelta esterna devo decidere prima!


-- Restituisce la coda di una lista
safetail :: [a] -> [a]
safetail [] = []
safetail [x] = []
safetail (x:xs) = xs

-- Stampa il processo in formato CSP
printP :: Process -> IO ()
printP process = case process of NP -> putStr "0"
                                 (Pfix az pr) -> do putStr az
                                                    putStr "."
                                                    printP pr
                                                    
                                 (EC pr1 pr2) -> do putStr "("
                                                    printP pr1
                                                    putStr "+"
                                                    printP pr2
                                                    putStr ")"
                                 (IC pr1 pr2) -> do putStr "("
                                                    printP pr1
                                                    putStr "&"
                                                    printP pr2
                                                    putStr ")"
-- Stampa i processi in parallelo
printProcesses :: [Process] -> IO ()
printProcesses [] = putStrLn ""
printProcesses (x:xs) = do printP x
                           if (length xs) > 0 then do putStr "\x1b[31m|\x1b[0m"
                                                      printProcesses xs
                           else printProcesses xs

-- INTERPRETE : VERSIONE CON STAMPA PIU' LEGGIBILE
interpreterDef :: Program -> IO ()
interpreterDef [] = putStrLn "Nessun processo da eseguire."
interpreterDef program = do reducProc <- (runReduction2 program)
                            putStrLn "Interpretazione finita."

-- Esecuzione parallela dei processi  (se si possono ridurre, riduco e riprovo)
-- VERSIONE CON STAMPA PIU' LEGGIBILE
runReduction2 :: Program -> IO [Process]
runReduction2 [] = error "Nessun processo da eseguire."
runReduction2 processes = do printProcesses processes
                             (p1, p2) <- checkProcesses processes
                             if p1 /= -1 && p2 /= -1 then do putStrLn ("Riduco pos :" ++ (show p1) ++ " e " ++  (show p2) ++ ".")
                                                             procRed <- reduce2 p1 p2 processes -- calcolo la riduzione se e' una coppia
                                                             runReduction2 procRed -- restituisco la riduzione dei processi ridotti
                             else if p1 /= -1 && p2 == -1 then do putStrLn ("Riduco pos :" ++ (show p1) ++ ".")
                                                                  procRed2 <- reduce1 p1 processes 
                                                                  runReduction2 procRed2
                             else return processes

------------------------------------------
-- Random 0 o 1
random1 :: IO Int
random1 = do time <- getClockTime
             return ((tdSec (diffClockTimes time (TOD 0 0))) `mod` 2)

-- Random da 0 a len-1
-- La random e' troppo veloce, quindi ho aggiunto una getChar per rallentare l'esecuzione
randomMax :: Int -> IO Int
randomMax 0 = return 0
randomMax len = do time <- getClockTime
                   --x <- getChar
                   return (((fromIntegral (tdPicosec (diffClockTimes time (TOD 0 0)))) `div` 10000000)  `mod` len)

time1 = do time <- getClockTime
           --x <- getChar
           return ((fromIntegral (tdPicosec (diffClockTimes time (TOD 0 0)))) `div` 10000000)

------- TEST ESEGUIBILI -----------------
-- ESEMPIO SLIDE
-- parser "(-coin.(coffee.0&tea.0)&0)|coin.(-coffee.0+-tea.0)"
-- > [IC (Pfix "-coin" (IC (Pfix "coffee" NP) (Pfix "tea" NP))) NP,Pfix "coin" (EC (Pfix "-coffee" NP) (Pfix "-tea" NP))]
-- interpreter [Pfix "-coin" (IC (Pfix "coffee" NP) (Pfix "tea" NP)),Pfix "coin" (EC (Pfix "-coffee" NP) (Pfix "-tea" NP))]
--     OPPURE
-- interpreter (parser "-coin.(coffee.0&tea.0)&0|coin.(-coffee.0+-tea.0)")
prova1 ::  IO ()
prova1 = interpreterDef (parser "(-coin.(coffee.0&tea.0)&0)|coin.(-coffee.0+-tea.0)")

-- ESEMPIO LIBRAIO, CLIENTE E SPEDIZIONIERE
-- parser "(-titolo1.-paga.-ind.libro.0&-titolo2.-paga.-ind.libro.0)|(titolo1.paga.ind.-ind2.0+titolo2.paga.ind.-ind2.0)|ind2.-libro.0"
-- CLIENTE = IC (Pfix "-titolo1" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP)))) (Pfix "-titolo2" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP))))
-- LIBRAIO = EC (Pfix "titolo1" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP)))) (Pfix "titolo2" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP))))
-- SPEDIZIONIERE = Pfix "ind2" (Pfix "-libro" NP)

-- PER UN TEST (Funziona)
-- interpreterDef [IC (Pfix "-titolo1" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP)))) (Pfix "-titolo2" (Pfix "-paga" (Pfix "-ind" (Pfix "libro" NP)))),EC (Pfix "titolo1" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP)))) (Pfix "titolo2" (Pfix "paga" (Pfix "ind" (Pfix "-ind2" NP)))),Pfix "ind2" (Pfix "-libro" NP)]
prova2 :: IO ()
prova2 = interpreterDef (parser "(-titolo1.-paga.-ind.libro.0&-titolo2.-paga.-ind.libro.0)|(titolo1.paga.ind.-ind2.0+titolo2.paga.ind.-ind2.0)|ind2.-libro.0")

-- Esempio doppia scelta esterna
prova3 :: IO ()
prova3 = interpreterDef (parser "(a.b.0+b.a.0)|(-a.b.0+-b.a.0)")

-- Esempio doppia scelta esterna su 3 processi
prova4 :: IO ()
prova4 = interpreterDef (parser "(a.b.0+b.a.0)|(-a.b.0+-b.a.0)|(-a.b.0+-b.a.0)")

prova5 :: IO ()
prova5 = interpreterDef (parser "a.0|-a.0|b.0|-b.0|b.0|-a.0|a.0|-b.0")

-- Siccome l'operazione + e' associativa, e' uguale a a.0+b.0+c.0|-c.0
prova6 :: IO ()
prova6 = interpreterDef (parser "(a.0+(b.0+c.0))|-c.0")

-- Siccome l'operazione + e' associativa, e' uguale a a.0+b.0+c.0+d.0|-d.0
prova7 :: IO ()
prova7 = interpreterDef (parser "(a.0+(b.0+(c.0+d.0)))|-d.0")

---------- Test vari ----------------
test :: IO ()
test = do x <- randomMax 2
          putStrLn (show x)
          x1 <- randomMax 2
          putStrLn (show x1)
          x2 <- randomMax 2
          putStrLn (show x2)
          x3 <- randomMax 2
          putStrLn (show x3)

testTime :: IO ()
testTime = do x <- time1
              putStrLn (show x)
              x1 <- time1
              putStrLn (show x1)
              x2 <- time1
              putStrLn (show x2)
              x3 <- time1
              putStrLn (show x3)