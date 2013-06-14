adimGoster :: String -> String -> String
adimGoster a b = a++". siradan diski alip "++b++". siraya koy."

adimCoz :: Integer -> String -> String -> String -> [String] 
adimCoz 0 _ _ _ = []
adimCoz n basla bit kullan = adimCoz (n - 1) basla kullan bit ++ [adimGoster basla bit] ++ adimCoz (n - 1) kullan bit basla

hanoi n = adimCoz n "1" "3" "2"

listeyiYazdir :: [String] -> IO ()
listeyiYazdir xs = mapM_ print xs

main :: IO ()
main =  do 
        putStrLn "Disk sayisini girin:"
        x <- getLine
        if (read x :: Integer) > 0
                then do listeyiYazdir (hanoi (read x :: Integer))
			putStrLn (show (length (hanoi (read x :: Integer)))++" adimda tamamlandi.")
                else putStrLn ("0 dan buyuk bir sayi girin")
