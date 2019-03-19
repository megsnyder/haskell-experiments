associations = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 .,:;'\"/\\<>(){}[]-=_+?!"
find l= [x | x<-[1..(length associations)], associations!!x==l]!!0

add [] []=[]
add (x:xs) (y:ys) = do
    if x+y < (length associations) then x+y : add xs ys
    else x+y-(length associations) : add xs ys

sub [] []=[]
sub (x:xs) (y:ys) = do
    if x-y >= 0 then x-y : sub xs ys
    else (length associations)+(x-y) : sub xs ys
    
main = do
    putStrLn "Enter e to encrypt, d to decrypt, or q to quit: "
    n <- getLine
    if n == "e" then do
        putStrLn "Message: "
        mess <- getLine
        putStrLn "Key: "
        key <- getLine
        let keyL = take (length mess) (cycle key)
        let code = add (map find mess) (map find keyL)
        putStrLn [associations!!x| x<-code]
        main
    else if n == "d" then do
        putStrLn "Message: "
        mess <- getLine
        putStrLn "Key: "
        key <- getLine
        let keyL = take (length mess) (cycle key)
        let code = sub (map find mess) (map find keyL)
        putStrLn [associations!!x| x<-code]
        main
    else if n == "q" then putStrLn "Goodbye!"
    else do
        putStrLn "Did not understand command, try again."
        main