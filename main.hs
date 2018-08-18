import Data.Word
import Control.Monad.State
import Control.Monad.Free
import Prelude hiding (Left, Right)

type VM = StateT VMState IO
data VMState = VMState {dmem :: Memory Word8, imem :: Memory Instruction} deriving(Show)
data Memory a = Memory [a] a [a] deriving Show
type Instruction = Char

data IRF next = 
    Add Int next 
    | Sub Int next 
    | Right Int next 
    | Left Int next
    | Output next 
    | Input next
    | Open next
    | Close next 
    | Clear next
    | Mul Int Int next
    | Div Int Int next
    | Copy Int next

instance Functor (IRF) where
    fmap f (Add x next) = Add x (f next)
    fmap f (Sub x next) = Sub x (f next)
    fmap f (Right x next) = Right x (f next)
    fmap f (Output next) = Output (f next)
    fmap f (Input next) = Input (f next)
    fmap f (Open next) = Open (f next)
    fmap f (Close next) = Close (f next)
    fmap f (Mul x y next) = Mul x y (f next)
    fmap f (Div x y next) = Div x y (f next)
    fmap f (Copy x next) = Copy x (f next)


    

type IR = Free IRF

parse :: [Char] -> IR () 
parse ('>':xs) = Free ((Right 1) (parse xs))
parse ('<':xs) = Free ((Left 1) (parse xs))
parse ('+':xs) = Free ((Add 1) (parse xs))
parse ('-':xs) = Free ((Sub 1) (parse xs))
parse ('.':xs) = Free (Output (parse xs))
parse (',':xs) = Free (Input (parse xs))
parse (']':xs) = Free (Close (parse xs))
parse ('[':xs) = Free (Open (parse xs))
parse (_:xs) = parse xs
parse [] = Pure ()

optimize :: IR () -> IR ()
-- Contraction
optimize (Free (Add x (Free (Add y next)))) = optimize (Free (Add (x + y) next))
optimize (Free (Sub x (Free (Sub y next)))) = optimize (Free (Sub (x + y) next))
optimize (Free (Left x (Free (Left y next)))) = optimize (Free (Left (x + y) next))
optimize (Free (Right x (Free (Right y next)))) = optimize (Free (Right (x + y) next))
-- Remove sequences of ><. <>. -+, +-
optimize (Free (Right x (Free (Left y next)))) 
    | x == y = optimize next
    | otherwise = Free $ Right x $ Free $ Left y $ optimize next

optimize (Free (Left x (Free (Right y next)))) 
    | x == y = optimize next
    | otherwise = Free $ Left x $ Free $ Right y $ optimize next

optimize (Free (Add x (Free (Sub y next)))) 
    | x == y = optimize next
    | otherwise = Free $ Add x $ Free $ Sub y $ optimize next
    
optimize (Free (Sub x (Free (Add y next)))) 
    | x == y = optimize next
    | otherwise = Free $ Sub x $ Free $ Add y $ optimize next

-- Clear == [-]
optimize (Free (Open (Free (Sub 1 (Free (Close next)))))) = Free (Clear (optimize next))
optimize (Free f) = Free (fmap (optimize) f)


--Check if r == l
-- [--->+<] == Divide number at pointer with three and add it to cell 1 to the right
-- [->+++<] == Multiply number at pointer with three and add it to cell 1 to the right
-- [->+<] == Copy number at pointer and add to cell 1 to the right
optimizeLoops :: IR () -> IR ()
optimizeLoops (Free (Open (Free (Sub x (Free (Right r (Free (Add y (Free (Left l (Free (Close next))))))))))))
    | y == 1 = Free $ Div x r (optimizeLoops next)
    | x == 1 = Free $ Mul y r (optimizeLoops next)
    | x == 1 && y == 1 = Free $ Copy r (optimizeLoops next)
    | otherwise = (Free (Open (Free (Sub x (Free (Right r (Free (Add y (Free (Left l (Free (Close (optimizeLoops next)))))))))))))

optimizeLoops (Free (Open (Free (Sub x (Free (Left l (Free (Add y (Free (Right r (Free (Close next))))))))))))
    | y == 1 = Free $ Div x (-l) (optimizeLoops next)
    | x == 1 = Free $ Mul y (-l) (optimizeLoops next)
    | x == 1 && y == 1 = Free $ Copy r (optimizeLoops next)
    | otherwise = (Free (Open (Free (Sub x (Free (Left l (Free (Add y (Free (Right r (Free (Close (optimizeLoops next)))))))))))))

increment :: Memory a -> Memory a
increment (Memory ls p (r:rs)) = Memory (p:ls) r rs
increment (Memory ls p []) = (Memory ls p [])

decrement :: Memory a -> Memory a
decrement (Memory (l:ls) dp rs) = Memory ls l (dp:rs)
decrement (Memory [] dp rs) = (Memory [] dp rs)

updateDmem :: (Memory Word8 -> Memory Word8) -> VMState -> VMState
updateDmem f (VMState d i) = VMState (f d) i

updateImem :: (Memory Instruction -> Memory Instruction) -> VMState -> VMState
updateImem f (VMState d i) = VMState d (f i)

memEnd :: Memory a -> Bool
memEnd (Memory _ _ []) = True
memEnd _ = False

load :: Memory a -> a
load (Memory _ p _) = p

write ::Memory a -> a -> Memory a
write (Memory l _ r) p = Memory l p r

main :: IO ()
main = do
    (i:is) <- readFile "example.bf"
    evalStateT execInstruction $ VMState (Memory [] 0 $ repeat 0) $ Memory [] i is

execInstruction :: VM ()
execInstruction = do
    s <- get                                                                                -- Get state
    case (load $ imem $ s) of                                                               -- Pattern match PC pointer to get instruction
        '>' -> modify $ updateDmem increment                                                -- Increment DP
        '<' -> modify $ updateDmem decrement                                                -- Decrement DP
        '+' -> modify $ updateDmem $ write <*> (+1) . load                                  -- Increment byte at DP
        '-' -> modify $ updateDmem $ write <*> (subtract 1) . load                          -- decrement byte at DP ("-1" doesnt play nice with Word8)
        '.' -> liftIO $ printByte $ load $ dmem $ s                                         -- Print byte at DP
        ',' -> do                                                                           -- Read char from IO, write to byte at DP
            byte <- liftIO $ getChar
            modify $ updateDmem $ flip write $ toEnum $ fromEnum byte
        '[' -> return ()                                                                    -- Just leave state as is (could also be "modify id")
        ']' -> 
            case 0 == (load (dmem s)) of                                                    -- If DP is nonzero we jump back to matching [
                True -> return ()
                False -> modify $ updateImem $ jumpBack 0 . decrement
                    where
                        jumpBack :: Int -> Memory Instruction -> Memory Instruction
                        jumpBack 0 m@(Memory _ '[' _) = m                                   -- Recursively decrement PC but track what we pass
                        jumpBack n m@(Memory _ '[' _) = jumpBack (n-1) (decrement m)        --      increment counter for every ] and decrement for [
                        jumpBack n m@(Memory _ ']' _) = jumpBack (n+1) (decrement m)        --      When counter is 0 and we find a [ it's our match!
                        jumpBack n m = jumpBack n (decrement m)                             --      Then resume execution from new PC
        _ -> return ()                                                                      -- Invalid instruction, dont modify state.
    modify $ updateImem increment                                                           -- Increment PC
    case memEnd $ imem $ s of
        True -> return ()                                                                   -- No more instructions, return unit
        False -> execInstruction                                                            -- Else, step again

printByte :: Word8 -> IO()
printByte w = putStrLn $ [toEnum (fromEnum w)] ++ "    " ++ show w
