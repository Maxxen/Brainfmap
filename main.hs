import Data.Word
import Control.Monad.State

type VM = StateT VMState IO
data VMState = VMState {dmem :: Memory Word8, imem :: Memory Instruction} deriving(Show)
data Memory a = Memory [a] a [a] deriving Show
type Instruction = Char

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

extract :: Memory a -> a
extract (Memory _ p _) = p

insert ::Memory a -> a -> Memory a
insert (Memory l _ r) p = Memory l p r

main :: IO ()
main = do
    (i:is) <- readFile "example.bf"
    evalStateT execInstruction $ VMState (Memory [] 0 $ repeat 0) $ Memory [] i is

execInstruction :: VM ()
execInstruction = do
    s <- get                                                                                -- Get state
    case (extract $ imem $ s) of                                                            -- Pattern match PC
        '>' -> modify $ updateDmem increment                                                -- Increment DP
        '<' -> modify $ updateDmem decrement                                                -- Decrement DP
        '+' -> modify $ updateDmem $ insert <*> (+1) . extract                              -- Increment byte at DP
        '-' -> modify $ updateDmem $ insert <*> (subtract 1) . extract                      -- decrement byte at DP ("-1" doesnt play nice with Word8)
        '.' -> liftIO $ printByte $ extract $ dmem $ s                                      -- Print byte at DP
        ',' -> do                                                                           -- Read char from IO, write to byte at DP
            byte <- liftIO $ (getChar)
            modify $ updateDmem $ insert <*> (\_ -> toEnum (fromEnum byte)) . extract
        '[' -> put s                                                                        -- Just leave state as is (could also be "modify id")
        ']' -> 
            case 0 == (extract (dmem s)) of                                                 -- If DP is nonzero we jump back to matching [
                True -> put s
                False -> modify $ updateImem $ jumpBack 0 . decrement
                    where
                        jumpBack :: Int -> Memory Instruction -> Memory Instruction
                        jumpBack 0 m@(Memory _ '[' _) = m                                   -- Recursively decrement PC but track what we pass
                        jumpBack n m@(Memory _ '[' _) = jumpBack (n-1) (decrement m)        --      increment counter for every ] and decrement for [
                        jumpBack n m@(Memory _ ']' _) = jumpBack (n+1) (decrement m)        --      When counter is 0 and we find a [ it's our match!
                        jumpBack n m = jumpBack n (decrement m)                             --      Then resume execution from new PC
        _ -> put s                                                                          -- Invalid instruction, dont modify state.
    modify $ updateImem increment                                                           -- Increment PC
    case memEnd $ imem $ s of
        True -> return ()                                                                   -- No more instructions, return unit
        False -> execInstruction                                                            -- Else, step again

printByte :: Word8 -> IO()
printByte w = putStrLn $ [toEnum (fromEnum w)] ++ "    " ++ show w
