import Data.Word
import Control.Monad.State

type VM = StateT VMState IO
data VMState = VMState {dmem :: Memory Word8, imem :: Memory Instruction} deriving(Show)
type Instruction = Char

updateDmem :: (Memory Word8 -> Memory Word8) -> VMState -> VMState
updateDmem f (VMState d i) = VMState (f d) i

updateImem :: (Memory Instruction -> Memory Instruction) -> VMState -> VMState
updateImem f (VMState d i) = VMState d (f i)


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
