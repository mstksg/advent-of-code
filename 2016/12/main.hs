import Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Char
import Text.Parsec
import System.IO.Unsafe

newtype Register = Reg Int deriving (Show, Eq, Ord)
data Expr = ExpReg Register | ExpValue Int deriving (Show, Eq, Ord)
data ASM = Copy Expr Register
         | Inc Register
         | Dec Register
         | Jnz Expr Int
         deriving (Show, Eq, Ord)

type Program = Vector ASM

asm :: Parsec String () ASM
asm = copy <|> inc <|> dec <|> jnz
  where
    number = do neg <- optionMaybe (char '-')
                num <- read <$> many1 digit
                case neg of
                  Just _ -> return (-num)
                  Nothing -> return num
    register = do c <- oneOf "abcd"
                  return $ Reg (ord c - ord 'a')
    expr = ExpReg <$> register
           <|> ExpValue <$> number
    copy = Copy <$ string "cpy " <*> expr <* string " " <*> register
    inc = Inc <$ string "inc " <*> register
    dec = Dec <$ string "dec " <*> register
    jnz = Jnz <$ string "jnz " <*> expr <* string " " <*> number

prog :: String -> Vector ASM
prog text = case traverse (parse asm "") (lines text) of
              Right a -> V.fromList a
              Left e -> error (show e)

data Machine = Machine (Map Register Int) Int
             deriving (Show, Eq, Ord)

initial :: Machine
initial = Machine (Map.fromList [(Reg 0,0),(Reg 1,0),(Reg 2,0),(Reg 3,0)]) 0
initial2 = Machine (Map.fromList [(Reg 0,0),(Reg 1,0),(Reg 2,1),(Reg 3,0)]) 0

done :: Program -> Machine -> Bool
done program (Machine _ pc) = pc >= V.length program

step :: Program -> Machine -> Machine
step program (Machine registers pc) =
  case program V.! pc of
    Copy e r -> Machine (Map.insert r (runExpr e) registers) (pc + 1)
    Inc r -> Machine (Map.adjust (\n -> n + 1) r registers) (pc + 1)
    Dec r -> Machine (Map.adjust (\n -> n - 1) r registers) (pc + 1)
    Jnz e n -> Machine registers (pc + (if runExpr e /= 0 then
                                          n else 1))

  where
    runExpr (ExpValue n) = n
    runExpr (ExpReg reg) = registers Map.! reg

run :: Program -> Machine -> Machine
run program machine = if done program machine then
                        machine
                      else
                        run program (step program machine)

text :: IO String
text = readFile "input.txt"

p :: Program
p = unsafePerformIO (prog <$> text)
