import Control.Monad
import Data.IORef
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import qualified Data.Map.Strict as Map

data Lambda =
	Abstract String Lambda
	| Expr [Lambda]
	| Variable String
 	| Alias String Lambda
instance Show Lambda where show = showLambda

type Dictionary = Map.Map String Lambda

showLambda :: Lambda -> String
showLambda (Variable x) = x
showLambda (Expr xs) = "(" ++ (unwords . map show $ xs) ++ ")"
showLambda a@(Abstract _ _) = "(\\" ++ unwords binds ++ " . " ++ show app ++ ")"
	where	(binds, app) = unfold a []
       		unfold (Abstract b l) s = unfold l (b:s)
	 	unfold other s = (reverse s, other)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany space

parseToken :: Parser String
parseToken = do
	spaces
	token <- many1 (letter <|> digit <|> symbol)
	spaces
	return token

parseVariable :: Parser Lambda
parseVariable = do
	token <- parseToken
 	return $ Variable token

parseAbstract :: Parser Lambda
parseAbstract = do
	char '\\'
 	bindings <- many parseToken
  	char '.'
   	app <- parseExpr
    	return $ foldr (Abstract) app bindings

parseWrappedExpr :: Parser Lambda
parseWrappedExpr = do
	char '('
	x <- parseExpr
 	char ')'
  	return x

parseExpr :: Parser Lambda
parseExpr = do
	exprs <- many $ (spaces >> 
	  	(parseWrappedExpr <|> parseAbstract <|> parseVariable))
 	return $ case exprs of
		[expr] -> expr
  		other -> Expr other

parseAlias :: Parser Lambda
parseAlias = do
	alias <- parseToken
 	char '='
  	app <- parseExpr
   	return $ Alias alias app

readExpr :: String -> Lambda
readExpr input = case parse (parseAlias <|> parseExpr) "lambdumb" input of
	Left err -> Variable $ show err
 	Right val -> val

replaceVar :: String -> Lambda -> Lambda -> Lambda
replaceVar var val this@(Abstract bind l)
	| var == bind = this
 	| otherwise = Abstract bind (replaceVar var val l)
replaceVar var val (Expr xs) = Expr $ map (replaceVar var val) xs
replaceVar var val this@(Variable s)
	| var == s = val
 	| otherwise = this

funcApply :: Lambda -> Lambda -> Lambda
funcApply (Abstract bind l) p = replaceVar bind p l

eval :: Dictionary -> Lambda -> (Lambda, Dictionary)
eval dict (Alias s l) = eval dict' l'
	where 	l' = fst $ eval dict l
       		dict' = Map.insert s l' dict
eval dict (Expr (f@(Abstract _ _) : v : xs)) = (eval dict) . Expr $ (funcApply f v):xs
eval dict (Expr [expr]) = eval dict expr
eval dict (Expr (expr:es)) = case expr of
       	v@(Variable n) -> case Map.lookup n dict of
		Nothing -> flip (,) dict $ Expr . (v:) $ map (fst . eval dict) es
	  	Just l -> eval dict . Expr $ (l:es)
 	_ -> (eval dict) $ Expr ((fst $ eval dict expr) : es)
eval dict anything = (anything, dict)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Dictionary -> String -> IO (Lambda, Dictionary)
evalString env expr = return $ eval env (readExpr expr)

evalAndPrint :: Dictionary -> String -> IO Dictionary
evalAndPrint dict expr = do
	(expr', dict') <- evalString dict expr
 	(putStrLn . show) expr'
  	return dict'

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
	result <- prompt
 	if pred result
     		then return ()
       		else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = evalAndPrint Map.empty expr >> return ()

runRepl :: Dictionary -> IO ()
runRepl dict = do
	result <- readPrompt "dumb> "
 	if (result == "quit")
     		then return ()
       		else do
		 	dict' <- evalAndPrint dict result
    			runRepl dict'

main :: IO ()
main = do
	args <- getArgs
	case length args of
		0 -> runRepl Map.empty
		1 -> runOne $ args !! 0
		otherwise -> putStrLn "Too many arguments"  

