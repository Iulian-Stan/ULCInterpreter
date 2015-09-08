--usage interpretProgram "file_name"
module Inter2 where

import Parser
import Data.Char
import Data.List

-- Defenition of expression data and types
type X = String
data Exp = Var X
			| Lam X Exp
			| App Exp Exp
			| Clo Exp Gam
			| Lamc X Exp Gam
			deriving(Eq, Show)
		
-- Defenition of context and closures
data Gam = Nil
			| Cont X Exp Gam
			deriving(Eq, Show)

-- Trim a string
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
		
-- Parsing functions	
extractString :: X -> (X, X)								-- Parse a string
extractString = head . spotWhile0 isLetter

extractVar :: X -> (Exp, X)									-- Parse a variable
extractVar prog = (Var x, rest) 
					where
					(x, rest) = extractString prog
					
extractLam :: X -> (Exp, X)								 	-- Parse a functions
extractLam prog = (Lam x e, rest)
					where
					(x, string) = extractString prog
					(e, rest) = extractExp $ tail string
					
extractApp :: X -> (Exp, X)									-- Parse a application
extractApp prog = (App e1 e2, tail rest) where
					(e1, string) = extractExp prog
					(e2, rest) = extractExp $ tail string

extractExp :: X -> (Exp, X)									-- Parse a expression
extractExp (h : t)
				| h == '\\' = extractLam t
				| h == '(' = extractApp t
				| h == '=' = extractExp t
				| otherwise = extractVar (h : t)
extractExp "" = (Var "", "") 

extract :: [X] -> [(X, Exp)]								-- General Parsing
extract (line : lines) =
						case rest of
							"" -> case string of
								"" -> [("", Var x)] ++ (extract lines)
								_ -> [(x, e)] ++ (extract lines)
							_ -> error "Parsing error"
						where
							prog = trim line
							(x, string) = extractString prog
							(e, rest) = extractExp string
extract _ = []		

-- Evaluation funtions
eval :: [(X, Exp)] -> [Exp]									-- Evaluation level1
eval lista = evaluate lista Nil

evaluate :: [(X, Exp)] -> Gam -> [Exp]						-- Evaluation level2
evaluate (h : t) g 
 	| fst h /= "" = e : evaluate t (Cont (fst h) (snd h) g)  
	| snd h == Var "" = [Clo (snd h) Nil] ++ evaluate t g
	| otherwise = e : evaluate t g 
	where
		(e,r) = evaluateExp (snd h) Nil g []
evaluate _ g = []

evaluateExp :: Exp -> Gam -> Gam -> [Char] -> (Exp, [Char])	-- Evaluation level3

evaluateExp (Var x) lg gg r 								-- EvalVar
	| Var x == e = (e, r ++ "EV! ")
	| otherwise = (e, r ++ "EV ") 							-- evaluateExp e lg gg
	where
		e  = evalExp x lg gg

evaluateExp (Lam x e) lg gg r = 							-- Close
	evaluateExp (Lamc x e lg) Nil gg (r ++ "Cl ")

evaluateExp (App (Lamc x e1 g) e) lg gg r = 				-- Reduce
	evaluateExp (Clo e1 (Cont x (Clo e lg) g)) Nil gg (r ++ "R ")     

evaluateExp (App e1 e2) lg gg r								-- EvalApp
	| e == e1 = (App e1 e2, r ++ "EA! ")
	| otherwise = evaluateExp (App e e2) lg gg (r1 ++ "EA ")
	where
		(e, r1) = evaluateExp e1 lg gg r	

evaluateExp (Clo e@(Clo _ _) g) lg gg r	=
	evaluateExp e lg gg (r ++ "ST1 ")
evaluateExp (Clo e@(Lamc _ _ _) g) lg gg r =
	evaluateExp e lg gg (r ++ "ST2 ")

evaluateExp (Clo e g) lg gg r
	| e1 == e = (Clo e g, r ++ "EC! ")
	| otherwise = evaluateExp (Clo e1 lg) Nil gg (r1 ++ "EC ") 
	where
		(e1, r1) = evaluateExp e g gg r

evaluateExp e lg gg r = (e, r ++ "? ")

		
evalExp :: X -> Gam -> Gam -> Exp			-- Substitution
evalExp x lg gg 
				| ls /= [] = head ls
				| gs /= [] = head gs
				| otherwise = Var x
				where
					ls = findBind x lg
					gs = findBind x gg

findBind :: X -> Gam -> [Exp]				-- Substitution find
findBind _ Nil = []
findBind t (Cont x e g)
					|  t == x = [e]
					| otherwise = findBind t g

-- Printing funtions
showExp (Var x) = x
showExp (Lam x e) = "\\" ++ x ++ "." ++ showExp e
showExp (App e1 e2) = "(" ++ showExp e1 ++ " " ++ showExp e2 ++ ")"	

showExp (Clo (Var x) g) 
					| g == Nil = x
					| otherwise = "<" ++ x ++ "; " ++ showGam g ++ ">"
showExp (Clo e g) = "<" ++ showExp e ++ "; " ++ showGam g ++ ">"
showExp (Lamc x e g) = "<\\"  ++ x ++ "." ++ showExp e ++ "; " ++ showGam g ++ ">"

showGam Nil = ""
showGam (Cont x c g) 
				| g == Nil = x ++ "<-" ++ showExp c
				| otherwise = x ++ "<-" ++ showExp c ++ ", " ++ showGam g

crunch :: X -> X
crunch = unlines . map showExp . eval . extract . lines

interpretProgram file = (readFile file) >>= putStr . crunch