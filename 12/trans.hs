import Expr

data DefUseE = Def String Expr | Use String

traceMExpr :: [DefUseE] -> [(String, Int)]

