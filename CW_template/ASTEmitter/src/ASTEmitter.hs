{- 
Implement the following functions:
 
    ppStencilDef, 
    ppArgDecl,
    ppFSig, 
    ppLHSExpr, 
    ppRHSExpr 
    
Leave everything else as it is.
-}
module ASTEmitter (
    ppProgram ,
    ppBindings ,
    ppAST ,
    ppExprTup ,
    ppFSig ,
    ppArgDecl ,
    ppStencilDef ,
    ppMainTypeDecl ,
    ppMainArgDef ,
    ppMainReturnDef     
) where

import AST

import Data.List (intercalate)


ppProgram :: ASTInstance -> IO ()
ppProgram astInstance= let
        (instanceName,ast,functionSignaturesList,stencilDefinitionsList,mainArgDeclsList) = astInstance
        (mainArgDeclsInArgs,mainArgDeclsOutArgs) = mainArgDeclsList
        stencilDefs = map ppStencilDef stencilDefinitionsList
        inArgDecls = map ppArgDecl mainArgDeclsInArgs
        outArgDecls = map ppArgDecl mainArgDeclsOutArgs
        -- inArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsInArgs
        -- outArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsOutArgs
        functionDecls = map ppFSig functionSignaturesList
        mainTypeDecl = ppMainTypeDecl mainArgDeclsList
        mainArgDef = ppMainArgDef mainArgDeclsList
        mainReturnDef = ppMainReturnDef mainArgDeclsList
        mainExprs = map ("    "++) (ppAST ast)
        mainDef = [mainArgDef]++mainExprs++[mainReturnDef]
    in
        mapM_ putStrLn (
            ["-- "++instanceName++"\n"]++
            ["-- Stencil definitions"]++
            stencilDefs++
            ["\n-- Argument type declarations"]++
            ["---- Input arguments"]++
            inArgDecls++
            ["---- Output arguments"]++
            outArgDecls++
            ["\n-- Function type declarations"]++
            functionDecls++
            ["\n-- Main function type declaration"]++
            [mainTypeDecl]++
            ["\n-- Main function definition"]++
            mainDef
            )

ppBindings :: AST -> String
ppBindings = unlines . ppAST

ppAST :: AST -> [String]
ppAST = map ppExprTup 

ppExprTup :: (Expr, Expr) -> String
ppExprTup (lhs,rhs) = ppLHSExpr lhs ++ " = " ++ ppRHSExpr rhs

ppLHSExpr  :: Expr -> String
ppLHSExpr (Scalar _  _ name) = name
ppLHSExpr (Tuple exprs) = "(" ++ intercalate "," (map ppLHSExpr exprs) ++ ")"
ppLHSExpr (Vec _ expr) = ppLHSExpr expr
ppLHSExpr (SVec _ expr) = ppLHSExpr expr

ppRHSExpr :: Expr -> String
ppRHSExpr (Vec _ expr) = ppRHSExpr expr
ppRHSExpr (SVec _ expr) = ppRHSExpr expr
ppRHSExpr (FVec _ expr) = ppRHSExpr expr
ppRHSExpr (Scalar _ _ name) = name
ppRHSExpr (Function name []) = name
ppRHSExpr (Function name args) = name ++ " (" ++ intercalate "," (map ppRHSExpr args) ++ ")"
ppRHSExpr (ZipT exprs) = "zipt (" ++ intercalate "," (map ppRHSExpr exprs) ++ ")"

ppRHSExpr (Map (Function fName []) (ZipT exprs)) = "map " ++ fName ++ " (" ++ ppRHSExpr (ZipT exprs) ++ ")"
ppRHSExpr (Map (Function fName []) args) = "map " ++ fName ++ " " ++ ppRHSExpr args
ppRHSExpr (Map function (ZipT exprs)) = "map (" ++ ppRHSExpr function  ++ ") (" ++ ppRHSExpr (ZipT exprs) ++ ")"
ppRHSExpr (Map function args) = "map (" ++ ppRHSExpr function  ++ ") " ++ ppRHSExpr args

ppRHSExpr (UnzipT expr) = "unzipt (" ++ ppRHSExpr expr ++ ")" 

ppRHSExpr (Stencil firstExpr secondExpr) = "stencil " ++ ppRHSExpr firstExpr ++ " " ++ ppRHSExpr secondExpr
ppRHSExpr x = show x


-- Pretty-printer for the function signatures
ppFSig :: FunctionSignature -> String
ppFSig (functionName, args) = functionName ++ " :: " ++ intercalate " -> " (ppFArgDTypes args)

ppFArgDTypes :: [Expr] -> [String]
ppFArgDTypes [] = []
ppFArgDTypes (firstArg:remainingArgs) = 
    let firstArgDType = ppFArgDType firstArg
        remainingArgDTypes = ppFArgDTypes remainingArgs
    in
        if firstArgDType == "()" then ppFArgDTypes remainingArgs
        else firstArgDType : ppFArgDTypes remainingArgs

ppFArgDType :: Expr -> String
ppFArgDType (Scalar _ dataType _) = ppDType dataType
ppFArgDType (FVec content expr) = "FVec " ++ show content ++ ppFArgDType expr
ppFArgDType (SVec size expr) = "SVec " ++ show size ++ ppFArgDType expr
ppFArgDType (Tuple exprs) = "(" ++ intercalate "," (map ppFArgDType exprs) ++ ")"

-- Pretty-printer for the argument data types
ppDType :: DType -> String
ppDType DInteger = "Int"
ppDType DInt = "Int"
ppDType DReal = "Float"
ppDType DFloat = "Float"
ppDType (DSVec sz dt) = "SVec "++ show sz ++" "++ ppDType dt
ppDType (DVec sz dt) = "Vec "++ show sz ++" "++ ppDType dt
ppDType (DFVec dims dt) = "FVec "++ show dims ++" "++ ppDType dt
ppDType (DTuple dts) = "("++  intercalate ", " (map ppDType dts) ++")"
ppDType DDC = show DDC

ppArgDecl :: (String, DType) -> String
ppArgDecl (argName, argType) = argName ++ " :: " ++ ppDType argType

ppArgDeclType :: (String, DType) -> String
ppArgDeclType (_,argType) = ppDType argType

ppArgName  :: (String, DType) -> String
ppArgName (argName,_) = argName

ppArgs pp argDecls
    | length argDecls == 1 = pp (head argDecls)
    | otherwise = "("++ intercalate "," (map pp argDecls) ++")"

-- Pretty-printer for stencil definitions
ppStencilDef :: StencilDefinition -> String
ppStencilDef (name, values) = name ++ " = " ++ show values

ppMainTypeDecl :: ([(String,DType)],[(String,DType)]) -> String
ppMainTypeDecl mainArgDeclsList_ = let
        (mainArgDeclsInArgs,mainArgDeclsOutArgs) = mainArgDeclsList_
        inArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsInArgs
        outArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsOutArgs
    in
        "main :: " ++ inArgDeclTypes ++ " -> " ++ outArgDeclTypes

ppMainArgDef :: ([(String,DType)],[(String,DType)]) -> String
ppMainArgDef (mainArgDeclsInArgs,mainArgDeclsOutArgs) = "main " ++ ppArgs ppArgName mainArgDeclsInArgs++" = let "

ppMainReturnDef :: ([(String,DType)],[(String,DType)]) -> String
ppMainReturnDef (mainArgDeclsInArgs,mainArgDeclsOutArgs) = "  in\n      " ++ ppArgs ppArgName mainArgDeclsOutArgs