{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]
$ap = '

tokens :-

  $white+                    	 ;
  "#".*                      	 ;
  \(                         	 { \_ -> LeftP }
  \)                         	 { \_ -> RightP }
  \.                         	 { \_ -> DotT }
  \\                         	 { \_ -> LyambdaT }
  $alpha [$alpha $digit $ap]*    { \s -> Ident s }

{

data Token = LyambdaT
           | DotT
           | LeftP
           | RightP
           | Ident String
           deriving (Show, Eq)

}
