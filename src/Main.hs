{-# LANGUAGE TypeOperators #-}

module Main 
  where

data Fix f = In { out :: f (Fix f) }

data PrimLogoF r = FD Int r | RT Int r | End
data SugarLogoF r = Repeat Int r

instance Functor PrimLogoF where
    fmap f (FD x r) = FD x $ f r
    fmap f (RT x r) = RT x $ f r
    fmap _ End = End

instance Functor SugarLogoF where
    fmap f (Repeat n r) = Repeat n $ f r

type PrimLogo = Fix PrimLogoF
type SugarLogo = Fix PrimLogoF

data (f :+: g) e = InL (f e) | InR (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap fn (InL f) = InL $ fmap fn f
    fmap fn (InR g) = InR $ fmap fn g

type LogoF = PrimLogoF :+: SugarLogoF
type Logo = Fix LogoF

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . out

{-

unroll :: Logo -> PrimLogo
unroll (In (InL (FD x is))) = In $ FD x (unroll is)
unroll (In (InL (RT x is))) = In $ RT x (unroll is)
unroll (In (InL (End)))     = In $ End
unroll (In (InR (Repeat n is))) = In $ End

-}

unroll :: LogoF PrimLogo -> PrimLogo                     
unroll (InL x) = In x
unroll (InR _) = In $ End

render :: Algebra PrimLogoF String
render (FD x tail) = "FD " ++ show x ++ " " ++ tail
render (RT x tail) = "RT " ++ show x ++ " " ++ tail
render (End) = "End"

-- Smart Constructors
fd :: Int -> Logo -> Logo
fd dist tail = (In . InL) $ FD dist tail

rt :: Int -> Logo -> Logo
rt ang tail = (In . InL) $ RT ang tail

end :: Logo
end = In . InL $ End

prog :: Logo
prog = fd 10 (rt 90 end)

x :: PrimLogo
x = cata unroll prog 

main :: IO ()
main = putStrLn $ cata render x



