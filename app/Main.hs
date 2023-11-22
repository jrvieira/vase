module Main where

import Relude
import Data.Map.Strict ( (!?), mapKeysWith, filterWithKey, elems, insert, update, adjust, keys )
import Zero

default ([], Word, Text)

data Σ = Σ
   { tile :: Map Coord [Vase]
   , nest :: Map Team [Word]  -- 0 is empty
   , turn :: Team
   , hand :: Map Team Hand
   , opts :: Opts
   , tell :: Maybe Text
   }

type Coord = (Word,Word)

data Vase = Vase
   { team :: Team
   , rank :: Word
   } deriving Eq

instance Ord Vase where
   compare = on compare rank

type Team = Word

data Hand = Hand
   { focus :: Focus  -- current focus
   , lnest :: Word  -- last recorded focus on nest
   , ltile :: Coord  -- last recorded focus on tile
   , picks :: Maybe Focus  -- grabbed vase origin
   }

data Focus = Nest Word | Grid Coord

data Opts = Opts
   { teams :: Word
   , scale :: Word
   , piles :: Word
   }

data Dir = N | S | E | W | X
data Act = Pick | Undo | Play

main :: IO ()
main = do
   c :: Word <- pure 2  -- number of teams
   s :: Word <- pure 4  -- size of square and number of vase ranks
   p :: Word <- pure 3  -- number of piles in each player's nest
   loop Σ
      { tile = fromList $ [ ((x,y),[]) | x <- genericTake s total , y <- genericTake s total ]
      , nest = fromList $ [ (n,genericReplicate p s) | n <- genericTake c total ]
      , turn = 0
      , hand = fromList $ [ (n,Hand { focus = Nest 0 , picks = Nothing , lnest = 0 , ltile = (prev $ div s 2,prev $ div s 2) }) | n <- genericTake c total ]
      , opts = Opts { teams = c , scale = s , piles = p }
      , tell = Nothing
      }

-- IPC?
loop :: Σ -> IO ()
loop st = do
   -- wait for input
   -- update state
   -- send message
   -- loop st
   pure ()

   where

   -- initial stack
   pile :: Team -> Word -> [Vase]
   pile s n = Vase s <$> [0..prev n]

   -- move hand
   move :: Dir -> Σ
   move d
      | Just h <- hand st !? turn st = st { hand = insert (turn st) (m h) (hand st) }
      | otherwise = st { tell = Just "error: no move" }
      where
      m :: Hand -> Hand
      m h
         | Nest n     <- focus h , E <- d = h { focus = Nest $ min (next n) (piles $ opts st) }
         | Nest n     <- focus h , W <- d = h { focus = Nest $ prev n }

         | Grid (x,y) <- focus h , N <- d = h { focus = Grid (min (next x) (prev $ scale $ opts st),y) }
         | Grid (x,y) <- focus h , S <- d = h { focus = Grid (prev x,y) }
         | Grid (x,y) <- focus h , E <- d = h { focus = Grid (x,min (next y) (prev $ scale $ opts st)) }
         | Grid (x,y) <- focus h , W <- d = h { focus = Grid (x,prev y) }

         | Nest n     <- focus h , X <- d = h { focus = Grid $ ltile h , lnest = n }
         | Grid c     <- focus h , X <- d = h { focus = Nest $ lnest h , ltile = c }

         | otherwise = h  -- no move

   -- pick vase to play
   pick :: Σ
   pick
      | Just h <- hand st !? turn st = st { hand = insert (turn st) (p h) (hand st) }
      | otherwise = st { tell = Just "error: no pick" }
      where
      p :: Hand -> Hand
      p h
         | Nest n <- focus h , Just (v:_) <- genericDrop n <$> nest st !? turn st , v > 0 = h { picks = Just (focus h) }
         | Grid c <- focus h , Just (v:_) <- tile st !? c , team v == turn st = h { picks = Just (focus h) }
         | otherwise = h  -- no pick

   -- drop grabbed vase
   undo :: Σ
   undo
      | Just h <- hand st !? turn st = st { hand = insert (turn st) (u h) (hand st) }
      | otherwise = st -- no undo
      where
      u :: Hand -> Hand
      u h = h { picks = Nothing }

   -- play vase if valid move
   play :: Σ
   play
      | Just h <- hand st !? turn st , Grid c <- focus h , Nothing <- top (focus h) = st' (focus h) c
      | Just h <- hand st !? turn st , Grid c <- focus h , Just (Grid _) <- picks h , caps = st' (focus h) c
      | Just h <- hand st !? turn st , Grid c <- focus h , Just (Nest _) <- picks h , caps , rescue = st' (focus h) c
      | otherwise = st  -- no play
      where
      st' f c = st
         { tile = tile' f c
         , nest = nest' f
         , turn = mod (succ $ turn st) (teams $ opts st)
         }
      tile' o t
         | Grid c <- o = update (\x -> (: x) <$> top o) t . adjust (drop 1) c $ tile st
         | Nest _ <- o = update (\x -> (: x) <$> top o) t $ tile st
      nest' o
         | Grid _ <- o = nest st
         | Nest n <- o = adjust (nth n prev) (turn st) $ nest st

   -- can the vase be placed
   caps :: Bool
   caps
      | Just h <- hand st !? turn st , Nothing <- top (focus h) = True
      | Just h <- hand st !? turn st , Just GT <- compare <$> (top =<< picks h) <*> top (focus h) = True
      | otherwise = False

   -- check if rescue move is allowed
   rescue :: Bool
   rescue
      | Just h <- hand st !? turn st , f <- focus h , Grid (x,y) <- f , Just v <- top f , team v /= turn st = pred (scale $ opts st) ∈
         [ genericLength $ filter (\k@(kx,__) -> and @[] [x == kx,(team <$> top (Grid k)) == Just (team v)]) (keys $ tile st)
         , genericLength $ filter (\k@(__,ky) -> and @[] [y == ky,(team <$> top (Grid k)) == Just (team v)]) (keys $ tile st)
         , genericLength $ filter (\k@(kx,ky) -> and @[] [x == y,kx == ky,(team <$> top (Grid k)) == Just (team v)]) (keys $ tile st)
         , genericLength $ filter (\k@(kx,ky) -> and @[] [x == prev (scale (opts st) - y),kx == prev (scale (opts st) - ky),(team <$> top (Grid k)) == Just (team v)]) (keys $ tile st)
         ]
      | otherwise = False

   -- adjust nth element of list
   nth :: Word -> (a -> a) -> [a] -> [a]
   nth _ _ [] = []
   nth 0 f (x:xs) = f x : xs
   nth n f (x:xs) = x : nth (prev n) f xs

   -- get top vase
   top :: Focus -> Maybe Vase
   top f
      | Grid c <- f , Just (v:_) <- tile st !? c = Just v
      | Nest n <- f , Just (v:_) <- genericDrop n <$> nest st !? turn st , v > 0 = Just $ Vase { team = turn st , rank = v }
      | otherwise = Nothing

   -- top vase is own
   own :: Focus -> Bool
   own f = (team <$> top f) == Just (turn st)

   -- check if game is over
   win :: Maybe Team
   win
      | x:_ <- mapMaybe line (elems rows <> elems cols <> pure posd <> pure negd) = Just x
      | otherwise = Nothing
      where
      rows :: Map Word [Team]
      rows = map team . take 1 <$> mapKeysWith (<>) fst (tile st)
      cols :: Map Word [Team]
      cols = map team . take 1 <$> mapKeysWith (<>) snd (tile st)
      posd :: [Team]
      posd = join . elems $ map team . take 1 <$> filterWithKey (\(x,y) _ -> x == y) (tile st)
      negd :: [Team]
      negd = join . elems $ map team . take 1 <$> filterWithKey (\(x,y) _ -> x == prev (scale (opts st) - y)) (tile st)
      line :: [Team] -> Maybe Team
      line [] = Nothing
      line (x:xs)
         | all (x ==) xs = Just x
         | otherwise = Nothing
