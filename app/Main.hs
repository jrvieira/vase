module Main where

import Protolude
import Data.Map.Strict ( (!?), mapKeysWith, filterWithKey, elems, insert, update, adjust, keys, fromList )
import Zero

import Network.Simple.TCP
import Data.ByteString as BS ( toStrict )
import Data.ByteString.Char8 ( unpack )
import Data.Text ( pack )

import Data.Aeson

default ([], Word, Text)

data Σ = Σ
   { tile :: Map Coord [Vase]
   , nest :: Map Team [Word]  -- 0 is empty
   , turn :: Team
   , hand :: Map Team Hand
   , opts :: Opts
   , tell :: Maybe Text
   }

data Msg = Msg
   { msg_turn :: Team
   , msg_tile :: Map Coord [Vase]
   , msg_tell :: Maybe Text
   , msg_focus :: Maybe Focus
   , msg_picks :: Maybe Focus
   } deriving Generic

instance ToJSON Msg
instance ToJSON Hand
instance ToJSON Vase
instance ToJSON Focus

type Coord = (Word,Word)

data Vase = Vase
   { team :: Team
   , rank :: Word
   } deriving ( Eq, Generic )

instance Ord Vase where
   compare = on compare rank

type Team = Word

data Hand = Hand
   { focus :: Focus  -- current focus
   , lnest :: Word  -- last recorded focus on nest
   , ltile :: Coord  -- last recorded focus on tile
   , picks :: Maybe Focus  -- grabbed vase origin
   } deriving Generic

data Focus = Nest Word | Grid Coord
   deriving Generic

data Opts = Opts
   { teams :: Word
   , scale :: Word
   , piles :: Word
   }

data Dir = N | S | E | W | X

dir :: Text -> Maybe Dir
dir "N" = Just N
dir "S" = Just S
dir "E" = Just E
dir "W" = Just W
dir "X" = Just X
dir  _  = Nothing

data Act = Pick | Undo | Play

main :: IO ()
main = do
   c :: Word <- pure 2  -- number of teams
   s :: Word <- pure 4  -- size of square and number of vase ranks
   p :: Word <- pure 3  -- number of piles in each player's nest

   serve (Host "127.0.0.1") "8500" $ \ (so,_) -> loop so Σ
      { tile = fromList [ ((x,y),[]) | x <- genericTake s total , y <- genericTake s total ]
      , nest = fromList [ (n,genericReplicate p s) | n <- genericTake c total ]
      , turn = 0
      , hand = fromList [ (n,Hand { focus = Nest 0 , picks = Nothing , lnest = 0 , ltile = (prev $ div s 2,prev $ div s 2) }) | n <- genericTake c total ]
      , opts = Opts { teams = c , scale = s , piles = p }
      , tell = Nothing
      }

-- game allows 4 state altering actions
   -- move Dir
   -- pick
   -- undo
   -- play

loop :: Socket -> Σ -> IO ()
loop so st = do
   send so $ BS.toStrict $ encode $ Msg
      { msg_turn  = turn st
      , msg_tile  = tile st
      , msg_tell  = tell st
      , msg_focus  = focus <$> hand st !? turn st
      , msg_picks  = picks =<< hand st !? turn st
      }
   res <- recv so 128
   case res of
      Nothing -> print "connection closed"
      Just msg -> loop so $ case words $ pack $ unpack msg of
         ["move",x]
            | Just d  <- dir x -> move d
            | otherwise -> st { tell = Just $ unwords ["illegal move:",pack $ unpack msg] }
         ["pick"] -> pick
         ["undo"] -> undo
         ["play"] -> play
         _ -> st { tell = Just $ unwords ["illegal msg:",pack $ unpack msg] }
   where

   -- initial stack
   pile :: Team -> Word -> [Vase]
   pile s n = Vase s <$> [0..prev n]

   -- ACTIONS

   -- move hand
   move :: Dir -> Σ
   move d
      | Just h <- hand st !? turn st = st { hand = insert (turn st) (m h) (hand st) , tell = Nothing }
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
      | Just h <- hand st !? turn st = st { hand = insert (turn st) (p h) (hand st) , tell = Nothing }
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
      | Just h <- hand st !? turn st = st { hand = insert (turn st) (u h) (hand st) , tell = Nothing }
      | otherwise = st -- no undo
      where
      u :: Hand -> Hand
      u h = h { picks = Nothing }

   -- play vase if valid move
   play :: Σ
   play
      | Just h <- hand st !? turn st , Grid f <- focus h , Just p <- picks h , Nothing <- top (focus h) = st' f p
      | Just h <- hand st !? turn st , Grid f <- focus h , Just (Grid p) <- picks h , caps = st' f (Grid p)
      | Just h <- hand st !? turn st , Grid f <- focus h , Just (Nest p) <- picks h , caps , rescue = st' f (Nest p)
      | otherwise = st { tell = Just "no play" }
      where
      st' f p = st
         { tile = tile' f p
         , nest = nest' p
         , turn = mod (succ $ turn st) (teams $ opts st)
         , tell = Nothing
         }
      tile' f p
         | Grid c <- p = update (\x -> (: x) <$> top p) f . adjust (drop 1) c $ tile st
         | Nest _ <- p = update (\x -> (: x) <$> top p) f $ tile st
      nest' p
         | Grid _ <- p = nest st
         | Nest n <- p = adjust (nth n prev) (turn st) $ nest st

   --

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
