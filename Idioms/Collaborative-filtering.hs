-- Collaborative filtering
-- ref: https://wiki.haskell.org/Collaborative_filtering
-- origin: http://www.serpentine.com/blog/2007/08/27/weighted-slope-one-in-haskell-collaborative-filtering-in-29-lines-of-code

import Data.List (foldl',foldl1')
import qualified Data.Map as M
 
-- The item type is a polymorphic parameter.  Since it goes into a Map
-- it must be able to be compared, so item must be an instance of Ord.
type Count = Int
type RatingValue = Double
-- The Rating is the known (item,Rating) information for a particular "user"
type Rating item = M.Map item RatingValue
 
-- The SlopeOne matrix is indexed by pairs of items and is implemented
-- as a sparse map of maps.
newtype SlopeOne item = SlopeOne (M.Map item (M.Map item (Count,RatingValue)))
  deriving (Show)
 
-- The SlopeOne' matrix is an un-normalized version of SlopeOne
newtype SlopeOne' item = SlopeOne' (M.Map item (M.Map item (Count,RatingValue)))
  deriving (Show)
 
empty = SlopeOne M.empty
empty' = SlopeOne' M.empty
 
-- This performs a strict addition on pairs made of two nuumeric types
addT (a,b) (c,d) = let (l,r) = (a+c, b+d) in l `seq` r `seq` (l, r)
 
 
-- There is never an entry for the "diagonal" elements with equal
-- items in the pair: (foo,foo) is never in the SlopeOne.
update :: Ord item => SlopeOne item -> [Rating item] -> SlopeOne item
update (SlopeOne matrixInNormed) usersRatings =
    SlopeOne . M.map (M.map norm) . foldl' update' matrixIn $ usersRatings
  where update' oldMatrix userRatings =
          foldl' (\oldMatrix (itemPair, rating) -> insert oldMatrix itemPair rating)
                 oldMatrix itemCombos
          where itemCombos = [ ((item1, item2), (1, rating1 - rating2)) 
                             | (item1, rating1) <- ratings
                             , (item2, rating2) <- ratings
                             , item1 /= item2]
                ratings = M.toList userRatings
        insert outerMap (item1, item2) newRating = M.insertWith' outer item1 newOuterEntry outerMap
          where newOuterEntry = M.singleton item2 newRating
                outer _ innerMap = M.insertWith' addT item2 newRating innerMap
        norm (count,total_rating) = (count, total_rating / fromIntegral count)
        un_norm (count,rating) = (count, rating * fromIntegral count)
        matrixIn = M.map (M.map un_norm) matrixInNormed
 
 
-- This version of update2 makes an unnormalize slopeOne' from each
-- Rating and combines them using Map.union* operations and addT.
update2 :: Ord item => SlopeOne' item -> [Rating item] -> SlopeOne' item
update2 s@(SlopeOne' matrixIn) usersRatingsIn | null usersRatings = s
                                              | otherwise =
    SlopeOne' . M.unionsWith (M.unionWith addT) . (matrixIn:) . map fromRating $ usersRatings
  where usersRatings = filter ((1<) . M.size) usersRatingsIn
        fromRating userRating = M.mapWithKey expand1 userRating
          where expand1 item1 rating1 = M.mapMaybeWithKey expand2 userRating
                  where expand2 item2 rating2 | item1 == item2 = Nothing
                                              | otherwise = Just (1,rating1 - rating2)
 
predict :: Ord a => SlopeOne a -> Rating a -> Rating a
predict (SlopeOne matrixIn) userRatings =
  let freqM = foldl' insert M.empty
                     [ (item1,found_rating,user_rating)
                     | (item1,innerMap) <- M.assocs matrixIn
                     , M.notMember item1 userRatings
                     , (user_item, user_rating) <- M.toList userRatings
                     , item1 /= user_item
                     , found_rating <- M.lookup user_item innerMap
                     ]
      insert oldM (item1,found_rating,user_rating) =
        let (count,norm_rating) = found_rating
            total_rating = fromIntegral count * (norm_rating + user_rating)
        in M.insertWith' addT item1 (count,total_rating) oldM
      normM = M.map (\(count, total_rating) -> total_rating / fromIntegral count) freqM
  in M.filter (\norm_rating -> norm_rating > 0) normM
 
-- This is a modified version of predict.  It also expect the
-- unnormalized SlopeOne' but this is a small detail
predict' :: Ord a => SlopeOne' a -> Rating a -> Rating a
predict' (SlopeOne' matrixIn) userRatings =
    M.mapMaybe calcItem (M.difference matrixIn userRatings)
  where calcItem innerMap | M.null combined = Nothing
                          | norm_rating <= 0 = Nothing
                          | otherwise = Just norm_rating
          where combined = M.intersectionWith weight innerMap userRatings
                (total_count,total_rating) = foldl1' addT (M.elems combined)
                norm_rating = total_rating / fromIntegral total_count
        weight (count,rating) user_rating =
          (count,rating + fromIntegral count *  user_rating)
 
userData :: [Rating String]
userData = map M.fromList [
 [("squid", 1.0), ("cuttlefish", 0.5), ("octopus", 0.2)],
 [("squid", 1.0), ("octopus", 0.5), ("nautilus", 0.2)],
 [("squid", 0.2), ("octopus", 1.0), ("cuttlefish", 0.4), ("nautilus", 0.4)],
 [("cuttlefish", 0.9), ("octopus", 0.4), ("nautilus", 0.5)]
 ]
 
userInfo = M.fromList [("squid", 0.4),("cuttlefish",0.9),("dolphin",1.0)]
 
predictions = predict (update empty userData) userInfo
 
predictions' = predict' (update2 empty' userData) userInfo

-- Optimize
-- The changes to SlopeOne/update/predict below use a different internal data structure for storing the sparse matrix of SlopeOne. Instead of a Map of Map design it uses a Map of List design and keeps the List in distinct ascending form. The list values are a strict (data Tup) type which should help save space compared to the previous inner Map design and yet efficiently provide all the operations needed by update and predict.

-- The SlopeOne matrix is indexed by pairs of items and is implemented
-- as a sparse map of distinct ascending lists.  The 'update' and
-- 'predict' functions do not need the inner type to actually be a
-- map, so the list saves space and complexity.
newtype SlopeOne item = SlopeOne (M.Map item [Tup item])
  deriving (Show)
 
-- Strict triple tuple type for SlopeOne internals
data Tup item = Tup { itemT :: !item, countT :: !Count, ratingT :: !RatingValue }
  deriving (Show)
 
empty :: SlopeOne item
empty = SlopeOne M.empty
 
update :: Ord item => SlopeOne item -> [Rating item] -> SlopeOne item
update s@(SlopeOne matrixIn) usersRatingsIn | null usersRatings = s
                                            | otherwise =
    SlopeOne . M.unionsWith mergeAdd . (matrixIn:) . map fromRating $ usersRatings
  where usersRatings = filter ((1<) . M.size) usersRatingsIn
        -- fromRating converts a Rating into a Map of Lists, a singleton SlopeOne.
        fromRating userRatings = M.mapWithKey expand userRatings
          where expand item1 rating1 = map makeTup . M.toAscList . M.delete item1 $ userRatings
                  where makeTup (item2,rating2) = Tup item2 1 (rating1-rating2)
 
-- 'mergeAdd' is a helper for 'update'.
-- Optimized traversal of distinct ascending lists to perform additive merge.
mergeAdd :: Ord item => [Tup item] -> [Tup item] -> [Tup item]
mergeAdd !xa@(x:xs) !ya@(y:ys) =
  case compare (itemT x) (itemT y) of
    LT -> x : mergeAdd xs ya
    GT -> y : mergeAdd xa ys
    EQ -> Tup (itemT x) (countT x + countT y) (ratingT x + ratingT y) : mergeAdd xs ys
mergeAdd xs [] = xs
mergeAdd [] ys = ys
 
-- The output Rating has no items in common with the input Rating and
-- only includes positively weighted ratings.
predict :: Ord item => SlopeOne item -> Rating item -> Rating item
predict (SlopeOne matrixIn) userRatings =
    M.mapMaybe (computeRating ratingList) (M.difference matrixIn userRatings)
  where ratingList = M.toAscList userRatings
 
-- 'computeRating' is a helper for 'predict'.
-- Optimized traversal of distinct ascending lists to compute positive weighted rating.
computeRating :: (Ord item) => [(item,RatingValue)] -> [Tup item] -> Maybe RatingValue
computeRating !xa@(x:xs) !ya@(y:ys) =
  case compare (fst x) (itemT y) of
    LT -> computeRating xs ya
    GT -> computeRating xa ys
    EQ -> helper (countT y) (ratingT y + fromIntegral (countT y) * snd x) xs ys
 where
  helper :: (Ord item) => Count -> RatingValue -> [(item,RatingValue)] -> [Tup item] -> Maybe RatingValue
  helper !count !rating !xa@(x:xs) !ya@(y:ys) =
    case compare (fst x) (itemT y) of
      LT -> helper count rating xs ya
      GT -> helper count rating xa ys
      EQ -> helper (count + countT y) (rating + ratingT y + fromIntegral (countT y) * (snd x)) xs ys
  helper !count !rating _ _  | rating > 0 = Just (rating / fromIntegral count)
                             | otherwise = Nothing
computeRating _ _ = Nothing