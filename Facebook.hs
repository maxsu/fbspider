import Data.Maybe
import Network.URI
import Network.Browser
import Network.HTTP
import Text.JSON
import Text.Printf
import Control.Monad
import Control.Monad.State
import Data.List hiding (insert)
import System.Environment
import Control.Concurrent.Chan
import Control.Concurrent
import Data.IntMap (IntMap, empty, insert, size, (!))
import System.IO
import Numeric

queryUri :: Int -> URI
queryUri uid = fromJust . parseURI $ "http://www.facebook.com/ajax/typeahead_friends.php?u="++show uid++"&__a=1"

friends :: Int -> IO [(Int, String)]
friends uid = browse' $ do 
  js <- fmap (drop (length "for (;;);") . rspBody . snd) . request . mkRequest GET . queryUri $ uid
  return $ case fromJson `fmap` decode js of
             (Ok (Just o)) -> o
             _             -> []

friendsAndFriends :: Int -> IO (Graph String)
friendsAndFriends uid = do 
  c        <- newChan
  enqueue  <- mkQueue 15 500000
  fs       <- friends uid
  mapM_ (\(i,_) -> enqueue (writeChan c . (,) i =<< friends i)) fs
  flip execStateT (Graph empty empty) $ do 
                    addFriends (uid,fs)
                    mapM_ (\(i,_) -> addFriends =<< liftIO (readChan c)) (zip [0..] fs)

addFriends (i,fs) = insertData fs >> insertEdges i (map fst fs) 

data Graph a = Graph { labels :: !(IntMap a) , children :: !(IntMap [Int]) } deriving (Show,Read)

modify' f = do s <- get
               let s' = f s               
               s' `seq` put s'

insertData fs = modify' $ \g -> g { labels = foldl' (\m (n,l) -> insert n l m) (labels g) fs}

insertEdges n ns = modify' $ \g -> g { children = insert n ns (children g)}

browse' a = browse $ do
  setErrHandler $ const $ return ()
  setOutHandler $ const $ return ()
  setEventHandler Nothing
  setEventHandler Nothing
  setEventHandler Nothing
  setEventHandler Nothing
  a

field f (JSObject o) = lookup f $ fromJSObject o
field _ _ = Nothing

string (JSString s) = Just $ fromJSString s
string _ = Nothing

fbId (JSRational _ r) = return . truncate $ r
fbId _ = Nothing

fromJson x = do 
  JSArray xs <- field "friends" <=< field "payload" $ x
  mapM (liftM2 (liftM2 (,)) (fbId <=< field "i") (string <=< field "t")) xs 


mkQueue :: Int -> Int -> IO (IO () -> IO ())
mkQueue n d = do 
  c <- newChan
  replicateM_ n . forkIO . forever $ do
    threadDelay d
    join $ readChan c
  return $ writeChan c

main = do 
  [idStr] <- getArgs
  let uid = read idStr
  g <- friendsAndFriends uid
  putStrLn "Data:"
  print g
  putStrLn "Summary:"
  let friends = (children g ! uid)
      nfriends = length friends
  putStrLn $ labels g ! uid ++ " has " ++ show nfriends ++ " friends."
  let average = (fromIntegral (sum . map (length . (children g !)) $ friends) / fromIntegral nfriends)
  putStrLn $ "Which have an average of " ++ showGFloat (Just 2) average "" ++ " friends."
  putStrLn $ "For a total of " ++ show (size (labels g)) ++ " distinct users."

