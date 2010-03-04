import Data.Maybe
import Network.URI
import Network.Browser
import Network.HTTP
import Text.JSON
import Control.Monad
import Control.Monad.State
import Data.List hiding (insert)
import System.Environment
import Control.Concurrent.Chan
import Control.Concurrent
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import System.IO

query_uri :: Int -> URI
query_uri id = fromJust . parseURI $ "http://www.facebook.com/ajax/typeahead_friends.php?u="++show id++"&__a=1"

friends id = browse' $ do 
  js <- fmap (drop (length "for (;;);") . rspBody . snd) . request . mkRequest GET . query_uri $ id
  return $ case fromJson `fmap` decode js of
             (Ok (Just o)) -> o
             _ -> []

friendsAndfriends id = do 
  c <- newChan
  enqueue <- mkQueue 15 (round $ 10^6 * 0.5)
  fs <- friends id
  mapM_ (\(i,_) -> enqueue (writeChan c . (,) i =<< friends i)) fs
  flip execStateT (Graph M.empty M.empty) $ do 
                    addFriends (id,fs)
                    mapM_ (\(i,_) -> addFriends =<< liftIO (readChan c)) (zip [0..] fs)

addFriends (i,fs) = insertData fs >> insertEdges i (map fst fs) 

data Graph a = Graph { labels :: !(IntMap a) , childrens :: !(IntMap [Int]) } deriving (Show,Read)

modify' f = do s <- get
               let s' = f s               
               s' `seq` put s'

insertData fs = modify' $ \g -> g { labels = foldl' (\m (n,l) -> M.insert n l m) (labels g) fs}
insertEdges n ns = modify' $ \g -> g { childrens = M.insert n ns (childrens g)}


browse' a = browse $ do
  setErrHandler $ const $ return ()
  setOutHandler $ const $ return ()
  setEventHandler Nothing
  a

field f (JSObject o) = lookup f $ fromJSObject o
field _ _ = Nothing
string (JSString s) = Just $ fromJSString s
string _ = Nothing
fb_id (JSRational _ r) = return . truncate $ r
fb_id _ = Nothing
fromJson x = do 
  JSArray xs <- field "friends" <=< field "payload" $ x
  mapM (liftM2 (liftM2 (,)) (fb_id <=< field "i") (string <=< field "t")) xs 


mkQueue :: Int -> Int -> IO (IO () -> IO ())
mkQueue n d = do 
  c <- newChan
  replicateM_ n . forkIO . forever $ do
    threadDelay d
    join $ readChan c
  return $ writeChan c

main = do 
  [id] <- getArgs
  g <- friendsAndfriends (read id)
  print g
