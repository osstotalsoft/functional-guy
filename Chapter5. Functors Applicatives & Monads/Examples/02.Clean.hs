{-# LANGUAGE DuplicateRecordFields #-}

--domain
data LeasingDocument = LeasingDocument
  { documentId :: Int,
    siteId :: Int,
    scoring :: Maybe Double
  }
  deriving (Show)

newLeasingDocument :: Int -> Int -> LeasingDocument
newLeasingDocument documentId siteId = LeasingDocument documentId siteId Nothing

score :: Double -> LeasingDocument -> LeasingDocument
score scoring document = document {scoring = Just scoring}

--repo
load :: Int -> Int -> IO LeasingDocument
load documentId siteId = do
  putStrLn $ "loading document with documentId: " ++ show documentId ++ " and siteId: " ++ show siteId
  return $ LeasingDocument documentId siteId (Just 5.0)

save :: LeasingDocument -> IO ()
save document = do
  putStrLn $ "saving document " ++ show document
  return ()

--application
data Cmd = ScoreCmd
  { documentId :: Int,
    siteId :: Int,
    score' :: Double
  }

handleCmd :: Cmd -> IO ()
handleCmd (ScoreCmd documentId siteId scoring) =
  score' <$> load documentId siteId >>= save
  where
    score' = score scoring

data GetDocumentScoreQuery = ScoreQuery
  { documentId :: Int,
    siteId :: Int
  }

handleQ :: GetDocumentScoreQuery -> IO Double
handleQ (ScoreQuery documentId siteId) = getScore <$> load documentId siteId
  where
    getScore (LeasingDocument _ _ Nothing) = 0.0
    getScore (LeasingDocument _ _ (Just score)) = score

--api
x :: IO ()
x = handleCmd $ ScoreCmd 1 1 4.0

y :: IO Double
y = handleQ (ScoreQuery 1 1)
