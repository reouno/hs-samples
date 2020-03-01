module SequenceExample where

doExample :: IO ()
doExample = do
    print $ sequence [Just 1, Just 2, Just 3]
    print $ sequence [Just 1, Nothing, Just 3]

    print =<< sequence [print "hello", print "world"]
    print =<< sequence_ [print "hello", print "world"]

