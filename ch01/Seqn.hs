seqn :: Monad m => [m a] -> m [t]
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return []

