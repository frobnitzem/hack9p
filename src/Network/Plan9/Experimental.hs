-- | This wire specifies timeout behavior and arranges message queueing for carrying
-- out asynchronous operations within the context of a wire.
asyncWire :: Time -- Allotted time.
	  -> Wire (Maybe b) m (Maybe a) (Maybe b) -- Async operation.
	  -> (Maybe b, StrategyWire m a (Maybe b)) -- Response and wire to switch to on incomplete.
	  -> StrategyWire m a (Maybe b) -- Wire to switch to when complete.
	  -> StrategyWire m a (Maybe b)
asyncWire tout srv fail done = aW tout srv
    where aW t srv = mkGen $ \ dt ma -> do
	case ma of
	    Nothing -> return ((first Left) fail) -- timeout reached.
	    Just a -> do
		(emb, srv') <- stepWire srv dt a
		case emb of
			Left err -> return (Left err, aW 0 srv')
			Right mb -> case mb of
				Nothing -> return (Right Nothing, aW (t-dt) srv')
				Just b -> return (Left (Just b), done)

asyncQueue >>> (operation1 <|> err_handler) <?> openfidhandler <|> )


-- | encapsulating a monadic action
data TCont = Continue Time | TDone
asyncWire :: (Monad m)
          => Wire e m (Maybe a) (b, TCont)
          -> Wire e m b c
          -> Wire e m (Maybe a) c
asyncWire w1 w2 = asW w1 w2 0
    where asW w1 w2 t = mkGen $ \ dt ma -> do
	

startOp :: a -> STM() -- do

doneOp :: STM() -- check

endOp :: STM() -- kill
	


-- | The asyncWire is used to run the first operation in polling mode,
-- during which it is run whenever the continue time-delta passes.
-- After the wire returns TDone, it finishes producing and is removed
-- from the stream.
--
--   Use as in:
--   input >>> async_operation \>= process_result
(\>=) = asyncWire

