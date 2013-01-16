
type Strategy = Wire String m NinePkt NinePkt
clientWire :: (Monad m) => Wire String m Strategy NinePkt
clientWire = clientUpdState Set.empty
	where clientUpdState st = updateStrategies . (newSt &&& (runStrategies . fifo))

