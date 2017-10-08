{-# LANGUAGE RankNTypes #-}


type Box a = forall b. (a -> b) -> b

-- Put the a in the box.
box :: a -> Box a
box a f = f a

-- Get the a out of the box.
unbox :: Box a -> a
unbox f = f id


rebox :: Box a -> Box a
rebox x = box $ unbox x
--rebox = box . unbox
