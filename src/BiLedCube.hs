-- Defines an LED cube display.
module BiLedCube (
	Cube (..),
	Lyr (..),
	Grounds,
	Layer,
	succLayer,
	drawLayer,
	drawCube
) where
	import Control.Monad
	import Control.Monad.IO.Class
	import Data.Maybe
	import Data.Functor
	import Data.Traversable
	import Data.Foldable as F
	import Data.List
	import Data.Tuple.Homogenous
	import System.Hardware.Arduino
	import Game.TicTacToe3D.Vector3 (base)
	import ArduinoUtil (BiLed, biLedWrite)

	-- Represents a set of ground
	-- terminals of an LED cube.
	type Grounds = Tuple3 Pin

	-- Represents a set of positive
	-- terminals of an LED cube.
	type Layer = Tuple9 BiLed

	-- Represents all terminals of an LED cube.
	data Cube = Cube Grounds Layer

	-- [A, B, C] -> [(0, A), (1, B), (2, C)]
	indexed :: [a] -> [(Int, a)]
	indexed = zip [0..]

	-- Draws the given layer of an LED cube
	-- with the given function that specifies
	-- a color for each coordinates on the layer.
	drawLayer :: Layer -> ((Int, Int) -> Maybe Bool) -> Arduino ()
	drawLayer ls f = F.sequence_ $ do
		(yz, l) <- indexed $ toList ls
		let	(y:z:_) = base 3 yz
		return $ biLedWrite l $ f (y, z)

	-- Represents one of three layers in an LED cube.
	data Lyr = LA | LB | LC deriving (Eq, Enum, Bounded, Show)

	-- Retrieves the given layer's next layer.
	-- The last layer's next layer is the first layer.
	succLayer :: Lyr -> Lyr
	succLayer LC = LA
	succLayer l = succ l

	-- partitionI 2 [a, b, c, d] = Just (c, [a, b, d])
	-- partitionI 5 [a, b, c, d] = Nothing
	partitionI :: Int -> [a] -> Maybe (a, [a])
	partitionI i ns = format $ partition (i ==+) (indexed ns) where
		j ==+ k = j == fst k
		format ([], _) = Nothing
		format ([n], ns) = Just (snd n, snd <$> ns)

	-- Draws the specified layer in the given cube
	-- with the assigned function that specifies
	-- a color for each coordinates on the layer.
	drawCube :: Cube -> Lyr -> ((Int, Int, Int) -> Maybe Bool) -> Arduino ()
	drawCube (Cube gs ls) i f = do
		let	x = fromEnum i
			(g, rgs) = fromJust $ partitionI x $ toList gs
		F.sequence_ $ flip digitalWrite True <$> rgs
		drawLayer ls $ \(y, z) -> f (x, y, z)
		digitalWrite g False
