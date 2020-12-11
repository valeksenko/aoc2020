module D11P2 (
    occupied
) where

import D11
import Data.List
import Data.Maybe
import qualified Data.Map as M

occupied :: AreaMap -> Int
occupied amap = count . fst $ until repeated simulate (M.empty, amap)
    where
        repeated (m, m') = m == m'
        simulate (_, m') = (m', simulateStep m')
        count = M.size . M.filter ((==) Occupied)

simulateStep :: AreaMap -> AreaMap
simulateStep amap = M.foldrWithKey addTile M.empty amap
    where
        addTile c tile = M.insert c (newTile c tile)
        newTile c tile = forTile tile . length . filter ((==) Occupied) $ checkDirection c amap
        forTile Occupied n = if (n < 5) then Occupied else Empty
        forTile Empty n = if (n == 0) then Occupied else Empty
        forTile Floor _ = Floor

checkDirection :: Coordinate ->AreaMap -> [Tile]
checkDirection (x, y) amap = mapMaybe check [(0, 1), (0, -1), (1, 0), (-1, 0), (-1,1), (1, -1), (1, 1), (-1, -1)]
    where
        check d = fst $ until foundSeat (checkNext d) (Just Floor, 1)
        foundSeat (f, _) = f == Nothing || f /= (Just Floor)
        checkNext (x',y') (_, step) = (M.lookup (x + (x' * step), y + (y' * step)) amap, step + 1)


{-
https://adventofcode.com/2020/day/11#part2

As soon as people start to arrive, you realize your mistake. People don't just care about adjacent seats - they care about the first seat they can see in each of those eight directions!

Now, instead of considering just the eight immediately adjacent seats, consider the first seat in each of those eight directions. For example, the empty seat below would see eight occupied seats:

.......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....
The leftmost empty seat below would only see one empty seat, but cannot see any of the occupied ones:

.............
.L.L.#.#.#.#.
.............
The empty seat below would see no occupied seats:

.##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.
Also, people seem to be more tolerant than you expected: it now takes five or more visible occupied seats for an occupied seat to become empty (rather than four or more from the previous rules). The other rules still apply: empty seats that see no occupied seats become occupied, seats matching no rule don't change, and floor never changes.

Given the same starting layout as above, these new rules cause the seating area to shift around as follows:

Again, at this point, people stop shifting around and the seating area reaches equilibrium. Once this occurs, you count 26 occupied seats.

Given the new visibility method and the rule change for occupied seats becoming empty, once equilibrium is reached, how many seats end up occupied?
-}