# This is how you can contribute

## Guidelines

(loosely) follow [conventional commits](https://gist.github.com/qoomon/5dfcdf8eec66a051ecd85625518cfd13)

## Documentation

Any improvements on documentation are hugely useful

## Tests

Here is a list of tests that need expanding.

| what                                                     | what it does                                             | what it needs                                                                          |
| -------------------------------------------------------- | -------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| [game::blockable_pieces](./src/game/blockable_pieces.rs) | enumerate the possible moves that a given piece can make | a position + the position of a piece and a list of every square that piece can move to |
| [game::Game::try_make_move](./src/game.rs) | make a move (ply) or fail if it isn't valid  | Some random or memorable game, the FEN after each move, and the SAN of each move. An example is in [game::tests::lichess_mM3VkF7P](./src/game.rs) |

## Other

- Add some real positions in FEN notation from some of your games to [positions.txt](./positions.txt) (one entry per line).
  On [Chess.com](https://chess.com), you get this by pressing the Share button and then selecting PGN.
  On [Lichess.org](https://lichess.org), you get this on the analysis board in the `Share & Export` Tab

**Ty :>**
