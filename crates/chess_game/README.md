# Chess Game

A simple chess implementation.

```rust
use chess_game::{Game, Ply, Player};

// set up a new game
let mut game = Game::new();
// play a king's pawn opening
assert_eq!(game.player_to_move(), Player::White);
game.try_make_move(Ply::parse_san("e4", &game).expect("valid notation")).expect("valid move");

// respond with sicilian defense
assert_eq!(game.player_to_move(), Player::Black);
game.try_make_move(Ply::parse_san("c5", &game).expect("valid notation")).expect("valid move");
assert_eq!(game.player_to_move(), Player::White);

game.try_make_move(Ply::parse_san("Nf3", &game).expect("valid notation")).expect("valid move");
assert_eq!(game.player_to_move(), Player::Black);
```

To compile this to Web Assembly, activate the feature "wasm"
