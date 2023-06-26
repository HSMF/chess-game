//! import games in PGN format. See [PGN on Wikipedia](https://en.wikipedia.org/wiki/Portable_Game_Notation)

use either::Either;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_till, take_until, take_while},
    character::complete::{char as nchar, digit1, line_ending, none_of, not_line_ending, one_of},
    combinator::opt,
    multi::many0,
    sequence::tuple,
    IResult, Parser,
};

use crate::{
    ply::{san_ply, WipPly},
    Game, ParseSanError, Ply,
};

use super::MoveError;

type IRes<'a, T> = IResult<&'a str, T>;

fn take_whitespace(s: &str) -> IRes<()> {
    let (s, _) = take_while(|x: char| x.is_whitespace())(s)?;
    Ok((s, ()))
}

fn pgn_tag(s: &str) -> IRes<(&str, &str)> {
    let (s, _) = sp(s)?;
    let (s, _) = nchar('[')(s)?;
    let (s, _) = sp(s)?;
    let (s, key) = take_till(char::is_whitespace)(s)?;
    let (s, _) = sp(s)?;
    let (s, _) = nchar('"')(s)?;
    let (s, value) = opt(escaped(none_of(r#""\"#), '\\', one_of(r#""\"#)))(s)?;
    let value = value.unwrap_or("");
    let (s, _) = nchar('"')(s)?;
    let (s, _) = sp(s)?;
    let (s, _) = nchar(']')(s)?;
    let (s, _) = sp(s)?;

    Ok((s, (key, value)))
}

fn line_comment(s: &str) -> IRes<&str> {
    let (s, _) = nchar(';')(s)?;
    let (s, comment) = not_line_ending(s)?;
    let (s, _) = line_ending(s)?;

    Ok((s, comment))
}

fn block_comment(s: &str) -> IRes<&str> {
    let (s, _) = nchar('{')(s)?;
    let (s, comment) = take_until("}")(s)?;
    let (s, _) = nchar('}')(s)?;

    Ok((s, comment))
}

fn comment(s: &str) -> IRes<&str> {
    alt((line_comment, block_comment))(s)
}

fn maybe_comment(s: &str) -> IRes<Option<&str>> {
    opt(comment)(s)
}

fn sp(s: &str) -> IRes<()> {
    let (s, _) = take_whitespace(s)?;
    let (s, _) = maybe_comment(s)?;
    let (s, _) = take_whitespace(s)?;
    Ok((s, ()))
}

fn header(s: &str) -> IRes<Vec<(&str, &str)>> {
    let (s, _) = sp(s)?;

    let (s, values) = many0(tuple((pgn_tag, maybe_comment)).map(|(x, _)| x))(s)?;

    let (s, _) = sp(s)?;

    Ok((s, values))
}

/// tuple of `(move_number, white_move?, black_move?)`
type SingleMove = (u64, Option<WipPly>, Option<WipPly>);

/// extracts a single move e.g. `1. e4 e5`
fn single_move(s: &str) -> IRes<SingleMove> {
    fn white_first(s: &str) -> IRes<SingleMove> {
        let (s, (num, _)) = tuple((digit1, nchar('.')))(s)?;
        let (s, _) = sp(s)?;
        let num = num.parse::<u64>().unwrap_or(u64::MAX); // this should only fail on overflowing input
        let (s, white_move) = san_ply(s)?;
        let (s, _) = sp(s)?;
        let (s, black_move) = opt(san_ply)(s)?;
        Ok((s, (num, Some(white_move), black_move)))
    }

    fn black_first(s: &str) -> IRes<SingleMove> {
        let (s, (num, _)) = tuple((digit1, tag("...")))(s)?;
        let (s, _) = sp(s)?;
        let num = num.parse::<u64>().unwrap_or(u64::MAX); // this should only fail on overflowing input
        let (s, black_move) = san_ply(s)?;
        Ok((s, (num, None, Some(black_move))))
    }

    let (s, _) = sp(s)?;

    let (s, single_move) = alt((white_first, black_first))(s)?;

    let (s, _) = sp(s)?;

    Ok((s, single_move))
}

/// extracts all the moves using [single_move]
fn moves(s: &str) -> IRes<Vec<SingleMove>> {
    let (s, mut v) = many0(single_move)(s)?;
    // sort just in case
    v.sort_by_key(|(i, _, _)| *i);
    Ok((s, v))
}

fn number(s: &str) -> IRes<u64> {
    let (s, num) = alt((
        tuple((digit1, nchar('/'), digit1)).map(Either::Left),
        digit1.map(Either::Right),
    ))(s)?;
    let num = num
        .map_left(|(x, _, _)| x.parse::<u64>().unwrap_or(0))
        .map_right(|x| x.parse::<u64>().unwrap_or(0) * 2)
        .either_into();

    Ok((s, num))
}

fn result(s: &str) -> IRes<Option<(u64, u64)>> {
    alt((
        nchar('*').map(|_| None),
        tuple((number, tag("-"), number)).map(|(x, _, y)| Some((x, y))),
    ))(s)
}

/// The result of a game
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum GameResult {
    /// The game has not yet concluded / is unknown
    #[default]
    Unknown,
    /// the game resulted in a draw
    Draw,
    /// the light player won
    WhiteWin,
    /// the dark player won
    BlackWin,
}

/// A recording of a game, as a sequence of moves
///
/// Use the [`FromStr`] trait to construct a GameRecord
///
/// ```
/// # use chess_game::game::pgn::*;
/// let game = GameRecord::parse(r#"[Event "F/S Return Match"]
/// [Site "Belgrade, Serbia JUG"]
/// [Date "1992.11.04"]
/// [Round "29"]
/// [White "Fischer, Robert J."]
/// [Black "Spassky, Boris V."]
/// [Result "1/2-1/2"]
///
/// 1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
/// 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
/// 11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
/// Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
/// 23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
/// hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
/// 35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
/// Nf2 42. g4 Bd3 43. Re6 1/2-1/2"#).unwrap();
///
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GameRecord<'a> {
    metadata: Vec<(&'a str, &'a str)>,
    moves: Vec<Ply>,
    /// (white_score, black_score), both in half-points
    result: (u64, u64),
}

/// Error that arises from [`GameRecord::parse`]
#[derive(Debug, thiserror::Error)]
pub enum PgnError<'a> {
    /// the input had invalid format
    #[error("invalid format: {0}")]
    Format(nom::Err<nom::error::Error<&'a str>>),
    /// processing a move failed
    #[error("{0}")]
    San(ParseSanError<'a>),
    /// the pgn was valid but there were trailing characters after
    #[error("trailing characters")]
    TrailingChars,
    /// made an invalid move while trying to play out the game
    #[error("invalid move in turn {0}: {1}")]
    InvalidMove(u64, MoveError),
}

impl<'a> GameRecord<'a> {
    /// Parses a game from [PGN](https://en.wikipedia.org/wiki/Portable_Game_Notation)
    pub fn parse(s: &'a str) -> Result<Self, PgnError> {
        let (s, metadata) = header(s).map_err(PgnError::Format)?;

        let (s, wip_moves) = moves(s).map_err(PgnError::Format)?;

        let (s, result) = result(s).map_err(PgnError::Format)?;

        let (s, _) = sp(s).map_err(PgnError::Format)?;
        if !s.is_empty() {
            return Err(PgnError::TrailingChars);
        }

        let mut moves = Vec::with_capacity(2 * wip_moves.len());
        let mut game = Game::default();
        for (turn, white, black) in wip_moves {
            if let Some(white) = white {
                let ply: Result<Ply, _> = white
                    .map_left(|raw| Ply::resolve_san_ply(raw, &game))
                    .map_right(Ok)
                    .either_into();
                let ply = ply.map_err(PgnError::San)?;
                game.try_make_move(ply)
                    .map_err(|e| PgnError::InvalidMove(turn, e))?;
                moves.push(ply);
            }

            if let Some(black) = black {
                let ply: Result<Ply, _> = black
                    .map_left(|raw| Ply::resolve_san_ply(raw, &game))
                    .map_right(Ok)
                    .either_into();
                let ply = ply.map_err(PgnError::San)?;
                game.try_make_move(ply)
                    .map_err(|e| PgnError::InvalidMove(turn, e))?;
                moves.push(ply);
            }
        }

        Ok(GameRecord {
            metadata,
            moves,
            result: result.unwrap_or((0, 0)),
        })
    }

    /// returns an iterator over all the positions in the game
    pub fn positions(&self) -> Positions {
        Positions {
            moves: &self.moves,
            game: Game::default(),
        }
    }

    /// returns a slice of the moves stored in the game record
    pub fn moves(&self) -> &[Ply] {
        &self.moves
    }

    /// returns a slice of the moves stored in the game record
    pub fn metadata(&self) -> &[(&str, &str)] {
        &self.metadata
    }
}

/// Iterator over Positions in a game, obtained by [`GameRecord::positions`]
#[derive(Debug, PartialEq, Eq)]
pub struct Positions<'a> {
    moves: &'a [Ply],
    game: Game,
}

impl<'a> Iterator for Positions<'a> {
    type Item = Game;

    fn next(&mut self) -> Option<Self::Item> {
        let (ply, rest) = self.moves.split_first()?;
        self.moves = rest;
        self.game.try_make_move(*ply).unwrap();
        Some(self.game.clone())
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::Game;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn example_wikipedia() {
        let pgn = r#"[Event "F/S Return Match"]
[Site "Belgrade, Serbia JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2"#;

        let mut game = Game::new();
        let moves = [
            "e4", "e5", "Nf3", "Nc6", "Bb5", "a6", "Ba4", "Nf6", "O-O", "Be7", "Re1", "b5", "Bb3",
            "d6", "c3", "O-O", "h3", "Nb8", "d4", "Nbd7", "c4", "c6", "cxb5", "axb5", "Nc3", "Bb7",
            "Bg5", "b4", "Nb1", "h6", "Bh4", "c5", "dxe5", "Nxe4", "Bxe7", "Qxe7", "exd6", "Qf6",
            "Nbd2", "Nxd6", "Nc4", "Nxc4", "Bxc4", "Nb6", "Ne5", "Rae8", "Bxf7+", "Rxf7", "Nxf7",
            "Rxe1+", "Qxe1", "Kxf7", "Qe3", "Qg5", "Qxg5", "hxg5", "b3", "Ke6", "a3", "Kd6",
            "axb4", "cxb4", "Ra5", "Nd5", "f3", "Bc8", "Kf2", "Bf5", "Ra7", "g6", "Ra6+", "Kc5",
            "Ke1", "Nf4", "g3", "Nxh3", "Kd2", "Kb5", "Rd6", "Kc5", "Ra6", "Nf2", "g4", "Bd3",
            "Re6",
        ]
        .into_iter()
        .map(|x| {
            let ply = Ply::parse_san(x, &game).unwrap();
            game.try_make_move(ply).unwrap();
            ply
        })
        .collect_vec();

        let record: GameRecord = GameRecord::parse(pgn).unwrap();
        assert_eq!(record.moves, moves);
        let end_position: Game = "8/8/4R1p1/2k3p1/1p4P1/1P1b1P2/3K1n2/8 b - - 2 43"
            .parse()
            .unwrap();
        assert_eq!(record.positions().last().unwrap().board, end_position.board);
    }

    /// tests if the last position matches with the field given in CurrentPosition
    ///
    /// on chess.com, go to the last position in the game and export the pgn
    fn consistent_current_position_field(pgn: &str) {
        let game_record = GameRecord::parse(pgn).unwrap();
        let current_position = game_record
            .metadata
            .iter()
            .find(|(key, _)| *key == "CurrentPosition")
            .map(|(_, value)| value)
            .unwrap();

        let game: Game = format!("{current_position} 0 1").parse().unwrap();
        assert_eq!(game_record.positions().last().unwrap().board, game.board)
    }

    /// tests if the last position of the record matches with the given fen
    fn consistent_last(pgn: &str, last: &str) {
        let game_record = GameRecord::parse(pgn).unwrap();

        let game: Game = last.parse().unwrap();
        let last_pos = game_record
            .positions()
            .inspect(|position| eprintln!("{position}"))
            .last()
            .unwrap();
        eprintln!("{last_pos}");
        eprintln!("{last}");
        assert_eq!(last_pos.board, game.board)
    }

    macro_rules! consistent_last_pos {
        ($name:ident, $pgn:literal) => {
            #[test]
            fn $name() {
                consistent_current_position_field($pgn);
            }
        };

        ($name:ident, $pgn:literal, $last:literal) => {
            #[test]
            fn $name() {
                consistent_last($pgn, $last);
            }
        };
    }

    #[test]
    fn chess_com_80343672397() {
        let pgn = r#"[Event "Live Chess"]
[Site "Chess.com"]
[Date "2023.06.12"]
[Round "-"]
[White "eekkkhhh"]
[Black "hydedfg"]
[Result "0-1"]
[CurrentPosition "r4rk1/1p2b1p1/p2p1p1p/2p1pq1P/2P5/3PRN2/PP6/R2Q2K1 w - -"]
[Timezone "UTC"]
[ECO "B23"]
[ECOUrl "https://www.chess.com/openings/Closed-Sicilian-Defense-2...d6-3.Bb5-Bd7-4.Bxd7"]
[UTCDate "2023.06.12"]
[UTCTime "21:17:31"]
[WhiteElo "713"]
[BlackElo "798"]
[TimeControl "180+2"]
[Termination "hydedfg won by resignation"]
[StartTime "21:17:31"]
[EndDate "2023.06.12"]
[EndTime "21:20:36"]
[Link "https://www.chess.com/game/live/80343672397"]
[WhiteUrl "https://images.chesscomfiles.com/uploads/v1/user/113423566.9a68e59a.50x50o.241db1ed725b.png"]
[WhiteCountry "85"]
[WhiteTitle ""]
[BlackUrl "https://images.chesscomfiles.com/uploads/v1/user/220084469.5805e6cb.50x50o.907fbd0f535a.jpg"]
[BlackCountry "133"]
[BlackTitle ""]

1. e4 c5 2. Nc3 d6 3. Bb5+ Bd7 4. Bxd7+ Nxd7 5. d3 Ngf6 6. f4 e5 7. Nf3 Be7 8.
O-O O-O 9. Re1 a6 10. f5 h6 11. g4 Nxg4 12. h3 Ngf6 13. Nd5 Nxd5 14. exd5 Nb6
15. Be3 Nxd5 16. c4 Nxe3 17. Rxe3 f6 18. h4 Qc8 19. h5 Qxf5 0-1"#;
        consistent_current_position_field(pgn)
    }

    consistent_last_pos!(
        chess_com_80323980449,
        r#"[Event "Live Chess"]
[Site "Chess.com"]
[Date "2023.06.12"]
[Round "?"]
[White "Hojamyrat01"]
[Black "hydedfg"]
[Result "1-0"]
[ECO "B28"]
[WhiteElo "760"]
[BlackElo "784"]
[TimeControl "180+2"]
[EndTime "8:53:27 PDT"]
[Termination "Hojamyrat01 won by resignation"]

1. e4 c5 2. Nf3 d6 3. d4 cxd4 4. Nxd4 a6 5. c4 Nf6 6. Bd3 g6 7. Nc3 Bg7 8. O-O
Nxe4 9. Bxe4 Qb6 10. Nce2 O-O 11. b3 Nc6 12. Be3 Nxd4 13. Nxd4 Bd7 14. Ne6 1-0"#,
        "r4rk1/1p1bppbp/pq1pN1p1/8/2P1B3/1P2B3/P4PPP/R2Q1RK1 b - - 2 14"
    );

    consistent_last_pos!(
        chess_com_80324381191,
        r#"[Event "Live Chess"]
[Site "Chess.com"]
[Date "2023.06.12"]
[Round "?"]
[White "hydedfg"]
[Black "vdefranceschi"]
[Result "1-0"]
[ECO "C28"]
[WhiteElo "792"]
[BlackElo "762"]
[TimeControl "180+2"]
[EndTime "9:03:57 PDT"]
[Termination "hydedfg won by checkmate"]

1. e4 e5 2. Nc3 Nf6 3. f4 d6 4. fxe5 dxe5 5. Nf3 Nc6 6. Bc4 Bg4 7. h3 Bxf3 8.
Qxf3 Nd4 9. Qd3 c5 10. b3 g6 11. Bb2 Bg7 12. O-O-O O-O 13. g4 Nd7 14. h4 h5 15.
gxh5 gxh5 16. Rdg1 Nf6 17. Ne2 Ng4 18. Nxd4 cxd4 19. c3 Qd7 20. Bd5 Nf2 21. Qg3
Ng4 22. Bc4 Kh8 23. Be2 Rg8 24. Bxg4 hxg4 25. Qxg4 Qxg4 26. Rxg4 Bh6 27. Rhg1 d3
28. c4 Rac8 29. Bxe5+ Kh7 30. R4g3 Bf8 31. Bf4 Be7 32. h5 Ba3+ 33. Kb1 Rcd8 34.
Bc7 Rde8 35. e5 Rxg3 36. Rxg3 Re6 37. Rxd3 Kh6 38. Rd7 Bb4 39. d3 a6 40. Rxf7
Kxh5 41. Rf6 Rxf6 42. exf6 Kg6 43. Be5 Bf8 44. Kb2 a5 45. Kc3 b6 46. Kd4 Bc5+
47. Kd5 Kf7 48. d4 Bb4 49. Kc6 Bc3 50. Kxb6 Be1 51. a3 Bh4 52. b4 Bxf6 53. Bxf6
Kxf6 54. bxa5 Ke6 55. a6 Kd7 56. Kb7 Kd6 57. a7 Ke6 58. a8=Q Kf5 59. Qd8 Ke4 60.
d5 Kd3 61. c5 Kc4 62. Qd6 Kb3 63. c6 Kc4 64. c7 Kd4 65. c8=Q Kd3 66. a4 Kd4 67.
a5 Kd3 68. a6 Kd4 69. a7 Kd3 70. a8=Q Ke2 71. Qa2+ Kd3 72. Qg6+ Kd4 73. Qcc4+
Ke5 74. Qge4+ Kd6 75. Qe6# 1-0"#,
        "8/1K6/3kQ3/3P4/2Q5/8/Q7/8 b - - 10 75"
    );

    consistent_last_pos!(
        chess_com_80323348825,
        r#"[Event "Live Chess"]
[Site "Chess.com"]
[Date "2023.06.12"]
[Round "?"]
[White "hydedfg"]
[Black "xenotrox"]
[Result "0-1"]
[ECO "B01"]
[WhiteElo "786"]
[BlackElo "808"]
[TimeControl "180+2"]
[EndTime "8:42:30 PDT"]
[Termination "xenotrox won by checkmate"]
; this is a comment

1. e4 d5 2. exd5 Qxd5 3. Nc3 Qd8 4. Nf3 Bf5 5. d4 Nf6 6. Bd3 Bc8 7. O-O Nc6 8. ; Be4 is next
Be4 e6 9. a3 Nd5 10. Bxd5 exd5 11. b4 Be6 12. Re1 Bd6 13. Qe2 O-O 14. Ng5 Re8
15. Nxe6 Rxe6 16. Qf3 Rxe1# 0-1"#,
        "r2q2k1/ppp2ppp/2nb4/3p4/1P1P4/P1N2Q2/2P2PPP/R1B1r1K1 w - - 0 17"
    );
}
