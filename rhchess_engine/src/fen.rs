use crate::board::{self, CastlingRights, Square};

fn parse_positions(
    mut string: &str,
) -> Option<([Option<board::Piece>; 64], board::Square, board::Square)> {
    let mut positions: [Option<board::Piece>; 64] = [None; 64];
    let mut white_position = None;
    let mut black_position = None;
    for rank in (0..=7).rev() {
        let mut was_digit = false;
        let mut file = 0;
        while file <= 7 {
            let p = string.chars().next()?;
            string = string.strip_prefix(p).unwrap();
            if let Some(digit) = p.to_digit(10) {
                if was_digit || digit < 1 || digit > 8 {
                    return None;
                }
                file += digit;
                was_digit = true;
            } else {
                was_digit = false;
                let owner = if p.is_lowercase() {
                    board::Player::Black
                } else {
                    board::Player::White
                };
                use board::PieceKind::*;
                let kind = match p.to_ascii_lowercase() {
                    'p' => Pawn,
                    'r' => Rook,
                    'n' => Knight,
                    'b' => Bishop,
                    'q' => Queen,
                    'k' => {
                        let (file, rank) = (file as u8, rank as u8);
                        match owner {
                            board::Player::White => {
                                white_position = Some(board::Square { rank, file })
                            }
                            board::Player::Black => {
                                black_position = Some(board::Square { rank, file })
                            }
                        };
                        King
                    }
                    _ => {
                        return None;
                    }
                };
                positions[(rank * 8 + file) as usize] = Some(board::Piece { owner, kind });
                file += 1;
            }
        }
        if rank > 0 {
            if let Some('/') = string.chars().next() {
                string = string.strip_prefix('/').unwrap();
            } else {
                return None;
            }
        } else if !string.is_empty() {
            return None;
        }
    }
    Some((positions, white_position?, black_position?))
}

fn parse_turn(string: &str) -> Option<board::Player> {
    match string {
        "w" => Some(board::Player::White),
        "b" => Some(board::Player::Black),
        _ => None,
    }
}

fn parse_castling(string: &str) -> Option<board::CastlingRights> {
    let mut cr = CastlingRights {
        white_king: false,
        white_queen: false,
        black_king: false,
        black_queen: false,
    };
    if string == "-" {
        return Some(cr);
    }
    for i in string.chars() {
        match i {
            'K' => cr.white_king = true,
            'k' => cr.black_king = true,
            'Q' => cr.white_queen = true,
            'q' => cr.black_queen = true,
            _ => return None,
        }
    }
    Some(cr)
}

fn parse_en_passnt(string: &str) -> Option<Option<board::Square>> {
    if string == "-" {
        return Some(None);
    }
    let string = string.chars().collect::<Vec<char>>();
    if string.len() != 2 {
        return None;
    }
    let (file, rank) = ((string[0] as u8 - 97), string[1].to_digit(10)? - 1);
    Some(Some(Square::new(file, rank as u8)?))
}

pub fn parse_fen(string: &str) -> Option<board::Board> {
    let sections = string
        .split(|x: char| x.is_whitespace())
        .collect::<Vec<&str>>();
    let (positions, white_pos, black_pos) = parse_positions(sections.first()?)?;
    Some(board::Board {
        positions,
        white_pos,
        black_pos,
        attacks: board::Attacks::default(),
        turn: parse_turn(sections.get(1)?)?,
        castling_rights: parse_castling(sections.get(2)?)?,
        en_passant: parse_en_passnt(sections.get(3)?)?,
        reversible_moves: sections.get(4)?.parse().ok()?,
        full_moves: sections.get(5)?.parse().ok()?,
    })
}
