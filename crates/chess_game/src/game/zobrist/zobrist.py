import random
import typing
import os
import subprocess

MAX = 2**64 - 1


def new_int() -> int:
    return random.randint(0, MAX)


def gen_piece_tables(f: typing.TextIO):
    for piece in ["pawn", "rook", "knight", "bishop", "queen", "king"]:
        for color in ["black", "white"]:
            name = f"{color.upper()}_{piece.upper()}"
            values = [new_int() for _ in range(64)]
            values = ", ".join(map(str, values))
            f.write(f"/// The Hash Indices for the {color} {piece}\n")
            f.write(f"pub const {name}: [u64; 64] = [{values}];\n")


def gen_castling_rights(f: typing.TextIO):
    for white in ["none", "k", "q", "all"]:
        for black in ["none", "k", "q", "all"]:
            name = f"CASTLING_RIGHT_{white.upper()}_{black.upper()}"
            value = new_int()

            f.write("/// The Hash Index for the respective castling right\n")
            f.write(f"pub const {name}: u64 = {value};\n")


def gen_to_move(f: typing.TextIO):
    f.write("/// The Hash Index for when it's black's turn to move\n")
    f.write(f"pub const BLACK_TO_MOVE: u64 = {new_int()};\n")


def gen_en_passant(f: typing.TextIO):
    values = [new_int() for _ in range(8)]
    values = ", ".join(map(str, values))
    f.write("/// The Hash Indices for the en passant files\n")
    f.write(f"pub const EN_PASSANT_FILE: [u64; 8] = [{values}];")


def main():
    dirname = os.path.dirname(os.path.abspath(__file__))
    fname = f"{dirname}/values.rs"
    with open(fname, "w") as f:
        f.write("// This file was auto-generated by zobrist.py\n")
        gen_piece_tables(f)
        f.write("\n")
        gen_castling_rights(f)
        f.write("\n")
        gen_to_move(f)
        f.write("\n")
        gen_en_passant(f)
        f.write("\n")
    subprocess.run(["rustfmt", fname])


if __name__ == "__main__":
    main()
