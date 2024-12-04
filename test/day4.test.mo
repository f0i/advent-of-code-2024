import { input; test1; test2 } "./input.day4";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Char "mo:base/Char";
import Nat "mo:base/Nat";
import Int "mo:base/Int";

type Iter<T> = Iter.Iter<T>;

let test3 = "M.S\n.A.\nM.S";

let lines = Iter.toArray(Iter.map(Text.split(Text.trim(input, #predicate(Char.isWhitespace)), #char '\n'), func(t : Text) : [Char] = Iter.toArray(t.chars())));

func toNat(t : Text) : Nat {
  let ?val = Nat.fromText(t) else Debug.trap("Invalid number " # t);
  val;
};

var acc = 0;
let rows = lines.size();
let cols = lines[0].size();

func check(x : Int, y : Int, dx : Int, dy : Int, chars : [Char]) : Bool {
  for (i in chars.keys()) {
    let row = y + (i * dy);
    let col = x + (i * dx);
    if (row < 0 or row >= rows) return false;
    if (col < 0 or col >= cols) return false;
    if (col < 0 or col >= lines[Int.abs(row)].size()) Debug.trap("Different line size in " # debug_show lines[Int.abs(row)]);
    if (lines[Int.abs(row)][Int.abs(col)] != chars[i]) return false;
  };
  //Debug.print("found at " # debug_show (x, y));
  return true;
};

for (row in lines.keys()) {
  for (col in lines[0].keys()) {
    if (check(col, row, -1, -1, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, -1, 0, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, -1, 1, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, 0, -1, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, 0, 1, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, 1, -1, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, 1, 0, ['X', 'M', 'A', 'S'])) acc += 1;
    if (check(col, row, 1, 1, ['X', 'M', 'A', 'S'])) acc += 1;
  };
};

Debug.print("Day 4 Part 1: " # Nat.toText(acc));

acc := 0;

for (row : Int in lines.keys()) {
  assert lines[0].size() == cols;
  for (col : Int in lines[0].keys()) {
    // forward
    if (check(col, row, 1, 0, ['M', 'A', 'S'])) {
      // down
      if (check(col + 1, row - 1, 0, 1, ['M', 'A', 'S'])) acc += 1;
      // up
      if (check(col + 1, row + 1, 0, -1, ['M', 'A', 'S'])) acc += 1;
    };
    // backwards
    if (check(col, row, -1, 0, ['M', 'A', 'S'])) {
      // down
      if (check(col - 1, row - 1, 0, 1, ['M', 'A', 'S'])) acc += 1;
      // up
      if (check(col - 1, row + 1, 0, -1, ['M', 'A', 'S'])) acc += 1;
    };
    // forward up
    if (check(col, row, 1, -1, ['M', 'A', 'S'])) {
      // forward down
      if (check(col, row - 2, 1, 1, ['M', 'A', 'S'])) acc += 1;
      // backwards up
      if (check(col + 2, row, -1, -1, ['M', 'A', 'S'])) acc += 1;
    };
    // forward down
    if (check(col, row, 1, 1, ['M', 'A', 'S'])) {
      // backwards down
      if (check(col + 2, row, -1, 1, ['M', 'A', 'S'])) acc += 1;
    };
    // backwards down
    if (check(col, row, -1, 1, ['M', 'A', 'S'])) {
      // backwards up
      if (check(col, row + 2, -1, -1, ['M', 'A', 'S'])) acc += 1;
    };
  };
};

//Debug.print("Day 4 Part 2: " # Nat.toText(acc));

acc := 0;

func checkSide(a : Char, b : Char) : Bool {
  if (a != 'M' and a != 'S') return false;
  if (b != 'M' and b != 'S') return false;
  if (a == b) return false;
  return true;
};

func checkPlus(x : Nat, y : Nat) : Bool {
  let row = y;
  let col = x;
  if (row < 1 or row >= (rows - 1 : Nat)) return false;
  if (col < 1 or col >= (cols - 1 : Nat)) return false;
  // center
  if (lines[row][col] != 'A') return false;
  let left = lines[row - 1][col];
  let right = lines[row + 1][col];
  if (not checkSide(left, right)) return false;
  let up = lines[row][col - 1];
  let down = lines[row][col + 1];
  if (not checkSide(up, down)) return false;

  return true;
};

func checkX(x : Nat, y : Nat) : Bool {
  let row = y;
  let col = x;
  if (row < 1 or row >= (rows - 1 : Nat)) return false;
  if (col < 1 or col >= (cols - 1 : Nat)) return false;
  // center
  if (lines[row][col] != 'A') return false;
  let left = lines[row - 1][col - 1];
  let right = lines[row + 1][col + 1];
  if (not checkSide(left, right)) return false;
  let up = lines[row + 1][col - 1];
  let down = lines[row - 1][col + 1];
  if (not checkSide(up, down)) return false;

  return true;
};

for (row in lines.keys()) {
  for (col in lines[0].keys()) {
    let hasPlus = checkPlus(col, row);
    let hasX = checkX(col, row);
    if (hasPlus and hasX) {
      //Debug.print("+ and x at " # debug_show (col, row));
    };
    if (hasX) acc += 1;
    //if (hasPlus) acc += 1;
  };
};

Debug.print("Day 4 Part 2: " # Nat.toText(acc));
