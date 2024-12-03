import { input; testdata } "./input.day2";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Char "mo:base/Char";
import Nat "mo:base/Nat";
import Int "mo:base/Int";

let lines = Iter.toArray(Text.split(Text.trim(input, #predicate(Char.isWhitespace)), #char '\n'));

func toNat(t : Text) : Nat {
  let ?val = Nat.fromText(t) else Debug.trap("Invalid number " # t);
  val;
};

var acc = 0;
label lineLoop for (line in lines.vals()) {
  let parts = Iter.toArray(Text.tokens(line, #predicate(Char.isWhitespace)));
  let numbers = Array.map(parts, toNat);
  if (numbers[0] == numbers[1]) continue lineLoop;
  let asc = numbers[0] < numbers[1];

  for (i in numbers.keys()) {
    if (i > 0) {
      let diff : Int = numbers[i] : Int - numbers[i - 1];
      if (asc and diff <= 0) continue lineLoop;
      if (not asc and diff >= 0) continue lineLoop;
      if (Int.abs(diff) > 3) continue lineLoop;
    };
  };
  //Debug.print("safe: " # line);
  acc += 1;
};

Debug.print("Day 2 Part 2: " # Nat.toText(acc));

acc := 0;
label lineLoop for (line in lines.vals()) {
  let parts = Iter.toArray(Text.tokens(line, #predicate(Char.isWhitespace)));
  let numbers = Array.map(parts, toNat);
  label skipLoop for (skip in numbers.keys()) {
    func toIndex(x : Nat) : Nat = if (x <= skip) x - 1 else x;

    if (numbers[toIndex(1)] == numbers[toIndex(2)]) continue skipLoop;
    let asc = numbers[toIndex(1)] < numbers[toIndex(2)];

    for (i in numbers.keys()) {
      if (i > 1) {
        let diff : Int = numbers[toIndex(i)] : Int - numbers[toIndex(i - 1)];
        if (asc and diff <= 0) continue skipLoop;
        if (not asc and diff >= 0) continue skipLoop;
        if (Int.abs(diff) > 3) continue skipLoop;
      };
    };
    //Debug.print("safe: " # line);
    acc += 1;
    continue lineLoop;
  };
};

Debug.print("Day 2 Part 1: " # Nat.toText(acc));
