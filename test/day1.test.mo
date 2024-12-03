import { input } "./input.day1";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Char "mo:base/Char";
import Nat "mo:base/Nat";

let lines = Iter.toArray(Text.split(Text.trim(input, #predicate(Char.isWhitespace)), #char '\n'));

let list1 = Array.map(
  lines,
  func(l : Text) : Nat {
    let parts = Iter.toArray(Text.tokens(l, #predicate(Char.isWhitespace)));
    //Debug.print("parts: " # debug_show (parts));
    let t = parts[0];
    let ?val = Nat.fromText(t) else Debug.trap("Invalid text " # t # " in line " # l);
    val;
  },
);
//Debug.print("parsing list 2");
let list2 = Array.map(
  lines,
  func(l : Text) : Nat {
    let parts = Iter.toArray(Text.tokens(l, #predicate(Char.isWhitespace)));
    //Debug.print("parts: " # debug_show (parts));
    let t = parts[1];
    let ?val = Nat.fromText(t) else Debug.trap("Invalid text " # t # " in line " # l);
    val;
  },
);

let sorted1 = Array.sort(list1, Nat.compare);
let sorted2 = Array.sort(list2, Nat.compare);

var acc = 0;
for (i in sorted1.keys()) {
  let a = sorted1[i];
  let b = sorted2[i];
  acc += if (a > b) a -b else b -a;
};

Debug.print("Day 1 Part 1: " # Nat.toText(acc));

acc := 0;
var j = 0;
var last = 0;
var count = 0;
for (i in sorted1.keys()) {
  let a = sorted1[i];
  if (a != last) {
    last := a;
    count := 0;
    while (sorted2[j] < a) {
      // skip
      j += 1;
    };
    while (sorted2[j] == a) {
      count += 1;
      j += 1;
    };
  };
  acc += (a * count);
};

Debug.print("Day 1 Part 2: " # Nat.toText(acc));
