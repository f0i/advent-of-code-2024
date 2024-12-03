import { input; test1; test2 } "./input.day3";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Char "mo:base/Char";
import Nat "mo:base/Nat";
import Int "mo:base/Int";

var lines = Iter.toArray(Text.split(Text.trim(input, #predicate(Char.isWhitespace)), #text "mul("));

func toNat(t : Text) : Nat {
  let ?val = Nat.fromText(t) else Debug.trap("Invalid number " # t);
  val;
};

var acc = 0;
label partLoop for (part in lines.vals()) {
  let iter = Text.split(part, #char ',');
  let ?num1 = iter.next() else Debug.trap("Invlid part " # part);
  let ?part2 = iter.next() else continue partLoop;
  let iter2 = Text.split(part2, #char ')');
  let ?num2 = iter2.next() else continue partLoop;
  let ?_ = iter2.next() else continue partLoop; // no ')' after num2
  let ?val1 = Nat.fromText(num1) else continue partLoop; // invalid number
  let ?val2 = Nat.fromText(num2) else continue partLoop; // invalid number
  acc += val1 * val2;
};

Debug.print("Day 3 Part 1: " # Nat.toText(acc));

lines := Iter.toArray(Text.split(Text.trim(input, #predicate(Char.isWhitespace)), #text "mul("));
acc := 0;
var doMul = true;
label partLoop for (part in lines.vals()) {
  let wasDoMul = doMul;
  let hasDo = Text.contains(part, #text("do()"));
  let hasDont = Text.contains(part, #text("don't()"));
  if (hasDo and hasDont) {
    Debug.trap("do and don't: " #part); // handle only when it comes up
  };
  if (hasDo) doMul := true;
  if (hasDont) doMul := false;
  if (not wasDoMul) continue partLoop;
  let iter = Text.split(part, #char ',');
  let ?num1 = iter.next() else Debug.trap("Invlid part " # part);
  let ?part2 = iter.next() else continue partLoop;
  let iter2 = Text.split(part2, #char ')');
  let ?num2 = iter2.next() else continue partLoop;
  let ?_ = iter2.next() else continue partLoop; // no ')' after num2
  let ?val1 = Nat.fromText(num1) else continue partLoop; // invalid number
  let ?val2 = Nat.fromText(num2) else continue partLoop; // invalid number
  acc += val1 * val2;
};

Debug.print("Day 3 Part 2: " # Nat.toText(acc));
