(* ::Package:: *)

(* : Title : base64 *)
(* : Context : base64` *)
(* : 
    Author : Dario Malchiodi *)
(* : 
    Summary : base64 encoding and decoding *)
(* : 
    Package Version : 1.0 *)
(* : Mathematica Version : 5.1 *)
(* : 
    Keywords : base64 encoding *)

BeginPackage["base64`"]

base64EncodeTable::usage = 
"base64EncodeTable[n] returns the ASCII character corresponding to the value n in the base64 encoding";\

base64GroupEncode::usage = 
"base64GroupEncode[group] returns the four-character string  base64-encoding a group of three bytes";\

base64Encode::usage = 
"base64Encode[byt] returns the string base64-encoding the sequence of bytes byt";\

base64DecodeTable::usage = 
"base64DecodeTable[c] returns the byte corresponding to the ASCII character c in the base64 encoding";\

base64GroupDecode::usage = 
"base64GroupDecode[group] returns the three bytes corresponding to the base64-encoding whose bytes are in the list group";\

base64Decode::usage=
    "base64Decode[str] returns the list containing the bytes corresponding to \
the base64-decoded version of the string str";\

Begin["`Private`"]

base64EncodeTable[n_] := Which[
    n == 0, "=",
    n < 26,
    FromCharacterCode[ToCharacterCode["A"] + n],
    n < 52,
    FromCharacterCode[ToCharacterCode["a"] + n - 26],
    n < 62,
    FromCharacterCode[ToCharacterCode["0"] + n - 52],
    n == 62, "+",
    n == 63, "-"
    ]

base64GroupEncode[group_] := Block[{},
    f = Partition[Map[IntegerDigits[#, 2, 8] &, group] // Flatten, 6];
    base64EncodeTable /@ Map[FromDigits[#, 2] &, f]
    ]

base64Encode[byt_] := Block[{b},
    b = byt;
    b = Join[b, Switch[Mod[Length[b], 3],
          0, {},
          1, {0, 0},
          2, {0}
          ]];
    StringExpression @@ (base64GroupEncode /@ Partition[b, 3] // Flatten)
    ]

base64DecodeTable[c_] := Which[
    FromCharacterCode[c] == "-", 63,
    FromCharacterCode[c] == "+", 62,
    ToCharacterCode["0"][[1]] <= c <= ToCharacterCode["9"][[1]],
    c - ToCharacterCode["0"][[1]] + 52,
    ToCharacterCode["a"][[1]] <= c <= ToCharacterCode["z"][[1]],
    c - ToCharacterCode["a"][[1]] + 26,
    ToCharacterCode["A"][[1]] <= c <= ToCharacterCode["Z"][[1]],
    c - ToCharacterCode["A"][[1]],
    FromCharacterCode[c] == "=", 0
    ]

base64GroupDecode[group_] := Block[{},
    FromDigits[#, 2] & /@ 
      Partition[IntegerDigits[#, 2, 6] & /@ group // Flatten, 8]
    ]

base64Decode[str_] := Block[{s},
    s = ToCharacterCode[
        StringReplace[str, n : Except["="] ... ~~ {"="} .. -> n]];
    (base64GroupDecode /@ Partition[base64DecodeTable/@s,4,4,{1,1},{}]) // Flatten
    ]

End[]

EndPackage[]
