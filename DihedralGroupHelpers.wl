(* ::Package:: *)

BeginPackage @ "DihedralGroupHelpers`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

makeLabel::usage = "";
pointOnCircle::usage = "";
composeGrid::usage = "";
roundN::usage = "";
midPoint::usage = "";
addLabelsToList::usage = "";
addLabelsToTable::usage = "";
selectColumnFromTable::usage = "";
dihedralMultiply::usage = "";
findPair::usage = "";

Begin @ "`Private`";
Module[{fontSize, itemSize, toDigit},

	findPair[list_, element_] := Module[{},
		Select[list, #[[1]] == element &][[1, 2]]
	];

	(* Multiply column on row *)
	dihedralMultiply[dihedral_, first_, second_] := Module[{}, 
		dihedral[[first + 1, second + 1]]
	];

	selectColumnFromTable[table_List, index_Integer] := #[[index]] & /@ table;
	
	makeLabel[text_, subscript_, OptionsPattern[]] := 
		Style[Subscript[text, subscript], OptionValue @ fontSize];
	Options @ makeLabel = {fontSize -> 18};
	
	addLabelsToList[list_, text_] := makeLabel[text, #] & /@ list;
	
	addLabelsToTable[table_, text_] := addLabelsToList[#, text] & /@ table;
	
	pointOnCircle = {Cos[# Degree], Sin[# Degree]} &;
	
	composeGrid[grid_, OptionsPattern[]] :=
		Grid[grid, ItemSize -> OptionValue @ itemSize];
	Options @ composeGrid = {itemSize -> {3, 1.5}};
	
	roundN[number_, OptionsPattern[]] :=
		Round[N @ number, OptionValue @ toDigit];
	Options @ roundN = {toDigit -> 1};
	
	midPoint = (#1 + #2) / 2 &;
	
];
End[];
EndPackage[];
