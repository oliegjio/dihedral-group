(* ::Package:: *)

BeginPackage @ "DihedralGroupDihedralGenerator`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupDihedralPartsGenerator`";

makeDihedral::usage = "";

Begin @ "`Private`";
Module[{},

	makeDihedral[firstPart_, secondPart_] := Module[{angles},
		angles = Length @ firstPart[[1]];
		Join[
			MapIndexed[Join[#1, Flatten @ firstPart[[#2]] + angles] &, firstPart],
			MapIndexed[Join[#1 + angles, Flatten @ secondPart[[#2]]] &, secondPart]
		]
	];
	
];
End[];
EndPackage[];
