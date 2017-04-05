(* ::Package:: *)

BeginPackage @ "DihedralGroupCenterFinder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

findCenter::usage = "";

Begin @ "`Private`";
Module[{},

	findCenter[dihedral_] := Module[{},
		Flatten @ Select[
			MapIndexed[If[selectColumnFromTable[dihedral, #2[[1]]] == #1, #2 - 1] &, dihedral],
			ListQ
		]
	];

];
End[];
EndPackage[];
