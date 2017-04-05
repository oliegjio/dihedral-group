(* ::Package:: *)

BeginPackage @ "DihedralGroupNormalizerFinder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

findNormalizer::usage = "";

Begin @ "`Private`";
Module[{},

	findNormalizer[dihedral_, inverses_] := Module[{computeRow},
		computeRow[element_] := Module[{},
			dihedralMultiply[
				dihedral,
				dihedralMultiply[dihedral, findPair[inverses, element], #],
				element
			] & /@ dihedral[[1]]
		];
		computeRow @ # & /@ dihedral[[1]]
	];

];
End[];
EndPackage[];
