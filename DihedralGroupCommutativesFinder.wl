(* ::Package:: *)

BeginPackage @ "DihedralGroupCommutativesFinder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

findCommutatives::usage = "";

Begin @ "`Private`";
Module[{},

	findCommutatives[dihedral_] :=
	Module[{dimension, result, i, k},
		dimension = Length @ dihedral[[1]];
		result = {};
		For[i = 0, i < dimension, i++,
			For[k = 0, k < dimension, k++,
				If[dihedralMultiply[dihedral, i, k] == dihedralMultiply[dihedral, k, i],
					AppendTo[result, {i, k}];
				];
			];
		];
		result
	];

];
End[];
EndPackage[];
