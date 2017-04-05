(* ::Package:: *)

BeginPackage @ "DihedralGroupInversesFinder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

findInverses::usage = "";

Begin @ "`Private`";
Module[{},

	findInverses[dihedral_, neutalElement_] :=
	Module[{dimension, result, i, k},
		dimension = Length @ dihedral[[1]];
		result = {};
		For[i = 0, i < dimension, i++,
			For[k = 0, k < dimension, k++,
				If[dihedralMultiply[dihedral, i, k] == neutalElement,
					AppendTo[result, {i, k}];
				];
			];
		];
		result
	];

];
End[];
EndPackage[];
