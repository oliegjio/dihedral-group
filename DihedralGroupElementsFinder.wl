(* ::Package:: *)

BeginPackage @ "DihedralGroupElementsFinder`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

findElements::usage = "";

Begin @ "`Private`";
Module[{},

	zElement[module_, element_] :=
		If[element <= module && element >= 1, element, Mod[element, module]];

	findElements[angles_] :=
	Module[{dimension, result, i, k},
		result = {};
		elements = Range[0, angles - 1];
		For[i = 0, i < angles, i++,
			permutation = {};
			For[k = 1, k <= angles, k++,
				AppendTo[permutation, zElement[angles, k + i] - 1];
			];
			rotationElement = {elements, permutation};
			reflectionElement = {elements, Reverse @ permutation};
			AppendTo[result, {makeLabel["R", i], MatrixForm @ rotationElement}];
			AppendTo[result, {makeLabel["S", i], MatrixForm @ reflectionElement}];
		];
		If[Length @ result >= 35, Take[result, 35], result]
	];

];
End[];
EndPackage[];
