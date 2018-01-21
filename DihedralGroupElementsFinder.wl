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
	Module[{dimension, result, i, k, index, reflectionElement, rotationElement, permutation, rotations, reflections, angle, elements, reversed},
		result = {};
		rotations = {};
		reflections = {};
		angle = 360 / angles;
		elements = Range[0, angles - 1];
		For[i = 0, i < angles, i++,
			permutation = {};
			For[k = 1, k <= angles, k++,
				AppendTo[permutation, zElement[angles, k + i] - 1];
			];
			rotationElement = {elements, permutation};
			reversed = Reverse @ permutation;
			reflectionElement = {elements, reversed};
			AppendTo[rotations, {makeLabel["R", i * angle], MatrixForm @ rotationElement}];
			If[Mod[angles, 2] == 0,
				index = Null;
				For[k = 1, k <= angles, k++,
					If[elements[[k]] == reversed[[k]],
						index = k - 1;
						Break[];
					, False];
				];
				If[index == Null,
					For[k = 1, k <= angles - 1, k++,
						If[elements[[k]] == reversed[[k + 1]] && reversed[[k]] == elements[[k + 1]],
							{top, bottom} = Sort[{elements[[k]], elements[[k + 1]]}, Greater];
							If[top == angles / 2, index = angles / 2,
								index = bottom + (angles / 2) + 1;
							];
							Break[];
						, False];
					];
				, False];
				AppendTo[reflections, {makeLabel["S", index], MatrixForm @ reflectionElement}];
			,
				For[k = 1, k <= angles, k++,
					If[elements[[k]] == reversed[[k]], 
						If[k == angles, index = 0, index = k], False];
				];
				AppendTo[reflections, {makeLabel["S", index], MatrixForm @ reflectionElement}];
			];
		];
		
		result = Join[rotations, reflections];
		
		If[Length @ result >= 35, Take[result, 35], result]
	];

];
End[];
EndPackage[];
