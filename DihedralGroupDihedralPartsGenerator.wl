(* ::Package:: *)

BeginPackage @ "DihedralGroupDihedralPartsGenerator`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

makeFirstDihedralPart::usage = "";
makeSecondDihedralPart::usage = "";

Begin @ "`Private`";
Module[{},

	makeFirstDihedralPart[angles_, OptionsPattern[]] :=
	Module[{table, angle},
		table = {};
		angle = 360 / angles;
		Module[{i}, For[i = 0, i <= angles - 1, i++,
			AppendTo[table, {}];
			Module[{k}, For[k = 0, k <= angles - 1, k++,
				If[i + k >= angles,
					AppendTo[table[[i + 1]], roundN[i + k - angles]];
				,
					AppendTo[table[[i + 1]], roundN[i + k]];
				];
			]];
		]];
		table
	];
	
	makeSecondDihedralPart[angles_, OptionsPattern[]] :=
	Module[{table, angle},
		table = {};
		angle = 360 / angles;
		Module[{i}, For[i = 0, i <= angles - 1, i++,
			AppendTo[table, {}];
			Module[{k, dif}, For[k = 0, k <= angles - 1, k++,
				dif = i - k;
				If[dif < 0,
					AppendTo[table[[i + 1]], angles - Abs @ dif];
				,
					AppendTo[table[[i + 1]], Abs @ dif];
				];
			]];
		]];
		table
	];
	
];
End[];
EndPackage[];
