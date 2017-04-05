(* ::Package:: *)

BeginPackage @ "DihedralGroupStatusGenerator`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

makeStatus::usage = "";

Begin @ "`Private`";
Module[{},

	makeStatus[angles_] := Module[{degrees, finalDegrees},
		degrees = roundN[360 / angles];
		finalDegrees = If[degrees == 120, 60, degrees];
		StringForm["Angles: ``, Degrees: ``", angles, finalDegrees]
	];

];
End[];
EndPackage[];
