(* ::Package:: *)

BeginPackage @ "DihedralGroupCayleyTable`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

makeCayleyTable::usage = "";

Begin @ "`Private`";
Module[{rotationLabel, reflectionLabel},

	makeCayleyTable[firstPart_, secondPart_, OptionsPattern[]] :=
	Module[{rotLabel, refLabel, rotationRotationTableAngled, reflectionReflectionTableAngled,
	rotationRotationTableLabeled, reflectionReflectionTableLabeled, rotationReflectionTableLabeled,
	reflectionRotationTableLabeled, rotationTitleRow, rotationTitleColumn, reflectionTitleRow,
	reflectionTitleColumn, table, angles},
		angles = Length @ firstPart[[1]];
	
		rotLabel = OptionValue @ rotationLabel;
		refLabel = OptionValue @ reflectionLabel;
	
		{rotationRotationTableAngled, reflectionReflectionTableAngled} =
			roundN[{firstPart, secondPart} * 360 / angles];
	
		{rotationRotationTableLabeled, reflectionReflectionTableLabeled} =
			addLabelsToTable[#, rotLabel] & /@ {
				rotationRotationTableAngled, reflectionReflectionTableAngled
			};
		{rotationReflectionTableLabeled, reflectionRotationTableLabeled} =
			addLabelsToTable[#, refLabel] & /@ {
				firstPart, secondPart
			};
	
		rotationTitleRow = {rotationRotationTableLabeled[[1]]};
		rotationTitleColumn = {#[[1]]} & /@ rotationRotationTableLabeled;
		reflectionTitleRow = {rotationReflectionTableLabeled[[1]]};
		reflectionTitleColumn = {#[[1]]} & /@ rotationReflectionTableLabeled;
	
		table = composeGrid @ # & /@ # & /@ {
			{{}, rotationTitleRow, reflectionTitleRow},
			{rotationTitleColumn, rotationRotationTableLabeled, rotationReflectionTableLabeled},
			{reflectionTitleColumn, reflectionRotationTableLabeled, reflectionReflectionTableLabeled}
		};
		
		Grid[table, Dividers -> Center]
	];
	Options @ makeCayleyTable = {rotationLabel -> "R", reflectionLabel -> "S"};

];
End[];
EndPackage[];
