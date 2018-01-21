(* ::Package:: *)

BeginPackage @ "DihedralGroupMain`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupDihedralGenerator`";
Needs @ "DihedralGroupDihedralPartsGenerator`";
Needs @ "DihedralGroupCayleyTable`";
Needs @ "DihedralGroupRegularPolygon`";
Needs @ "DihedralGroupHelpers`";
Needs @ "DihedralGroupInversesFinder`";
Needs @ "DihedralGroupCommutativesFinder`";
Needs @ "DihedralGroupCenterFinder`";
Needs @ "DihedralGroupNormalizerFinder`";
Needs @ "DihedralGroupStatusGenerator`";
Needs @ "DihedralGroupElementsFinder`";

Begin @ "`Private`";
Module[{refresh, table1, table2, angles, firstPart, secondPart, dihedral, inverses,
commutatives, center, normalizer, status, polygon, cayleyTable, elements},

	refresh = Module[{},
		angles = #;
		status = makeStatus @ angles;
		firstPart = makeFirstDihedralPart @ angles;
		secondPart = makeSecondDihedralPart @ angles;
		polygon = Graphics @ makeRegularPolygon @ angles;
		cayleyTable = makeCayleyTable[firstPart, secondPart];
		dihedral = makeDihedral[firstPart, secondPart];
		inverses = findInverses[dihedral, 0];
		commutatives = findCommutatives @ dihedral;
		center = findCenter @ dihedral;
		normalizer = findNormalizer[dihedral, inverses];
		elements = findElements @ angles;
	] &;

	table1 = {
		{Slider[Dynamic[angles, refresh], {3, 10, 1}], Dynamic @ status},
		{Dynamic @ polygon, Dynamic @ cayleyTable}
	};
	
	table2 = {
		{"Inverses:", Dynamic @ inverses},
		{"Commutatives:", Dynamic @ commutatives},
		{"Center:", Dynamic @ center},
		{"Normalizer:", Dynamic @ Grid @ normalizer},
		{"Elements:", Dynamic @ elements}
	};

	Print @ Grid @ table1;
	Print @ Grid @ table2;

];
End[];
EndPackage[];









