(* ::Package:: *)

BeginPackage @ "DihedralGroupRegularPolygon`";
If[Position[$Path, NotebookDirectory[]] === {}, AppendTo[$Path, NotebookDirectory[]]];

Needs @ "DihedralGroupHelpers`";

makeRegularPolygon::usage = "";

Begin @ "`Private`";
Module[{},

	makeRegularPolygon[angles_] :=
	Module[{graph, degree, currentAngle, lastPoint},
		graph = {};
		degree = 360 / angles;
		currentAngle = 0;
		lastPoint = pointOnCircle[currentAngle - degree];
	
		(* Draw polygon *)
		Module[{i, nextPoint, label}, For[i = 0, i < angles, i++,
			nextPoint = pointOnCircle @ currentAngle;
			label = Text[Style[i, 18, Black], nextPoint * 1.25];
			AppendTo[graph, {Blue, Line @ {lastPoint, nextPoint}, label}];
			lastPoint = nextPoint;
			currentAngle = currentAngle + degree;
		]];
	
		(* Draw reflection lines. *)
		If[Mod[angles, 2] == 0,
			(* Draw reflection lines for polygon with even amount of angles:
				middle-side to middle-side lines, vertex to pair vertex lines. *)
			Module[{vertex, vertexPair, line, firstPointX, firstPointY, secondPointX,
			secondPointY, firstPointPairX, firstPointPairY, secondPointPairX, label2,
			secondPointPairY, midPoint1X, midPoint1Y, i, midPoint2X, midPoint2Y, label1},
			For[i = 1, i <= angles / 2, i++,
				vertex = graph[[i, 2, 1, 1]];
				vertexPair = graph[[i + (angles / 2), 2, 1, 1]];
		
				line = Line @ {vertex, vertexPair};
				AppendTo[graph, {Red, line}];
			
				firstPointX = graph[[i, 2, 1, 1, 1]];
				firstPointY = graph[[i, 2, 1, 1, 2]];
				secondPointX = graph[[i + 1, 2, 1, 1, 1]];
				secondPointY = graph[[i + 1, 2, 1, 1, 2]];
			
				firstPointPairX = graph[[i + (angles / 2), 2, 1, 1, 1]];
				firstPointPairY = graph[[i + (angles / 2), 2, 1, 1, 2]];
				secondPointPairX = graph[[i + (angles / 2) + 1, 2, 1, 1, 1]];
				secondPointPairY = graph[[i + (angles / 2) + 1, 2, 1, 1, 2]];
			
				midPoint1X = midPoint[firstPointX, secondPointX];
				midPoint1Y = midPoint[firstPointY, secondPointY];
				midPoint2X = midPoint[firstPointPairX, secondPointPairX];
				midPoint2Y = midPoint[firstPointPairY, secondPointPairY];

				label1 = Text[Style[makeLabel["S", i - 1 + angles / 2], Gray], {midPoint1X, midPoint1Y} * 1.4];
				label2 = Text[Style[makeLabel["S", i - 1], Gray], {secondPointX, secondPointY} * 1.7];
				
				line = Line @ {{midPoint1X, midPoint1Y}, {midPoint2X, midPoint2Y}};
				
				AppendTo[graph, {Green, line, Black, label1, label2}];
			]];
		,
			(* Draw reflection lines for polygon with odd
				amount of angles: vertex to middle-side lines. *)
			Module[{i, floor, floorIterator, ceiling, ceilingIterator, firstPointX,
			firstPointY, secondPointX, secondPointY, midPointX, midPointY, vertex,
			line, label}, For[i = 1, i <= angles, i++,
				floor = Floor[angles / 2];
				floorIterator = i + floor;
				ceiling = Ceiling[angles / 2];
				ceilingIterator = i + ceiling;

				If[floorIterator > angles, floorIterator -= angles];
				If[ceilingIterator > angles, ceilingIterator -= angles];

				firstPointX = graph[[floorIterator, 2, 1, 1, 1]];
				firstPointY = graph[[floorIterator, 2, 1, 1, 2]];
				secondPointX = graph[[ceilingIterator, 2, 1, 1, 1]];
				secondPointY = graph[[ceilingIterator, 2, 1, 1, 2]];

				midPointX = midPoint[firstPointX, secondPointX];
				midPointY = midPoint[firstPointY, secondPointY];

				vertex = graph[[i, 2, 1, 1]];

				line = Line @ {vertex, {midPointX, midPointY}};
				label = Text[Style[makeLabel["S", i - 1], Gray], {midPointX, midPointY} * 1.4];
				AppendTo[graph, {Red, line, label}];
			];
		]];
	
		graph
	];
	
];
End[];
EndPackage[];
