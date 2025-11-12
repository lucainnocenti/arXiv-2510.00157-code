(* ::Package:: *)

(* ::Input::Initialization:: *)
If[MemberQ[$Packages,"pauliStringsHandler`"],
Unprotect@@Names@"pauliStringsHandler`*";
ClearAll@@Names@"pauliStringsHandler`*";
Remove@@Names@"pauliStringsHandler`*";
];

BeginPackage["pauliStringsHandler`"];

pauliProd;
pauliProdWithGenerators;
pauliPower;
allPauliStrings;
allStringsFromAbGenerators;

prettifyPaulis;
$prettifyPaulisHighlighterFun;

paulisCommuteQ;
paulisAnticommuteQ;
anticommutingSetQ;

partitionByCosets;
centralizerGenerators;
stabiliserFromGenerators;
centralizerOverGroupFromGenerators;

Begin["`Private`"];


(* ::Input:: *)
(*SetOptions[$FrontEndSession,"ExportTypesetOptions"->{"PageWidth"->500}]*)
(*SelectPositions=ResourceFunction["SelectPositions"];*)


(* ::Input::Initialization:: *)
ClearAll[pauliProd];
pauliProd[]:="I";
pauliProd[s_String]:=s;
pauliProd["I",s_String]:=s;
pauliProd[s_String,"I"]:=s;
pauliProd[s1_String,s2_String]/;(StringLength@s1==StringLength@s2==1&&s1==s2):="I";
pauliProd["X","Y"]:="Z";
pauliProd["X","Z"]:="Y";
pauliProd["Y","X"]:="Z";
pauliProd["Y","Z"]:="X";
pauliProd["Z","X"]:="Y";
pauliProd["Z","Y"]:="X";
pauliProd::unequalStrings="The given strings have different lengths: I got `1` and `2`.";
pauliProd[s1_String,s2_String]/;(StringLength@s1!=StringLength@s2):=(
Message[pauliProd::unequalStrings,s1,s2];$Failed
);
pauliProd[s1_String,s2_String]:=Map[
pauliProd[#[[1]],#[[2]]]&,
Transpose@{StringSplit[s1,""],StringSplit[s2,""]}
]//StringJoin;
pauliProd[s1_,s2_]:=pauliProd[ToString@s1,ToString@s2];
pauliProd[s__]:=pauliProd[{s}[[1]],pauliProd@@(Rest@{s})];


(* ::Input:: *)
(*{pauliProd["X","Y"],pauliProd["XZ","YY"]}*)


(* ::Text:: *)
(*Compute P^0 or P^1 for a pauli string P*)


(* ::Input::Initialization:: *)
pauliPower[s_String,n:(0|1)]:=If[n==0,StringJoin@ConstantArray["I",StringLength@s],s];


(* ::Input:: *)
(*{pauliPower["XX",0],pauliPower["XX",1]}*)


(* ::Text:: *)
(*Compute the full abelian group corresponding to a given set of (abelian) generators*)


(* ::Input::Initialization:: *)
allPauliStrings[numQubits_Integer]:=StringJoin/@Tuples[{"I","X","Y","Z"},{numQubits}];

allStringsFromAbGenerators[generators_List]:=With[
{numGen=Length@generators,n=StringLength@generators[[1]]},
DeleteDuplicates@With[{tuples=Tuples[{0,1},{numGen}]},
Table[
pauliProd@@Table[
pauliPower[generators[[idx]],tuple[[idx]]],
{idx,Length@tuple}
],
{tuple,tuples}
]
]];


(* ::Input:: *)
(*allPauliStrings@2*)


(* ::Input:: *)
(*allStringsFromAbGenerators@{"XX","YY"}*)


(* ::Text:: *)
(*Commands to pretty print pauli strings, and highlight strings without Zs*)


(* ::Input::Initialization:: *)
pauliColorsMap=<|"X"->Red,"Y"->Blue,"Z"->Darker@Green|>;

$prettifyPaulisHighlighterFun=Not@StringContainsQ[#,"Z"]&;
prettifyPaulis[str_String]:=With[{chars=Characters@str},
Row@Table[
Style[c,Lookup[pauliColorsMap,c,Black],Bold],
{c,chars}
]//If[$prettifyPaulisHighlighterFun@str,Style[#,Background->LightGray],#]&
];
prettifyPaulis[expr_]:=ReplaceAll[expr,s_String:>prettifyPaulis@s];


(* ::Input:: *)
(*{"XX","IYZ"+2,"XYX"}//prettifyPaulis*)


(* ::Text:: *)
(*Figure out whether stuff commutes or anticommutes*)


(* ::Input::Initialization:: *)
singlePaulisCommuteQ[p1_String,p2_String]:=Which[
p1==p2,True,
p1=="I"||p2=="I",True,
True,False
];

ClearAll[paulisCommuteQ];
paulisCommuteQ[s1_String,s2_String]/;StringLength@s1!=StringLength@s2:=(Echo["ERRRRRRRROR"];Abort[]);
paulisCommuteQ[s1_String,s2_String]:=Table[
singlePaulisCommuteQ[StringTake[s1,{idx}],StringTake[s2,{idx}]],
{idx,StringLength@s1}
]//Cases@False//Length//EvenQ;

paulisCommuteQ[setOfStrings__String]:=And@@(paulisCommuteQ@@#&/@Subsets[{setOfStrings},{2}]);
paulisCommuteQ[listOfStrings_List]:=paulisCommuteQ@@listOfStrings;

anticommutingSetQ[list_List]:=And@@(
Subsets[list,{2}]//Map[Not@paulisCommuteQ@#&]
);
paulisAnticommuteQ=anticommutingSetQ;


(* ::Input:: *)
(*paulisCommuteQ@{"XX","YY"}*)
(*paulisCommuteQ@{"XX","XY"}*)
(*paulisCommuteQ@{"XX","YY","ZZ"}*)


(* ::Input:: *)
(*anticommutingSetQ@{"XX","YY"}*)
(*anticommutingSetQ@{"X","Y","Z"}*)


(* ::Input::Initialization:: *)
pauliProdWithGenerators[s_String,generators_List]:=DeleteDuplicates[pauliProd[s,#]&/@allStringsFromAbGenerators@generators];

partitionByCosets[paulis_List,coset_String]:=DeleteDuplicates[
Sort@{#,pauliProd[#,coset]}&/@paulis
];
partitionByCosets[paulis_List,cosets_List]:=DeleteDuplicates[
Sort@pauliProdWithGenerators[#,cosets]&/@paulis
];


(* ::Input:: *)
(*pauliProdWithGenerators["XX",{"YI","ZI"}]*)


(* ::Input:: *)
(*partitionByCosets[allStringsFromAbGenerators@{"XX","ZZ"},"XX"]*)


(* ::Input:: *)
(*partitionByCosets[allStringsFromAbGenerators@{"XXI","ZZI","IIX"},{"XXI","ZZI"}]*)


(* ::Input::Initialization:: *)
pauliBits=<|"I"->{0,0},"X"->{1,0},"Z"->{0,1},"Y"->{1,1}|>;
(*Convert a Pauli string "XIYZ\[Ellipsis]" to a length-2n binary row*)
pauliToVector[str_String]:=Flatten@Transpose[pauliBits/@Characters[str]];
	
(*Build the 2n\[Times]2n symplectic form J*)
symplecticForm[n_Integer]:=ArrayFlatten[{
{ConstantArray[0,{n,n}],IdentityMatrix[n]},
{IdentityMatrix[n],ConstantArray[0,{n,n}]}
}];

vectorToPauli[v:{(0|1)..}]:=With[{n=Length@v/2},
With[{x=Take[v,n],z=Take[v,-n]},
StringJoin@MapThread[
Which[#1==0&&#2==0,"I",#1==1&&#2==0,"X",#1==0&&#2==1,"Z",True,"Y"]&,
{x,z}
]
]
];

(* find generators for the centraliser of a given set of paulis *)
centralizerGenerators[gens_List]:=With[{
(*binary rows of the given generators*)
S=pauliToVector/@gens,
J=symplecticForm@StringLength@First@gens
},
With[{nullBasis=NullSpace[Mod[S . J,2],Modulus->2]},
(* not sure DeleteDuplicates is needed here tbh *)
DeleteDuplicates[vectorToPauli/@nullBasis]
]
];


(* ::Input:: *)
(*{pauliToVector@"X",pauliToVector@"Z",pauliToVector@"XY"}*)


(* ::Input:: *)
(*symplecticForm@2//MatrixForm*)


(* ::Input:: *)
(*{vectorToPauli@{1,1,0,1},vectorToPauli@{1,1,0,1}}*)


(* ::Input:: *)
(*centralizerGenerators@{"X"}*)


(* ::Input:: *)
(*centralizerGenerators@{"XYX","XXX"}*)


(* ::Input::Initialization:: *)
stabiliserFromGenerators[generators_List]:=partitionByCosets[
allStringsFromAbGenerators@centralizerGenerators@generators,
generators
]//Map[SortBy[StringContainsQ[#,"Z"]&]];
centralizerOverGroupFromGenerators=stabiliserFromGenerators;


(* ::Input:: *)
(*stabiliserFromGenerators@{"XX"}*)


(* ::Input::Initialization:: *)
End[];

Protect /@ Names["pauliStringsHandler`*"];

EndPackage[];
