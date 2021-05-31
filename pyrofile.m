(* ::Package:: *)

(* :Title : MoleculesTutorial *)
(* :Context : MoleculesTutorial` *)
(* :Author : Giosu\[EGrave] Cotugno, Michele Gaspari, Nico Giambi, Davide Pruscini *)
(* :Summary :  Functions for interactive chemistry lesson to be called from the proper notebook *)
(* :Copyright : GC, MG, NG, DP 2021 *)
(* :Package Version : 1.0 *)
(* :Mathematica Version : 12.2 *)
(* :Sources : https://github.com/MicheleGa/Young-IT-students-close-to-the-failure.git *)
(* :Limitations : Academic usage *)


BeginPackage["MoleculesTutorial`"];

DictMolecules::usage = "DictMolecules []
	return an Association beetwen {CommonName, AlternateNames, IUPACName -> CanonicalName} for each molecule."
	
ListMolecules::usage = "ListMolecules []
	return a List with the union of all kind of names (CommonName, AlternateNames, IUPACName, Canonical Name) for each molecule."

GetMolecule::usage = "GetMolecule[name_:String, dict_:Association, theme_:String, typePlot_:String]
	given any type of molecule name and a dictionary to search on returns a MoleculePlot/MoleculePlot3D depending on the value of typePlot.
	The theme string will be used to change the display theme of the molecule within the Manipulate."

GetProperties::usage = "GetProperties [prt_:List]
	given a list of properties, a hyperlink is added to each of them and a google search is carried out using the CanonicalName to provide a detailed description."
	
RemoveChars::usage = "RemoveChars[x_:String, y_:Integer]
	this function takes one string x and one integer y  and returns a word with y characters missing from the word x.
	It returns the missing index too because it will be useful inside the hint function."
	
HintChar::usage = "HintChar[completa_:String, incompleta_:String, indexRem_:List]
	this function takes 2 words and one list and returns the second word with a character that it was deleted from the RemoveChars function.
	It returns the indexes that are left to be used if called again."
	
RandomMolecule::usage = "RandomMolecule[molecules_:List]
	this function takes a list of molecules' entity canonical names and returns one random of these."


Begin["`Private`"];


(*Return an Association beetwen {CommonName, AlternateNames, IUPACName -> CanonicalName} for each molecule*)
DictMolecules[] :=
    Module[{path, namesBad, iupacBad, alternateBad, canonical, names, iupac, alternate, dNames, dIupac, dAlternate, dAll},
        (*Extraction of data from pre-processed files*)
        path = StringJoin[NotebookDirectory[], "\\Resources\\"];
        namesBad = Import[StringJoin[path, "namesList"], "List"];
        iupacBad = Import[StringJoin[path, "iupacList"], "List"];
        alternateBad = Import[StringJoin[path, "alternateList.json"], "JSON"];
        canonical = Import[StringJoin[path, "canonicalList"], "List"];
        (*Data contained within the files are shown in their original form*)
        names = PresetNoNested[namesBad, "_" -> " "];
        iupac = PresetNoNested[iupacBad, "_" -> " "];
        alternate = PresetNested[alternateBad, "_" -> " "];
        (*Creating dictionaries*)
        dNames = CreateAssoc[names, canonical];
        dIupac = CreateAssoc[iupac, canonical];
        dAlternate = CreateAssocAlt[alternate, canonical];
        (*Union of the 3 dictionaries*)
        dAll = Join[dNames, dIupac, dAlternate];
        dAll = KeyDrop[dAll, "NaN"];
        Return[dAll]
    ];


(*Return a List with the union of all kind of names (CommonName, AlternateNames, IUPACName, Canonical Name) for each molecule*)
ListMolecules[] :=
    Module[{path, namesBad, iupacBad, alternateBad, canonical, names, iupac, alternate, allNames},
        (*Extraction of data from pre-processed files*)
        path = StringJoin[NotebookDirectory[], "\\Resources\\"];
        namesBad = Import[StringJoin[path, "namesList"], "List"];
        iupacBad = Import[StringJoin[path, "iupacList"], "List"];
        alternateBad = Import[StringJoin[path, "alternateList.json"], "JSON"];
        canonical = Import[StringJoin[path, "canonicalList"], "List"];
        (*Data contained within the files are shown in their original form*)
        names = PresetNoNested[namesBad, "_" -> " "];
        iupac = PresetNoNested[iupacBad, "_" -> " "];
        alternate = PresetNested[alternateBad, "_" -> " "];
        (*Deletion of elements that didn't have the right nomenclature*)
        names = DeleteCases[names, "NaN"];
        iupac = DeleteCases[iupac, "NaN"];
        alternate = DeleteCases[Flatten[alternate], "NaN"];
        allNames = Join[names, iupac, alternate];
        Return[allNames]
    ];


(*Given any type of molecule name and a dictionary to search on returns a MoleculePlot/MoleculePlot3D depending on the value of typePlot.
	The theme string will be used to change the display theme of the molecule within the Manipulate.*)
GetMolecule[name_:String, dict_:Association, theme_:String, typePlot_:String] :=
    Module[{},
        (*Checking that the specified molecule name has a match in the dictionary*)
        If[MissingQ[dict[name]],
            (*No match, a default image is shown*)
            Return[Image[Import[StringJoin[NotebookDirectory[],"\\Resources\\Wolfram.png"]],ImageSize->{500,500}]]
            ,
            (*Return a plot depending on the value of the parameter*)
            If[typePlot == "2D",
                Return[MoleculePlot[Entity["Chemical", dict[name]], ImageSize -> {500,500}, PlotTheme -> theme, PlotLegends -> Automatic]]
                ,
                Return[MoleculePlot3D[Entity["Chemical", dict[name]], ImageSize -> {500,500}, PlotTheme -> theme, PlotLegends -> Automatic]]
            ]
        ]
    ];


(*Given a list of properties, a hyperlink is added to each of them and a google search is carried out using the CanonicalName to provide a detailed description*)
GetProperties[prt_:List] :=
    Module[{},
        (*Add an hyperlink to each molecule inside the list*)
        Table[
            AddLink[prt[[idx]], prt[[idx]]["CanonicalName"]],
            {idx, 1, Length[prt]}
        ]
    ];


(*This function takes one string x and one integer y  and returns a word with y characters missing from the word x.
	It returns the missing index too because it will be useful inside the hint function*)
RemoveChars[x_:String, y_:Integer] :=
    Module[{tmp, n, indexRemoved},
        (*Choose some random index to replace characters*)
        indexRemoved = RandomSample[Range[StringLength[x]], y];
        tmp = Characters[x];
        (*Replace characters with a certain index*)
        tmp = StringJoin[Table[If[MemberQ[indexRemoved, n],
            "_"
            ,
            tmp[[n]]
        ], {n, Length[tmp]}]];
        Return[{tmp, indexRemoved}];
    ]


(*This function takes 2 words and one list and returns the second word with a character that it was deleted from the RemoveChars function.
	It returns the indexes that are left to be used if called again*)
HintChar[completa_:String, incompleta_:String, indexRem_:List] :=
    Module[{index, charCompl, indexRemoved},
        (*Choose one index where the characters was removed*)
        index = RandomSample[indexRem, 1][[1]];
        indexRemoved = indexRem;
        (*Deletes the index from the list of indexes removed*)
        indexRemoved = Delete[indexRemoved, Position[indexRemoved, index]];
        charCompl = Characters[incompleta];
        (*Add again the characters choosen*)
        charCompl[[index]] = Characters[completa][[index]];
        Return[{StringJoin[charCompl], indexRemoved}];
    ]


(*This function takes a list of molecules' entity canonical names and returns one random of these*)
RandomMolecule[molecules_:List] :=
    Module[{mol},
        mol = RandomSample[molecules, 1][[1]];
        Return[mol];
    ]


(* ::Text:: *)
(*Auxiliary function*)


PresetNoNested[list_:List, rule_:Rule] :=
    Module[{},
        (*Replace the strings contained in the list (without nesting) according to the specified rule*)
        Return[Table[StringReplace[ToString[list[[n]]], rule], {n, 1, Length[list], 1}]];
    ];


PresetNested[list_:List, rule_:Rule] :=
    Module[{},
        (*Replace the strings contained in the list (with nesting) according to the specified rule*)
        Return[Table[Table[StringReplace[ToString[list[[n]][[m]]], rule], {m, 1, Length[list[[n]]], 1}], {n, 1, Length[list], 1}]];
    ];


CreateAssoc[names_:List, canonical_:List] :=
    Module[{assoc},
        (*Create the association between common name/iupac name and canonical name*)
        assoc = Association[
            Table[
                names[[idx]] -> canonical[[idx]],
                {idx, 1, Length[names], 1}
            ]
        ];
        Return[assoc]
    ];


CreateAssocAlt[names_:List, canonical_:List] :=
    Module[{assoc},
        (*Create the association between all alternative names and canonical name*)
        assoc = Association[
            Table[
                Table[
                    names[[i]][[j]] -> canonical[[i]],
                    {j, 1, Length[names[[i]]], 1}
                ],
                {i, 1, Length[names]}
            ]
        ];
        Return[assoc]
    ];


AddLink[enty_:Entity, name_:String] :=
    Module[{googleSearch},
        (*Adds an hyperlink to the single entity by building the search string "What is 'Canonical Name' in chemistry?" for google*)
        googleSearch = "https://www.google.it/search?q=";
        Hyperlink[enty, StringJoin[googleSearch, "What is ", UpdatePropertyLabel[name], " in chemistry?"]]
    ];


UpdatePropertyLabel = Module[{},
	(*
		Adds a space to each string before capital letters
		Example: "TestName" \[Rule] "Test Name"
	*)
    StringReplace[Except[WordBoundary, str_?UpperCaseQ] :> " " <> str]
];


End[];


EndPackage[];
