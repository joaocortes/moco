getId = Function[fileName,
StringCases[fileName, Except["-"]..][[6]]];
getLabel = Function[fileName,
Module[{id = getId[fileName], algorithm},
	   algorithm = StringCases[StringCases[FileBaseName[fileName], Except["-"]..][[7]],"SC_S"~~alg__:> alg ][[1]];
StringJoin["id = ", id, ",alg = ", algorithm]]]
memoryPlot = Function[
		{fileName},
		Module[
 		{data},		
		   data = Import[fileName,"CSV"];	
		   Print[StringReplace[fileName, ".txt"-> ".jpeg"]];
		  Export[StringReplace[fileName, ".txt"-> ".jpeg"],ListLinePlot[data,PlotLabel-> getLabel[fileName],
		  				 	  AxesLabel -> {"t/s", "Memory/Gb"}, PlotRange -> {{0, 3600 , All}},
							  GridLines -> Automatic],
		  				 ImageResolution -> 200 ]]];

timePlot = Function[
		{fileName},
		Module[
 		{data},		
		   data = Import[fileName,"CSV"];
		   data = MapIndexed[{#2[[1]],#1}&, data];
		   Print[StringReplace[fileName, ".txt"-> ".jpeg"]];
		   Export[StringReplace[fileName, ".txt"-> ".jpeg"] ,NumberLinePlot[data,PlotLabel->getLabel[fileName],
											AxesLabel -> {"t/s"},
											PlotRange -> {0,3600}],
		  ImageResolution -> 200]]];

