#!/usr/bin/env wolframscript
getId = Function[fileName,
StringCases[fileName, Except["-"]..][[6]]];
memoryPlot = Function[
		{fileName},
		Module[
 		{data},		
		   data = Import[fileName,"CSV"];	
		   Print[StringReplace[fileName, ".txt"-> ".jpeg"]];
		  Export[StringReplace[fileName, ".txt"-> ".jpeg"],ListPlot[data,PlotLabel-> getId[fileName],
		  				 	  AxesLabel -> {"t/s", "Memory/Gb"}, PlotRange -> {3600}],
		  				 ImageResolution -> 200 ]]];

timePlot = Function[
		{fileName},
		Module[
 		{data},		
		   data = Import[fileName,"CSV"];
		   data = MapIndexed[{#2[[1]],#1}&, data];
		   Print[StringReplace[fileName, ".txt"-> ".jpeg"]];
		   Export[StringReplace[fileName, ".txt"-> ".jpeg"] ,NumberLinePlot[data,PlotLabel->getId[fileName],
											AxesLabel -> {"t/s"},
											PlotRange -> {0,3600}],
		  ImageResolution -> 200]]];

fileName = FindFile[$ScriptCommandLine[[2]]];
Print[getId[fileName]]
Which[StringMatchQ[fileName,"*timePlot_*.txt"],timePlot[fileName],
	StringMatchQ[fileName,"*memoryPlot_*.txt"], memoryPlot[fileName]];
