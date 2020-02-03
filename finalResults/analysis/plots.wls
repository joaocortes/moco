#!/usr/bin/env wolframscript
<<"wlsUtilities.wls";
fileName = FindFile[$ScriptCommandLine[[2]]];
Print[getId[fileName]]
Which[StringMatchQ[fileName,"*timePlot_*.txt"],timePlot[fileName],
	StringMatchQ[fileName,"*memoryPlot_*.txt"], memoryPlot[fileName]];
