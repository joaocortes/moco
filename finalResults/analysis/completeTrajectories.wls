#!/usr/bin/env wolframscript
<<"wlsUtilities.wls";
runAll = Function[{},
       Map[timePlot[#]&,FileNames["../timePlot*txt", "", Infinity]];
       Map[memoryPlot[#]&,FileNames["../memoryPlot*txt", "", Infinity]];];

runAll[];