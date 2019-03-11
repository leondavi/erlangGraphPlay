
import pandas as ps
from enum import Enum
import matplotlib.pyplot as plot
from collections import Counter
import numpy as np
import csv

DEF_COL_TIME = 0
DEF_COL_SRC = 1
DEF_COL_DST = 2


class trace:

    def __init__(self,traceFileName,experimentName):
        self.trace = ps.read_csv(traceFileName)
        self.experimentName=experimentName

    def extract_statistics(self):
        statistics = dict()
        TimeColumn = self.trace.iloc[:,DEF_COL_TIME]
        SourcesColumn = self.trace.iloc[:,DEF_COL_SRC]
        DestinationsColumn = self.trace.iloc[:,DEF_COL_DST]

        statistics["# of unique sources"] = len(ps.unique(SourcesColumn))
        statistics["# of unique destinations"] = len(ps.unique(DestinationsColumn))

        UnifiedTxRxNodes = ps.concat([SourcesColumn,DestinationsColumn])

        UniqueAddresses = ps.unique(UnifiedTxRxNodes)

        statistics["# of unique addresses"] = len(UniqueAddresses)

        listOfPairs = self.two_columns_to_list_of_pairs(SourcesColumn,DestinationsColumn)
        statistics["# of unique requests"] = ps.unique(listOfPairs)

        self.generate_activity_histogram(UnifiedTxRxNodes,'CDF Nodes Activity')
        self.generate_activity_histogram(listOfPairs,'CDF Pairs (Edges) Activity')

        self.JointlyDistMat = np.zeros((len(UniqueAddresses),len(UniqueAddresses)),dtype=float)

        for idx,node in enumerate(UniqueAddresses):
            appearancesList = np.where(SourcesColumn==node)[0]
            NomOfOutNodes = len(appearancesList)
            if NomOfOutNodes > 0 :
                partVal = 1/len(appearancesList)
                DestList = []
                for idx in appearancesList:
                    self.JointlyDistMat[idx][np.where(UniqueAddresses==DestinationsColumn[idx])[0][0]]+=partVal

        self.statistics = statistics #save the dictionary




    def two_columns_to_list_of_pairs(self,ColA,ColB):
        Res = []
        for i in range(0,len(ColA)-1):
            Res.append((ColA[i],ColB[i]))
        return Res

    def generate_activity_histogram(self,givenList,PlotName):
        #plot.hist(x=,bins='auto',color='#0504aa',alpha=0.7,rwidth=0.85)
        labels,values = zip(*Counter(givenList).items())
        indexes = range(0,len(labels))
        valuesDist = [x/sum(values) for x in values]
        plot.figure()
        plot.ylabel('Probability')
        plot.title(PlotName)
        plot.bar(indexes, valuesDist, alpha=0.75, color="skyblue")
        fname = self.experimentName+"_"+PlotName+".png"
        plot.savefig(fname)

    def print_to_file(self):
        np.savetxt(self.experimentName+"_JointlyDistMatrix.csv",self.JointlyDistMat,delimiter=",")
        #saving the statitstics
        with open(self.experimentName+'_statistics.csv', 'w') as f:
            for key in self.statistics.keys():
                f.write("%s,%s\n" % (key, self.statistics[key]))


