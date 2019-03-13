
import pandas as ps
from enum import Enum
import matplotlib.pyplot as plot
from collections import Counter
import numpy as np
import csv
from threading import Thread
import matplotlib.cm as cm
from scipy.sparse import csr_matrix

DEF_COL_TIME = 0
DEF_COL_SRC = 1
DEF_COL_DST = 2


class trace:

    def __init__(self,experimentName,traceFileName):
        self.trace = ps.read_csv(traceFileName)
        self.experimentName = experimentName
        self.expStrBlock = "["+self.experimentName+"] "

    def extract_statistics(self):
        statistics = dict()
        TimeColumn = self.trace.iloc[:,DEF_COL_TIME]
        SourcesColumn = self.trace.iloc[:,DEF_COL_SRC]
        DestinationsColumn = self.trace.iloc[:,DEF_COL_DST]

        print(self.expStrBlock+"Converting strings to integer representation")
        UniqueAddressesInt,SourcesColumn,DestinationsColumn,UnifiedTxRxNodes = self.convert_string_column_to_indexes(SourcesColumn,DestinationsColumn)

        print("# of unique addresses: "+str(len(UniqueAddressesInt)))
        print(self.expStrBlock+"Calculating unique src/dst")

        statistics["# of unique sources"] = len(ps.unique(SourcesColumn))
        statistics["# of unique destinations"] = len(ps.unique(DestinationsColumn))
        statistics["# of unique addresses"] = len(UniqueAddressesInt)

        listOfPairs = self.two_columns_to_list_of_pairs(SourcesColumn,DestinationsColumn)
        print("# num of pairs "+str(len(listOfPairs)))
        print(self.expStrBlock+"Calculating unique requests")

        statistics["# of unique requests"] = len(ps.unique(listOfPairs))

        self.JointlyDistMat_Calc(UniqueAddressesInt,listOfPairs)

        self.generateHistograms(UnifiedTxRxNodes,listOfPairs)

        self.statistics = statistics #save the dictionary

        print(self.expStrBlock+"Analayze completed")


    def convert_string_column_to_indexes(self,SourcesColumnStr,DestinationsColumnStr):
        UnifiedList = ps.concat([SourcesColumnStr, DestinationsColumnStr])
        print(self.expStrBlock + "Generating array of uniques")
        UniqueAddresses = ps.unique(UnifiedList)
        UniqueAddressesInt = []

        print(self.expStrBlock + "Generating hash table")
        convert_hash = dict()
        for idx,address in enumerate(UniqueAddresses):
            convert_hash[address] = idx
            UniqueAddressesInt.append(idx)

        print(self.expStrBlock + "Generating new columns")
        SourcesIntCol = np.zeros(shape=SourcesColumnStr.shape)
        DestinationsIntCol =  np.zeros(shape=DestinationsColumnStr.shape)

        for idx in range(SourcesColumnStr.shape[0]):
            SourcesIntCol[idx] = convert_hash[SourcesColumnStr[idx]]
            DestinationsIntCol[idx] = convert_hash[DestinationsColumnStr[idx]]


        UnifiedListInt = np.concatenate((SourcesIntCol,DestinationsIntCol))

        return UniqueAddressesInt,SourcesIntCol,DestinationsIntCol,UnifiedListInt

    def generateHistograms(self,UnifiedTxRxNodes,listOfPairs):
        print(self.expStrBlock+"Generating nodes activity histogram")
        self.generate_activity_histogram(UnifiedTxRxNodes, 'Nodes Activity')
        print(self.expStrBlock+"Generating edges activity histogram")
        self.generate_activity_histogram(listOfPairs, 'Pairs (Edges) Activity')


    def JointlyDistMat_Calc(self,UniqueAddresses,listOfPairs):
        print(self.expStrBlock+"Generating Jointly Distribution Matrix")
        self.JointlyDistMat = csr_matrix((len(UniqueAddresses), len(UniqueAddresses)),dtype=float)

        for pair in listOfPairs:
            row = np.array([pair[0]])
            col = np.array([pair[1]])
            data = np.array([1])
            tmpMat = csr_matrix((data,(row,col)),shape=(len(UniqueAddresses), len(UniqueAddresses)),dtype=float)
            self.JointlyDistMat = self.JointlyDistMat+tmpMat

    def two_columns_to_list_of_pairs(self,ColA,ColB):
        Res = []
        for idx,val in enumerate(ColA):
            Res.append((val,ColB[idx]))
        return Res

    def generate_activity_histogram(self,givenList,PlotName):
        #plot.hist(x=,bins='auto',color='#0504aa',alpha=0.7,rwidth=0.85)
        labels,values = zip(*Counter(givenList[0:10000]).items())
        indexes = range(0,len(labels))
        valuesDist = [x/sum(values) for x in values]
        plot.figure()
        plot.ylabel('Probability')
        plot.title(PlotName+" Distribution")
        plot.bar(indexes, valuesDist, alpha=0.75, color="skyblue")
        fname = self.experimentName+"_bar_"+PlotName+".png"
        plot.savefig(fname)
        plot.figure()
        plot.ylabel('Occurances')
        plot.title(PlotName+" Number Of Transmits")
        plot.hist(x=values, bins='auto', alpha=0.75, color="skyblue")
        fname = self.experimentName + "_hist_" + PlotName + ".png"
        plot.savefig(fname)


    def print_to_file(self):
        print(self.expStrBlock+"Saving results to files")
        plot.figure()
        #plot.imshow(self.JointlyDistMat,interpolation='bilinear')
        #plot.savefig(self.experimentName+"_JointlyDistMatrix_HeatMap.png")
        #np.savetxt(self.experimentName+"_JointlyDistMatrix.csv",self.JointlyDistMat,delimiter=",")
        self.save_sparseMatcsv(self.experimentName+"_JointlyDistMatrix.csv",self.JointlyDistMat)
        #saving the statitstics
        with open(self.experimentName+'_statistics.csv', 'w') as f:
            for key in self.statistics.keys():
                f.write("%s,%s\n" % (key, self.statistics[key]))

    def save_sparseMatcsv(self,filename,SparseMat):
        with open(filename, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['x', 'y', 'value'])
            MatCoo = SparseMat.tocoo()
            for idx,data in enumerate(MatCoo.data):
                writer.writerow([MatCoo.row[idx],MatCoo.col[idx],data])
