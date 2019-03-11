from traces_extraction import *
from multiprocessing import Pool

def f(traceInst):
    traceInst.extract_statistics()
    traceInst.print_to_file()

Experiments = dict()
Experiments["A"] = "data/small_check.csv"
Experiments["B"] = "data/small_check.csv"
Experiments["C"] = "data/small_check.csv"

ExperimentsList = []
for ExpName, FileName in Experiments.items():
    ExperimentsList.append(trace(ExpName,FileName))

#parallel loop

with Pool(len(ExperimentsList)) as p:
    p.map(f,ExperimentsList)


