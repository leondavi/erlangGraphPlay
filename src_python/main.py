from traces_extraction import *
from multiprocessing import Pool

def f(traceInst):
    traceInst.extract_statistics()
    traceInst.print_to_file()

Experiments = dict()
Experiments["traceA"] = "data/Trace_In_cluster_A_segment_9_{Time,Src,Dst}_.csv"
Experiments["traceB"] = "data/Trace_In_cluster_B_segment_9_{Time,Src,Dst}_.csv"
Experiments["traceC"] = "data/Trace_In_cluster_C_segment_9_{Time,Src,Dst}_.csv"
#
# Experiments["traceA"] = "data/small_check.csv"
# Experiments["traceB"] = "data/small_check.csv"
# Experiments["traceC"] = "data/small_check.csv"


ExperimentsList = []
for ExpName, FileName in Experiments.items():
    ExperimentsList.append(trace(ExpName,FileName))

#parallel loop

with Pool(len(ExperimentsList)) as p:
    p.map(f,ExperimentsList)


