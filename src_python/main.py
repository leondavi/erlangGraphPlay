from traces_extraction import *
from multiprocessing import Pool

def f(traceInst):
    traceInst.extract_statistics()
    traceInst.print_to_file()

Experiments = dict()
Experiments["traceC"] = "data/Trace_In_cluster_C_segment_9_{Time,Src,Dst}_.csv"
Experiments["traceA"] = "data/Trace_In_cluster_A_segment_9_{Time,Src,Dst}_.csv"
Experiments["traceB"] = "data/Trace_In_cluster_B_segment_9_{Time,Src,Dst}_.csv"
#
# Experiments["traceA"] = "data/small_check.csv"
# Experiments["traceB"] = "data/small_check.csv"
# Experiments["traceC"] = "data/small_check.csv"

print("Loading traces")

ExperimentsList = []
for ExpName, FileName in Experiments.items():
    #ExperimentsList.append(trace(ExpName,FileName))
    print("Experiment: "+ExpName)
    print("---------------------\n")
    CurrentTraceInst = trace(ExpName, FileName)
    CurrentTraceInst.extract_statistics()
    CurrentTraceInst.print_to_file()
    print("ended\n\n")
print("Load completed")

#parallel loop
# print("Initiating experiments (parallel run)")
# with Pool(len(ExperimentsList)) as p:
#     p.map(f,ExperimentsList)


