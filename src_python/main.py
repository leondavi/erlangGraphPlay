from traces_extraction import *
from multiprocessing import Pool
from graphs import *
import time


Experiments = dict()
Experiments["traceA"] = "data/Trace_In_cluster_A_segment_9_{Time,Src,Dst}_.csv"
#Experiments["traceB"] = "data/Trace_In_cluster_B_segment_9_{Time,Src,Dst}_.csv"
# Experiments["traceC"] = "data/Trace_In_cluster_C_segment_9_{Time,Src,Dst}_.csv"

#
# Experiments["demo"] = "data/small_check.csv"
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
    #CurrentTraceInst.print_to_file()
    print("ended\n\n")

    network_graph = NetGraph(CurrentTraceInst.statistics["# of unique addresses"],4)
    G = network_graph
    starting_time = time.time()
    avg = network_graph.simulate_simple_binary(CurrentTraceInst.trace,CurrentTraceInst.convert_hash)
    print("Simple binary tree, path average: "+str(avg)+" time took: "+str(time.time()-starting_time)+"sec")

    starting_time = time.time()
    avg = network_graph.simulate_algo_activity_dist(CurrentTraceInst.trace,CurrentTraceInst.convert_hash,CurrentTraceInst.nodes_activity_df)
    print("Order of tree by activity, path average: "+str(avg)+" time took: "+str(time.time()-starting_time)+"sec")

    starting_time = time.time()
    avg = network_graph.simulate_algo_activity_dist_add_random_shortcuts(CurrentTraceInst.trace,CurrentTraceInst.convert_hash,CurrentTraceInst.nodes_activity_df,rand_shortctus=100)
    print("Order of tree by activity with 1k random shortcuts, path average: "+str(avg)+" time took: "+str(time.time()-starting_time)+"sec")

    starting_time = time.time()
    avg = network_graph.simulate_algo_activity_dist_add_random_shortcuts(CurrentTraceInst.trace,CurrentTraceInst.convert_hash,CurrentTraceInst.nodes_activity_df,rand_shortctus=1000)
    print("Order of tree by activity with 10k random shortcuts, path average: "+str(avg)+" time took: "+str(time.time()-starting_time)+"sec")

    starting_time = time.time()
    avg = network_graph.simulate_algo_activity_dist_add_random_shortcuts(CurrentTraceInst.trace,CurrentTraceInst.convert_hash,CurrentTraceInst.nodes_activity_df,rand_shortctus=10000)
    print("Order of tree by activity with 100k random shortcuts, path average: "+str(avg)+" time took: "+str(time.time()-starting_time)+"sec")

print("Load completed")


