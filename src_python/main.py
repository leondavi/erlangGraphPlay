from traces_extraction import *
from multiprocessing import Pool
from graphs import *


Experiments = dict()
# Experiments["traceC"] = "data/Trace_In_cluster_C_segment_9_{Time,Src,Dst}_.csv"
# Experiments["traceA"] = "data/Trace_In_cluster_A_segment_9_{Time,Src,Dst}_.csv"
# Experiments["traceB"] = "data/Trace_In_cluster_B_segment_9_{Time,Src,Dst}_.csv"
#
Experiments["demo"] = "data/small_check.csv"
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

    network_graph = NetGraph(CurrentTraceInst.statistics["# of unique addresses"],4)
    G = network_graph
    network_graph.simulate_simple_binary(CurrentTraceInst.trace,CurrentTraceInst.convert_hash)
    pos = graphviz_layout(G, prog='dot')
    nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'), node_size=500)
    nx.draw_networkx_labels(G, pos)
    nx.draw_networkx_edges(G, pos, edge_color='r', arrows=True)
    nx.draw_networkx_edges(G, pos, arrows=False)
    plt.show()
print("Load completed")


