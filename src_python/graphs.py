
import networkx as nx
from networkx.drawing.nx_agraph import write_dot, graphviz_layout
import matplotlib.pyplot as plt
import math

class NetGraph:
    TRACE_TIME = 0
    TRACE_SRC = 1
    TRACE_DST = 2

    def __init__(self,NumOfNodes,BranchingFactor):
        self.treeHeight = self.calculate_tree_height(NumOfNodes,BranchingFactor)
        self.tree = self.remove_redundant_leaves(nx.balanced_tree(BranchingFactor,self.treeHeight),NumOfNodes)


    def calculate_tree_height(self,numOfNodes,tree_branching_factor):
        return int(math.ceil(math.log(numOfNodes,tree_branching_factor)))

    def remove_redundant_leaves(self,G,NumOfNodes):
        leaves_list = []
        while len(G.nodes()) > NumOfNodes:
            if leaves_list == []:
                leaves_list = self.get_leaves(G)
            G.remove_node(leaves_list.pop())
        return G

    def get_leaves(self,G):
        leavesList = []
        for tpl in G.degree(G.nodes()):
            if tpl[1] == 1 : #Then it's leaf
                leavesList.append(tpl[0])
        return leavesList

    def simulate_simple_binary(self,trace,nodes_hash,samples=100):
        sum_of_hopes = 0
        trace_subsampled = trace.sample(n=samples)
        for index,row in trace_subsampled.iterrows():
            source_add = nodes_hash[row.values[self.TRACE_SRC]]
            destination_add = nodes_hash[row.values[self.TRACE_DST]]
            sum_of_hopes += nx.shortest_path_length(self.tree,source=source_add,target=destination_add)
        self.simple_binary_avg = sum_of_hopes/samples
        return self.simple_binary_avg

   # Taking the nodes with maximal activity to be in lower levels
   #  def simulate_algo(self,trace,nodes_hash,distribution_mat,samples=100):
   #
   #  def hash_by_distribution(self,nodes_hash,distribution):
   #      address_by_dist = dict()
   #      for






