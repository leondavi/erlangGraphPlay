
import networkx as nx
from networkx.drawing.nx_agraph import write_dot, graphviz_layout
import matplotlib.pyplot as plt
import math
import random

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

    def simulate_simple_binary(self,trace,nodes_hash,samples=10000):
        sum_of_hopes = 0
        if(samples>trace.shape[0]):
            samples = trace.shape[0]
        trace_subsampled = trace.sample(n=samples)
        for index,row in trace_subsampled.iterrows():
            source_add = nodes_hash[row.values[self.TRACE_SRC]]
            destination_add = nodes_hash[row.values[self.TRACE_DST]]
            sum_of_hopes += nx.shortest_path_length(self.tree,source=source_add,target=destination_add)
        self.simple_binary_avg = sum_of_hopes/samples
        return self.simple_binary_avg

   # Taking the nodes with maximal activity to be in lower levels
    def simulate_algo_activity_dist(self,trace,nodes_hash,nodes_activity_distribution,samples=10000):
        if (samples > trace.shape[0]):
            samples = trace.shape[0]
        node_by_dist = self.hash_by_distribution(nodes_hash,nodes_activity_distribution)
        sum_of_hopes = 0
        trace_subsampled = trace.sample(n=samples)
        for index, row in trace_subsampled.iterrows():
            source_add = node_by_dist[nodes_hash[row.values[self.TRACE_SRC]]]
            destination_add = node_by_dist[nodes_hash[row.values[self.TRACE_DST]]]
            sum_of_hopes += nx.shortest_path_length(self.tree, source=source_add, target=destination_add)
        self.simple_activity_dist_avg = sum_of_hopes / samples
        return self.simple_activity_dist_avg

    def simulate_algo_activity_dist_add_random_shortcuts(self,trace,nodes_hash,nodes_activity_distribution,samples=10000,rand_shortctus=1000):
        if (samples > trace.shape[0]):
            samples = trace.shape[0]
        duplicated_graph = self.tree
        node_by_dist = self.hash_by_distribution(nodes_hash,nodes_activity_distribution)
        non_edges_list = list(nx.non_edges(duplicated_graph))
        if(len(non_edges_list) < rand_shortctus):
            rand_shortctus = len(non_edges_list)
        non_edges_list_to_add = random.sample(non_edges_list,rand_shortctus)
        for edge in non_edges_list_to_add:
            duplicated_graph.add_edge(edge[0], edge[1])
        sum_of_hopes = 0
        trace_subsampled = trace.sample(n=samples)
        for index, row in trace_subsampled.iterrows():
            source_add = node_by_dist[nodes_hash[row.values[self.TRACE_SRC]]]
            destination_add = node_by_dist[nodes_hash[row.values[self.TRACE_DST]]]
            sum_of_hopes += nx.shortest_path_length(duplicated_graph, source=source_add, target=destination_add)
        self.simple_activity_dist_rand_shortcuts_avg = sum_of_hopes / samples
        return self.simple_activity_dist_rand_shortcuts_avg

    def hash_by_distribution(self,nodes_hash,nodes_activity_distribution):
        node_by_distribution = dict()
        tree_node_id = 0
        for index,rows in nodes_activity_distribution.iterrows():
            node_by_distribution[rows[0]] = tree_node_id
            tree_node_id += 1

        return node_by_distribution







