
import networkx as nx
from networkx.drawing.nx_agraph import write_dot, graphviz_layout
import matplotlib.pyplot as plt
import math

class NetGraph:
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

    def simulate(self,trace,nodes_hash):




degree_seq = [4,1,1,1,1]
#G = nx.degree_sequence_tree(degree_seq)
G = NetGraph(NumOfNodes=25,BranchingFactor=4).tree
#G = nx.balanced_tree(4,4)

pos = graphviz_layout(G,prog='dot')
nx.draw_networkx_nodes(G, pos, cmap=plt.get_cmap('jet'),node_size = 500)
nx.draw_networkx_labels(G, pos)
nx.draw_networkx_edges(G, pos, edge_color='r', arrows=True)
nx.draw_networkx_edges(G, pos, arrows=False)
plt.show()



