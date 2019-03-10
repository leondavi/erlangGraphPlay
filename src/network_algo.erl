%%%-------------------------------------------------------------------
%%% @author david
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2019 12:12 PM
%%%-------------------------------------------------------------------
-module(network_algo).
-author("david").



%% API
-export([test/0,generate_cubiq_mesh/1,list_of_edges_representation/1,generate_csv/1,generate_random_3D_indexes/2]).
-export([remove_vertices_by_percentage/3,generate_random_pairs/3,add_random_edges_by_percentage/3]).

test()->G = generate_cubiq_mesh(10),generate_random_pairs(G,10,10),list_of_edges_representation(G).

generate_cubiq_mesh(N)-> Graph = digraph:new(),NewGraph = generate_vertices(Graph,N),connect_mesh_edges(Graph,N,digraph:vertices(NewGraph)).

generate_vertices(Graph,N)-> generate_vertices_layer_z(Graph,{N,N,N}).

generate_vertices_layer_z(Graph,{_,_,0})->Graph;
generate_vertices_layer_z(Graph,{X,Y,Zi})-> NewGraph = generate_vertices_layer_y(Graph,{X,Y},Zi),generate_vertices_layer_z(NewGraph,{X,Y,Zi-1}).
generate_vertices_layer_y(Graph,{_,0},_)->Graph;
generate_vertices_layer_y(Graph,{X,Yi},Z)-> NewGraph = generate_vertices_layer_x(Graph,X,Yi,Z),generate_vertices_layer_y(NewGraph,{X,Yi-1},Z).
generate_vertices_layer_x(Graph,0,_,_)-> Graph;
generate_vertices_layer_x(Graph,Xi,Y,Z)-> digraph:add_vertex(Graph,{Xi,Y,Z}),generate_vertices_layer_x(Graph,Xi-1,Y,Z).

connect_mesh_edges(Graph,_,[])->Graph;
connect_mesh_edges(Graph,N,Vertices)-> {Xc,Yc,Zc} = hd(Vertices),
                                    % Genrate List of 6 possible neighbors
                                      NeighborsList = [{Xc-1,Yc,Zc},{Xc+1,Yc,Zc},
                                                       {Xc,Yc-1,Zc},{Xc,Yc+1,Zc},
                                                       {Xc,Yc,Zc-1},{Xc,Yc,Zc+1}],
                                      connect_with_neighbors(Graph,N,{Xc,Yc,Zc},NeighborsList),
                                      connect_mesh_edges(Graph,N,tl(Vertices)).
connect_with_neighbors(Graph,_,_,[])->Graph;
connect_with_neighbors(Graph,N,Center,Neighbors)-> %filtering the valid neighbors and create edge between those and the center
                                                case hd(Neighbors) of
                                                  {Xn,_,_} when Xn < 1 ; Xn>N -> do_nothing;
                                                  {_,Yn,_} when Yn < 1 ; Yn>N -> do_nothing;
                                                  {_,_,Zn} when Zn < 1 ; Zn>N -> do_nothing;
                                                  {Xn,Yn,Zn}-> digraph:add_edge(Graph,Center,{Xn,Yn,Zn})
                                                end, connect_with_neighbors(Graph,N,Center,tl(Neighbors)).

list_of_edges_representation(Graph)->EdgesList = digraph:edges(Graph),iterate_over_edges(Graph,EdgesList,[]).
iterate_over_edges(_,[],EdgesStringList)->EdgesStringList;
iterate_over_edges(Graph,EdgesList,EdgesStringList)-> StartVertex = element(2,digraph:edge(Graph,hd(EdgesList))),
                                                      EndVertex = element(3,digraph:edge(Graph,hd(EdgesList))),
                                                      NewEdgesStringList = EdgesStringList++[io_lib:format("(~p) -> (~p),", [StartVertex,EndVertex])],
                                                      iterate_over_edges(Graph,tl(EdgesList),NewEdgesStringList).

generate_csv(Graph)->EdgesRep = list_of_edges_representation(Graph),FileName = "graph.csv",file:delete(FileName),generate_csv_write_to_file(FileName,EdgesRep).
generate_csv_write_to_file(FileName,[])->file:close(FileName),ended;
generate_csv_write_to_file(FileName,ListOfLines)-> [H | T] = ListOfLines,
                                                    file:write_file(FileName,io_lib:fwrite("~s\n",[lists:flatten([H])]),[append]),
                                                    generate_csv_write_to_file(FileName,T).

generate_random_3D_indexes(MaxN,Instances)-> generate_random_3D_indexes_rec(MaxN,[],Instances).
generate_random_3D_indexes_rec(_,List,0) -> List;
generate_random_3D_indexes_rec(MaxN,List,Instances) -> generate_random_3D_indexes_rec(MaxN,List++[{rand:uniform(MaxN),rand:uniform(MaxN),rand:uniform(MaxN)}],Instances-1).

remove_vertices_by_percentage(Graph,MaxN,Percentage)-> NumToRemove = trunc(length(digraph:vertices(Graph))*(Percentage/100)),io:format("~p\n",[NumToRemove]),
                                                      digraph:del_vertices(Graph,generate_random_3D_indexes(MaxN,NumToRemove)).

generate_random_pairs(Graph,MaxN,NumOfPairs)-> removeNeighborsOrSelf(Graph,list_to_listofpairs(generate_random_3D_indexes(MaxN,trunc(NumOfPairs*2)),[]),[]).

list_to_listofpairs([],ListOfPairs) -> ListOfPairs;
list_to_listofpairs(List,ListOfPairs) -> [H | T] = List, [H2 | T2] = T, list_to_listofpairs(T2,ListOfPairs++[{H,H2}]).

removeNeighborsOrSelf(_,[],NewList) -> NewList;
removeNeighborsOrSelf(Graph,ListOfPairs,NewList)-> [H | T] = ListOfPairs , IsNeighbor = lists:member(element(1,H),digraph:in_neighbours(Graph,element(2,H))),
                                                 if
                                                   element(1,H)  == element(2,H) -> removeNeighborsOrSelf(Graph,T,NewList);
                                                   IsNeighbor ->  removeNeighborsOrSelf(Graph,T,NewList);
                                                   true -> removeNeighborsOrSelf(Graph,T,NewList++[H])
                                                 end.

add_random_edges_by_percentage(Graph,Maxn,Perc) -> NumOfEdgesToAdd = round(length(digraph:vertices(Graph))*(Perc/100)),
                                                  add_edges_given_vertices_pairslist(Graph,generate_random_pairs(Graph,Maxn,NumOfEdgesToAdd)).

add_edges_given_vertices_pairslist(_,[])->done;
add_edges_given_vertices_pairslist(Graph,VerticesPairsList) -> [H | T] = VerticesPairsList,
                                                              digraph:add_edge(Graph,element(1,H),element(2,H)), %V1->V2
                                                              digraph:add_edge(Graph,element(2,H),element(1,H)), %V2->V1
                                                              add_edges_given_vertices_pairslist(Graph,T).