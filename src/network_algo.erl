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
-export([test/0,generate_cubiq_mesh/1,list_of_edges_representation/1]).

test()->G = generate_cubiq_mesh(10),list_of_edges_representation(G).

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
                                                      NewEdgesStringList = EdgesStringList++lists:flatten(io_lib:format("(~p) -> (~p),", [StartVertex,EndVertex])),
                                                      iterate_over_edges(Graph,tl(EdgesList),NewEdgesStringList).