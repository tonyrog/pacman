%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    player driver
%%% @end
%%% Created :  11 Jun 2018 by Tony Rogvall <tony@rogvall.se>

-module(pacman_player).

-export([start/1,start/2]).

-record(state,
	{
	  id :: integer(),
	  position :: {integer(),integer()},
	  color = 1  :: 1|2|3|4,
	  keys = [],   %% list of keys pressed
	  dir,         %% heading
	  dirs = []    %% current available directions
	}).

start(Game) -> start(Game, 1).

start(Game,Color) ->
    spawn_link(
      fun() ->
	      {ok,Id,Pos} = pacman_maze:spawn_player(Game,Color),
	      loop(Game,
		   #state{ id=Id,
			   position=Pos,
			   color = Color
			 })
      end).

%% simple player loop

loop(Game,State) ->
    receive
	%% Keys
	{key_down, $q} -> 
	    quit;
	{key_down, $Q} -> 
	    quit;
	{key_down, K} ->
	    State1 = key_down(K,State),
	    Dir = select_direction(State1#state.keys,State1#state.dirs),
	    pacman_maze:set_direction(Game,State#state.id,Dir),
	    loop(Game,State1);
	{key_up,   K} -> 
	    State1 = key_up(K,State),
	    loop(Game,State1);
	%% hit a wall must make a decision
	{wall,Game,_Id,_Pos,Dir,Ds=[]} ->
	    loop(Game,State#state{dir=Dir,dirs=Ds});
	{wall,Game,Id,_Pos,Dir0,Ds=[Dir]} ->
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State#state{dir=Dir0,dirs=Ds});
	{wall,Game,Id,_Pos,Dir0,Ds} ->
	    Dir = select_direction(State#state.keys,Ds),
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State#state{dir=Dir0,dirs=Ds});
	%% junction is decision points
	{junction,Game,_Id,_Pos,Dir,[]} ->
	    loop(Game,State#state{dir=Dir,dirs=[]});
	{junction,Game,Id,_Pos,Dir0,Ds=[Dir]} ->
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State#state{dir=Dir0,dirs=Ds});
	{junction,Game,Id,_Pos,Dir0,Ds} ->
	    Ds1 = Ds -- [came_from(Dir0)],
	    Dir = select_direction(State#state.keys, Ds1),
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State#state{dir=Dir,dirs=Ds});
	{collision,Game,_Id,_Pos,_Dir,Parties} ->
	    io:format("collision ~p\n", [Parties]),
	    stop;
	Other ->
	    io:format("got ~p\n", [Other]),
	    loop(Game,State)
    end.

key_down(Key,State) ->
    State#state { keys = [Key|State#state.keys] }.

key_up(Key, State) ->
    State#state { keys = State#state.keys--[Key] }.

select_direction(Keys, Ds) ->
    first_direction(directions_from_keys(Keys),Ds).

directions_from_keys([left|Ks]) -> [{-1,0}|directions_from_keys(Ks)];
directions_from_keys([right|Ks]) -> [{1,0}|directions_from_keys(Ks)];
directions_from_keys([up|Ks]) -> [{0,-1}|directions_from_keys(Ks)];
directions_from_keys([down|Ks]) -> [{0,1}|directions_from_keys(Ks)];
directions_from_keys([_|Ks]) -> directions_from_keys(Ks);
directions_from_keys([]) -> [].

came_from({Dx,Dy}) ->
    {-Dx,-Dy}.

first_direction([D|Ds], Dirs) ->
    case lists:member(D, Dirs) of
	true -> D;
	false -> first_direction(Ds, Dirs)
    end;
first_direction([], _Dirs) ->
    {0,0}.
