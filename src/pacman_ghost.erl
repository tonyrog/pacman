%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Ghost driver
%%% @end
%%% Created :  5 Sep 2016 by Tony Rogvall <tony@rogvall.se>

-module(pacman_ghost).

-export([start/1,start/2,start/3,start/4]).

-record(state,
	{
	  id :: integer(),
	  position :: {integer(),integer()},
	  color = 1  :: 1|2|3|4,
	  speed = 1  :: 1|2|3|4|6|8,
	  scared = false,
	  style  = random :: random | left | right
	}).

start(Game) ->
    start(Game, 1).

start(Game, Color) ->
    start(Game, Color, 1).

start(Game, Color, Speed) ->
    start(Game, Color, Speed, random).

start(Game, Color, Speed, Style) ->
    spawn_link(fun() ->
		       {ok,Id,Pos} = pacman_maze:spawn_ghost(Game,Color,Speed),
		       loop(Game, #state{ id=Id,
					  position=Pos,
					  color = Color, 
					  speed = Speed,
					  style = Style })
	       end).

%% simple ghost
%% 1 - 
%
%%

loop(Game, State) ->
    receive
	%% hit a wall must make a decision
	{wall,Game,_Id,_Pos,_Dir,[]} ->
	    loop(Game,State);
	{wall,Game,Id,_Pos,_Dir0,[Dir]} ->
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State);
	{wall,Game,Id,_Pos,Dir0,Ds} ->
	    Ds1 = Ds -- [came_from(Dir0)],
	    Dir = select_direction(State#state.style, Ds1),
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State);
	%% junction is decision points
	{junction,Game,_Id,_Pos,_Dir0,[]} ->
	    loop(Game,State);
	{junction,Game,Id,_Pos,_Dir0,[Dir]} ->
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State);
	{junction,Game,Id,_Pos,Dir0,Ds} ->
	    Ds1 = Ds -- [came_from(Dir0)],
	    Dir = select_direction(State#state.style, Ds1),
	    pacman_maze:set_direction(Game,Id,Dir),
	    loop(Game,State);
	{collision,Game,Id,Pos,Dir,Parties} ->
	    io:format("collision ~p\n", [Parties]),
	    stop;
	Other ->
	    io:format("got ~p\n", [Other]),
	    loop(Game,State)
    end.

select_direction(random, Ds) ->
    N = length(Ds),
    lists:nth(rand:uniform(N), Ds);
select_direction(left, Ds) ->
    case first_direction([{-1,0},{0,1},{1,0},{0,-1}], Ds) of
	false -> select_direction(random, Ds);
	Dir -> Dir
    end;
select_direction(right, Ds) ->
    case first_direction([{1,0},{0,-1},{-1,0},{0,1}], Ds) of
	false -> select_direction(random, Ds);
	Dir -> Dir
    end.
    
came_from({Dx,Dy}) ->
    {-Dx,-Dy}.

first_direction([D|Ds], Dirs) ->
    case lists:member(D, Dirs) of
	true -> D;
	false -> first_direction(Ds, Dirs)
    end;
first_direction([], _Dirs) ->
    false.


	    
	    
