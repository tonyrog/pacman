%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Pacman maze server
%%% @end
%%% Created :  3 Sep 2016 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(pacman_maze).

-behaviour(gen_server).

-include_lib("epx/include/epx.hrl").
-include_lib("epx/include/epx_image.hrl").

%% API
-export([start/0, start_link/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(TICK, 50).

-type position() :: {integer(),integer()}.

-record(images,
	{
	  ghost1,
	  ghost2,
	  ghostscared1,
	  ghostscared2,
	  pacman,
	  pacman_up,
	  pacman_left,
	  pacman_right,
	  pacman_down
	 }).

-record(entity,
	{
	  id   :: integer(),
	  type :: player | ghost | sprite,
	  pid :: pid(),
	  mon :: reference(),
	  color :: integer(),
	  lifes = 3 :: integer(),
	  x   :: integer(),
	  y   :: integer(),
	  dx  :: integer(),
	  dy  :: integer(),
	  idx :: integer(),
	  idy :: integer(),
	  speed = 1 :: integer()
	}).

-record(profile,
	{
	  background_color = {255,0,0,0},
	  solid_color      = {255,0,255,100},
	  maze_color       = {255,32,192,255},
	  dot_color        = {255,192,192,0},
	  big_dot_color    = 192
	}).

-record(state, 
	{
	  width   :: integer(),
	  height  :: integer(),
	  maze    :: #epx_pixmap{},
	  window  :: #epx_window{},
	  pixels  :: #epx_pixmap{},
	  backend :: #epx_backend{},
	  images  :: #images{},
	  entities :: table(),
	  entity_map :: table(),
	  player_start = [] :: [position()],
	  ghost_start  = [] :: [position()],
	  profile :: #profile{},
	  redraw = false :: boolean(),
	  dbigdotcolor  = -2
	}).

-type table() :: term().

-define(WALL_LEFT,  16#01).
-define(WALL_ABOVE, 16#02).
-define(WALL_RIGHT, 16#04).
-define(WALL_BELOW, 16#08).
-define(FOOD_SMALL, 16#10).
-define(FOOD_BIG,   16#20).
-define(SOLID,      16#40).

-define(IS_WALL_LEFT(Z),  (((Z) band ?WALL_LEFT) =:= ?WALL_LEFT)).
-define(IS_WALL_RIGHT(Z), (((Z) band ?WALL_RIGHT) =:= ?WALL_RIGHT)).
-define(IS_WALL_ABOVE(Z), (((Z) band ?WALL_ABOVE) =:= ?WALL_ABOVE)).
-define(IS_WALL_BELOW(Z), (((Z) band ?WALL_BELOW) =:= ?WALL_BELOW)).
-define(IS_SOLID(Z),      (((Z) band ?SOLID) =:= ?SOLID)).

-define(NO_WALL_LEFT(Z),  (((Z) band ?WALL_LEFT) =:= 0)).
-define(NO_WALL_RIGHT(Z), (((Z) band ?WALL_RIGHT) =:= 0)).
-define(NO_WALL_ABOVE(Z), (((Z) band ?WALL_ABOVE) =:= 0)).
-define(NO_WALL_BELOW(Z), (((Z) band ?WALL_BELOW) =:= 0)).

-define(IS_FOOD_SMALL(Z), (((Z) band ?FOOD_SMALL) =:= ?FOOD_SMALL)).
-define(IS_FOOD_BIG(Z),   (((Z) band ?FOOD_BIG) =:= ?FOOD_BIG)).

-define(BlockSize, 24). %% in pixels


demo() ->
    {ok,Game} = start(),
    pacman_ghost:start(Game, 1, 3),
    pacman_ghost:start(Game, 2, 3),
    pacman_ghost:start(Game, 3, 3),
    pacman_ghost:start(Game, 4, 3),
    pacman_player:start(Game).


sign(X) when is_number(X) ->
    if X < 0 -> -1;
       X > 0 -> 1;
       true -> 0
    end.

start() ->
    epx:start(),
    gen_server:start(?MODULE, [], []).

start_link() ->
    epx:start(),
    gen_server:start_link(?MODULE, [], []).

%% Player protocol:
%%   Process will get casts from server containing
%%   {collision, wall, {X,Y}, [D1..D4]}
%%   {collision, ghost, {X,Y}, []}
%%   Dx = -1,0,1  Dy=-1,0,1

%% "spawn" a player, the caller must be the player process
spawn_player(Game) ->
    spawn_player(Game,1).
spawn_player(Game,Color) ->
    gen_server:call(Game, {add_entity,player,self(),Color,1}).

%% "spawn" a ghost, the caller must be the ghost process
spawn_ghost(Game,Color) ->
    spawn_ghost(Game,Color,1).
spawn_ghost(Game,Color,Speed) ->
    gen_server:call(Game, {add_entity,ghost,self(),Color,Speed}).
set_direction(Game,Id,Dir) ->
    gen_server:call(Game, {set_direction,self(),Id,Dir}).

%% initialize maze
init([]) ->
    {H,W,Data,Ps,Gs} = ascii_to_data(maze_level_4()),
    io:format("player starts = ~p\n", [Ps]),
    io:format("ghost starts = ~p\n", [Gs]),
    Width  = W*?BlockSize,
    Height = H*?BlockSize,
    %% setup a maze pixmap from data
    Maze = epx:pixmap_create(W, H, a8),
    ok = epx:pixmap_put_pixels(Maze,0,0,W,H,a8,Data),
    {Window,Pixels,Backend} = setup_window(Width,Height),
    epx:window_attach(Window, Backend),
    epx:pixmap_attach(Pixels, Backend),
    Images = load_images(),
    Entities = ets:new(entities, [{keypos,#entity.id}]),
    EntityMap = ets:new(entity_map, []),
    State = 
	#state {
	   width = Width,
	   height = Height,
	   maze = Maze,
	   player_start = Ps,
	   ghost_start = Gs,
	   entities = Entities,
	   entity_map = EntityMap,
	   window = Window,
	   pixels = Pixels,
	   images = Images,
	   backend = Backend,
	   profile = #profile{}
	  },
    State1 = schedule_redraw(State),
    erlang:start_timer(?TICK, self(), tick),
    {ok, State1}.

handle_call({add_entity,Type,Pid,Color,Speed}, _From, State) ->
    {X,Y} = select_start_position(Type,State),
    Id = make_entity_id(State#state.entities),
    E = #entity { id = Id,
		  type = Type,
		  pid = Pid,
		  mon = erlang:monitor(process, Pid),
		  color = Color,
		  speed = Speed,
		  x   = X,
		  y   = Y,
		  dx  = 0,
		  dy  = 0
		},
    Code = entity_code(E, State),
    Rs = routes(Code),
    E#entity.pid ! {junction,self(),E#entity.id,{X,Y},{0,0},Rs},
    map_entity(E,State#state.entity_map),
    save_entity(E, State#state.entities),
    State1 = schedule_redraw(State),
    {reply, {ok,Id,{X,Y}}, State1};
handle_call({set_direction,Pid,Id,{Dx,Dy}}, _From, State) ->
    case load_entity(Id,State#state.entities) of
	false ->
	    {reply, {error,enoent}, State};
	E when E#entity.pid =/= Pid ->
	    {reply, {error,not_owner}, State};
	E ->
	    Dx1 = sign(Dx),
	    Dy1 = sign(Dy),
	    Code = entity_code(E, State),
	    unmap_entity(E,State#state.entity_map),
	    E1 = case lists:member({Dx1,Dy1}, routes(Code)) of
		     true ->
			 step_entity(E, Dx, Dy);
		     false ->
			 E
		 end,
	    map_entity(E1,State#state.entity_map),
	    save_entity(E1, State#state.entities),
	    {reply, ok, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error,bad_call}, State}.

handle_cast(_Msg, State) ->
    io:format("bad cast ~p\n", [_Msg]),
    {noreply, State}.

handle_info({epx_event,_Win,{key_press,Sym,_Mod,_Code}},State) ->
    io:format("pacman_maze: key_press = ~w\n", [Sym]),
    ets:foldl(fun(E,_Acc) ->
		      if E#entity.type =:= player ->
			      E#entity.pid ! {key_down,Sym};
			 true ->
			      ok
		      end
	      end, [], State#state.entities),
    {noreply,State};
handle_info({epx_event,_Win,{key_release,Sym,_Mod,_Code}},State) ->
    io:format("pacman_maze: key_release = ~w\n", [Sym]),
    ets:foldl(fun(E,_Acc) ->
		      if E#entity.type =:= player ->
			      E#entity.pid ! {key_up,Sym};
			 true ->
			      ok
		      end
	      end, [], State#state.entities),
    {noreply,State};

handle_info({timeout,_TRef,tick}, State) ->
    State1 = move_entities(State),
    State2 = schedule_redraw(State1),
    erlang:start_timer(?TICK, self(), tick),
    {noreply, State2};
handle_info(redraw, State) ->
    State1 = draw_window(State),
    {noreply, State1#state { redraw = false }};

handle_info({epx_event,_Win, close}, State) ->
    {stop, normal, State};
handle_info({'DOWN',Mon,process,_Pid,_Reason}, State) ->
    io:format("~w died reason=~p\n", [_Pid,_Reason]),
    case kill_entity(Mon, State) of
	false ->
	    {noreply, State};
	State1 ->
	    {noreply, State1}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    epx:window_detach(State#state.window),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% send redraw message if needed
schedule_redraw(State) ->
    if State#state.redraw ->
	    State;
       true ->
	    self() ! redraw,
	    State#state { redraw = true }
    end.

%% Maze definitions
%% 'X' is a solid wall
%% 'O' is a hallow wal
%% ' ' is a corridor and food
%% '@' is big food
%% '&' is a user spawn point
%% '#' is a ghost spawn point

%% Ascii version of maze
maze_level_1() ->
    [
     "XXXXXXXXXXXXXXXXX",
     "X    XX   XX    X",
     "X@XX    X    XX@X",
     "X    XX   XX    X",
     "X XX  X X X  XX X",
     "X XXX X X X XXX X",
     "X       X       X",
     "XX X X XXX X X XX",
     "XX X X O#  X X XX",
     "XX X X XXX X X XX",
     "X       &       X",
     "X XXXX X X XXXX X",
     "X XXXX     XXXX X",
     "X    X XXX X    X",
     "X@XX    X    XX@X",
     "X    XX   XX    X",
     "XXXXXXXXXXXXXXXXX"
    ].

maze_level_2() ->
    [
     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
     "X    XX   XX    X    XX   XX    X",
     "X@XX    X    XX@X@XX    X    XX@X",
     "X    XX & XX    X    XX & XX    X",
     "X XX  X X X  XX X XX  X X X  XX X",
     "X XXX X X X XXX X XXX X X X XXX X",
     "X       X       X       X       X",
     "XX X X XXX X X XXX X X XXX X X XX",
     "XX X X O#  X X  #  X X O#  X X XX",
     "XX X X XXX X X XXX X X XXX X X XX",
     "X       &       X       &       X",
     "X XXXX X X XXXX X XXXX X X XXXX X",
     "X XXXX     XXXX X XXXX     XXXX X",
     "X    X XXX X    X    X XXX X    X",
     "X@XX    X    XX@X@XX    X    XX@X",
     "X    XX   XX    X    XX   XX    X",
     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ].

maze_level_4() ->
    [
     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
     "X    XX   XX    X    XX   XX    X    XX   XX    X    XX   XX    X",
     "X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X",
     "X    XX & XX    X    XX & XX    X    XX & XX    X    XX & XX    X",
     "X XX  X X X  XX X XX  X X X  XX X XX  X X X  XX X XX  X X X  XX X",
     "X XXX X X X XXX X XXX X X X XXX X XXX X X X XXX X XXX X X X XXX X",
     "X       X       X       X       X       X       X       X       X",
     "XX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XX",
     "XX X X O#  X X  #  X X O#  X X     X X O#  X X  #  X X O#  X X XX",
     "XX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XX",
     "X       &       X       &       X       &       X       &       X",
     "X XXXX X X XXXX X XXXX X X XXXX X XXXX X X XXXX X XXXX X X XXXX X",
     "X XXXX     XXXX X XXXX     XXXX X XXXX     XXXX X XXXX     XXXX X",
     "X    X XXX X    X    X XXX X    X    X XXX X    X    X XXX X    X",
     "X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X",
     "X    XX   XX    X    XX   XX    X    XX   XX    X    XX   XX    X",
     "X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X",
     "X    XX & XX    X    XX & XX    X    XX & XX    X    XX & XX    X",
     "X XX  X X X  XX X XX  X X X  XX X XX  X X X  XX X XX  X X X  XX X",
     "X XXX X X X XXX X XXX X X X XXX X XXX X X X XXX X XXX X X X XXX X",
     "X       X       X       X       X       X       X       X       X",
     "XX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XX",
     "XX X X O#  X X  #  X X O#  X X       X O#  X X  #  X X O#  X X XX",
     "XX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XXX X X XX",
     "X       &       X       &       X       &       X       &       X",
     "X XXXX X X XXXX X XXXX X X XXXX X XXXX X X XXXX X XXXX X X XXXX X",
     "X XXXX     XXXX X XXXX     XXXX   XXXX     XXXX X XXXX     XXXX X",
     "X    X XXX X    X    X XXX X    X    X XXX X    X    X XXX X    X",
     "X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X@XX    X    XX@X",
     "X    XX   XX    X    XX   XX    X    XX   XX    X    XX   XX    X",
     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ].

draw_maze(Maze, Pixels, Profile) ->
    Width = epx:pixmap_info(Maze, width),
    Height = epx:pixmap_info(Maze, height),
    draw_maze_rows(0, Height, Width, Maze, Pixels, Profile).

draw_maze_rows(Y, Height, Width, Maze, Pixels, Profile) when Y < Height ->
    draw_maze_row(Y, 0, Width, Maze, Pixels, Profile),
    draw_maze_rows(Y+1, Height, Width, Maze, Pixels, Profile);
draw_maze_rows(Height, Height, _Width, _Maze, _Pixels, _Profile) ->
    ok.

draw_maze_row(Y, X, Width, Maze, Pixels, Profile) when X < Width ->
    {Code,_,_,_} = epx:pixmap_get_pixel(Maze,X,Y),
    draw_maze_position(X*?BlockSize, Y*?BlockSize, Code, Pixels, Profile),
    draw_maze_row(Y, X+1, Width, Maze, Pixels, Profile);
draw_maze_row(_Y, Width, Width, _Maze, _Pixels, _Profile) ->
    ok.

draw_maze_position(X, Y, Code, Pixels, Profile) ->
    epx_gc:set_foreground_color(Profile#profile.maze_color),
    W = ?BlockSize-1,
    if ?IS_WALL_LEFT(Code) ->
	    epx:draw_line(Pixels,X,Y,X,Y+W);
       true -> ok
    end,
    if ?IS_WALL_ABOVE(Code) ->
	    epx:draw_line(Pixels,X,Y,X+W,Y);
       true -> ok
    end,
    if ?IS_WALL_RIGHT(Code) ->
	    epx:draw_line(Pixels,X+W,Y,X+W,Y+W);
       true -> ok
    end,
    if ?IS_WALL_BELOW(Code) ->
	    epx:draw_line(Pixels,X,Y+W,X+W,Y+W);
       true -> ok
    end,
    if ?IS_FOOD_SMALL(Code) ->
	    epx_gc:set_fill_color(Profile#profile.dot_color),
	    epx:draw_rectangle(Pixels,X+11,Y+11,2,2);
       true -> ok
    end,
    if ?IS_FOOD_BIG(Code) ->
	    BigDotColor = Profile#profile.big_dot_color,
	    Color = {255,224,224-BigDotColor,BigDotColor},
	    epx_gc:set_fill_color(Color),
	    epx:draw_rectangle(Pixels,X+8,Y+8,8,8);
       true -> ok
    end,
    if ?IS_SOLID(Code) ->
	    epx_gc:set_fill_color(Profile#profile.solid_color),
	    epx:draw_rectangle(Pixels,X,Y,W+1,W+1);
       true ->
	    ok
    end.

%% maze to pixmap (debug)
maze_to_pixmap(Maze) ->    
    {N,M,Data,_Ps,_Gs} = ascii_to_data(Maze),
    Pixmap = epx:pixmap_create(M,N,a8),
    ok = epx:pixmap_put_pixels(Pixmap,0,0,M,N,a8,Data),
    Pixmap.

%% Convert ascii maze into bit flag version 
ascii_to_data(Maze) ->
    N = length(Maze),          %% number of rows
    M = length(hd(Maze)),      %% number of columns
    {Ms,Ps,Gs} = ascii_to_data_(Maze),
    {N-2,M-2,Ms,Ps,Gs}.

-define(ITE(Cond,Then,Else), if (Cond) -> (Then); true -> (Else) end).

ascii_to_data_(Rs=[Xs,Ys,Zs|_]) ->
    ascii_to_data(Xs,Ys,Zs,Rs,[],0,0,[],[]).

ascii_to_data([_,_],[_,_],[_,_],[_|Rs=[Xs,Ys,Zs|_]],Acc,_X,Y,Ps,Gs) ->
    ascii_to_data(Xs,Ys,Zs,Rs,Acc,0,Y+1,Ps,Gs);
ascii_to_data([_,_],[_,_],[_,_],[_,_,_],Acc,_X,_Y,Ps,Gs) ->
    {list_to_binary(lists:reverse(Acc)),Ps,Gs};
ascii_to_data([_X1|Xs=[X2,_X3|_]],
	      [Y1|Ys=[Y2,Y3|_]],
	      [_Z1|Zs=[Z2,_Z3|_]],Rs,Acc,X,Y,Ps,Gs) ->
    Walls =
	?ITE(X2 =:= $X orelse X2 =:= $O,?WALL_ABOVE,0) bor
	?ITE(Y1 =:= $X orelse Y1 =:= $O,?WALL_LEFT,0)  bor
	?ITE(Y3 =:= $X orelse Y3 =:= $O,?WALL_RIGHT,0) bor
	?ITE(Z2 =:= $X orelse Z2 =:= $O,?WALL_BELOW,0),
    Code = case Y2 of
	       $O -> 0;
	       $X -> ?SOLID;
	       $@ -> Walls bor ?FOOD_BIG;
	       $\s -> Walls bor ?FOOD_SMALL;
	       $&  -> Walls bor ?FOOD_SMALL;  %% spawn point player
	       $#  -> Walls   %% spawn point ghost
	   end,
    Xp = X*?BlockSize,
    Yp = Y*?BlockSize,
    Ps1 = if Y2 =:= $& -> [{Xp,Yp}|Ps]; true -> Ps end,
    Gs1 = if Y2 =:= $# -> [{Xp,Yp}|Gs]; true -> Gs end,
    ascii_to_data(Xs,Ys,Zs,Rs,[Code|Acc],X+1,Y,Ps1,Gs1).

setup_window(Width,Height) ->
    case lists:keyfind(fb, 1, init:get_arguments()) of
	{fb,_} ->
	    Format = r5g6b5,
	    epx_backend:start_link([{backend,"fb"},{pixel_format,Format}]),
	    Backend = epx_backend:default(),
	    BW = epx_backend:info(Backend,width),
	    BH = epx_backend:info(Backend,height),
	    Window = epx:window_create(0,0,BW,BH,[key_press,key_release]),
	    Pixels = epx:pixmap_create(BW,BH,Format),
	    {Window,Pixels,Backend};
	_ ->
	    Window = epx:window_create(50, 50,Width,Height,
				       [key_press,key_release]),
	    Pixels = epx:pixmap_create(Width, Height),
	    Backend = epx_backend:default(),
	    {Window,Pixels,Backend}
    end.

draw_entities(Tab,State) ->
    ets:foldl(fun(E,_Acc) -> draw_entity(E,State) end, [], Tab).

draw_entity(E,State) ->
    Images = State#state.images,
    Image = case E#entity.type of
		ghost ->
		    Images#images.ghost1;
		player ->
		    Images#images.pacman
	    end,
    draw_image(Image,E#entity.x+1,E#entity.y+1,State).

draw_image(Pixmap, X, Y, State) ->
    W = epx:pixmap_info(Pixmap, width),
    H = epx:pixmap_info(Pixmap, height),
    epx:pixmap_copy_area(Pixmap, State#state.pixels,
			 0, 0, X, Y, W, H, [blend]).

load_image(FileName) ->
    PathName = filename:join(code:priv_dir(pacman), FileName),
    {ok, #epx_image { pixmaps = [Image]}} = epx_image:load(PathName),
    Image.

load_directions(FileName) ->
    Right  = load_image(FileName),
    W  = epx:pixmap_info(Right, width),
    H = epx:pixmap_info(Right, height),
    Format = epx:pixmap_info(Right, pixel_format),
    Cx = (W div 2),
    Cy = (H div 2),
    E = 0.001,  %% works, but check me!!!
    Up = epx:pixmap_create(W, H, Format),
    epx:pixmap_rotate_area(Right, Up, math:pi()/2+E,0,0,Cx,Cy,Cx,Cy,W,H,[]),

    Down = epx:pixmap_create(W, H, Format),
    epx:pixmap_rotate_area(Right, Down, -math:pi()/2,0,0,Cx,Cy,Cx,Cy,W,H,[]),

    Left = epx:pixmap_create(W, H, Format),
    epx:pixmap_rotate_area(Right, Left, math:pi()+E,0,0,Cx,Cy,Cx,Cy,W,H,[]),
    {Right,Left,Up,Down}.

load_images() ->
    {R2,L2,U2,D2} = load_directions("PacMan2.png"),
    {R3,L3,U3,D3} = load_directions("PacMan3.png"),
    {R4,L4,U4,D4} = load_directions("PacMan4.png"),
    P = load_image("PacMan1.png"),
    #images {
       ghost1=load_image("Ghost1.png"),
       ghost2=load_image("Ghost2.png"),
       ghostscared1=load_image("GhostScared1.png"),
       ghostscared2=load_image("GhostScared2.png"),
       pacman = P,
       pacman_left  = {P,L2,L3,L4},
       pacman_right = {P,R2,R3,R4},
       pacman_up    = {P,U2,U3,U4},
       pacman_down  = {P,D2,D3,D4}
      }.
    
draw_window(State) ->
    Profile = State#state.profile,
    epx_gc:set_fill_color(Profile#profile.background_color),
    epx_gc:set_border_width(0),
    epx:draw_rectangle(State#state.pixels, 0, 0,
		       State#state.width, State#state.height),
    draw_maze(State#state.maze, State#state.pixels, State#state.profile),
    draw_entities(State#state.entities, State),
    epx:pixmap_draw(State#state.pixels,
		    State#state.window,
		    0, 0, 0, 0, State#state.width, State#state.height),
    State.

move_entities(State) ->
    Tab = State#state.entities,
    ets:foldl(fun(E,_Acc) ->
		      unmap_entity(E,State#state.entity_map),
		      E1 = move_entity(E,State),
		      IDs = map_entity(E1,State#state.entity_map),
		      %% ids is a list of all entity in the same position
		      Collide = 
			  lists:foldl(
			    fun(Id,Acc) ->
				    case load_entity(Id,Tab) of
					false -> Acc;
					F when F#entity.type =/= 
					       E#entity.type ->
					    [{F#entity.type,Id}|Acc];
					_ ->
					    Acc
				    end
			    end, [], IDs),
		      Pos = {E#entity.x,E#entity.y},
		      Dir = {E#entity.dx,E#entity.dy},
		      if Collide =/= [] ->
			      E#entity.pid ! {collision,self(),E#entity.id,
					      Pos,Dir,Collide};
			 true ->
			      ok
		      end,
		      save_entity(E1, Tab)
	      end, [], Tab),
    State.

move_entity(E,State) ->
    if E#entity.x rem ?BlockSize =:= 0,
       E#entity.y rem ?BlockSize =:= 0 ->
	    Xm = E#entity.x div ?BlockSize,
	    Ym = E#entity.y div ?BlockSize,
	    case delta_entity(E,Xm,Ym,State) of
		{Dx,Dy,{_,[]}} ->
		    step_entity(E, Dx, Dy);
		{Dx,Dy,{Type,Ds}} ->
		    Pos = {E#entity.x,E#entity.y},
		    Dir = {E#entity.dx,E#entity.dy},
		    E#entity.pid ! {Type,self(),E#entity.id,Pos,Dir,Ds},
		    step_entity(E, Dx, Dy)
	    end;
       true ->
	    step_entity(E, E#entity.dx, E#entity.dy)
    end.

%% at block position
delta_entity(E,Xm,Ym,State) ->
    Dx = E#entity.dx,
    Dy = E#entity.dy,
    {Code,_,_,_} = epx:pixmap_get_pixel(State#state.maze,Xm,Ym),
    case alternative_routes(Dx,Dy,Code) of
	{junction,[{Dx1,Dy1}]} ->
	    {Dx1,Dy1,{junction,[]}};
	{Type,Ds} ->
	    {0,0,{Type,Ds}}
    end.

step_entity(E, Dx, Dy) ->
    Speed = E#entity.speed,
    X = E#entity.x + Dx*Speed,
    Y = E#entity.y + Dy*Speed,
    E#entity { x=X, y=Y, dx=Dx, dy=Dy }.

entity_code(E,State) ->
    entity_code(E, E#entity.x div ?BlockSize, E#entity.y div ?BlockSize, State).

entity_code(_E, Xm, Ym, State) ->
    {Code,_,_,_} = epx:pixmap_get_pixel(State#state.maze,Xm,Ym),
    Code.

%% From the given Code position what are the alternative routes
alternative_routes(Dx,Dy,Code) ->
    if   
	Dx < 0 ->
	    if ?IS_WALL_LEFT(Code) ->
		    {wall,
		     ?ITE(?NO_WALL_RIGHT(Code),[{1,0}],[]) ++
			 ?ITE(?NO_WALL_ABOVE(Code),[{0,-1}],[]) ++
			 ?ITE(?NO_WALL_BELOW(Code),[{0,1}],[])};
	       true ->
		    {junction,?ITE(?NO_WALL_ABOVE(Code),[{0,-1}],[]) ++
			 ?ITE(?NO_WALL_BELOW(Code),[{0,1}],[]) ++
			 ?ITE(?NO_WALL_LEFT(Code),[{-1,0}],[])}
	    end;
       Dx > 0 ->
	    if ?IS_WALL_RIGHT(Code) ->
		    {wall,
		     no_wall_list([?WALL_LEFT,{-1,0},
				   ?WALL_ABOVE,{0,-1},
				   ?WALL_BELOW,{0,1}], Code)};
	       true ->
		    {junction,
		     no_wall_list([?WALL_RIGHT,{1,0},
				   ?WALL_ABOVE,{0,-1},
				   ?WALL_BELOW,{0,1}],Code)}
	    end;
       Dy < 0 ->
	    if ?IS_WALL_ABOVE(Code) ->
		    {wall,
		     no_wall_list([?WALL_BELOW,{0,1},
				   ?WALL_LEFT,{-1,0},
				   ?WALL_RIGHT,{1,0}], Code)};
	       true ->
		    {junction,
		     no_wall_list([?WALL_LEFT,{-1,0},
				   ?WALL_RIGHT,{1,0},
				   ?WALL_ABOVE,{0,-1}],Code)}
	    end;
       Dy > 0 ->
	    if ?IS_WALL_BELOW(Code) ->
		    {wall,
		     no_wall_list([?WALL_ABOVE,{0,-1},
				   ?WALL_LEFT,{-1,0},
				   ?WALL_RIGHT,{1,0}],Code)};
	       true ->
		    {junction,
		     no_wall_list([?WALL_LEFT,{-1,0},
				   ?WALL_RIGHT,{1,0},
				   ?WALL_BELOW,{0,1}],Code)}
	    end;
       true ->
	    {junction,[]}
    end.

no_wall_list([F,Dir|Fs], Code) ->
    if Code band F =:= 0 ->
	    [Dir | no_wall_list(Fs, Code)];
       true ->
	    no_wall_list(Fs, Code)
    end;
no_wall_list([], _Code) ->
    [].

wall_list([F,Dir|Fs], Code) ->
    if Code band F =:= F ->
	    [Dir | wall_list(Fs, Code)];
       true ->
	    wall_list(Fs, Code)
    end;
wall_list([], _Code) ->
    [].



%% 
%% Routes given Code
%%
routes(Code) ->
    ?ITE(?NO_WALL_ABOVE(Code),[{0,-1}],[]) ++
	?ITE(?NO_WALL_BELOW(Code),[{0,1}],[]) ++
	?ITE(?NO_WALL_LEFT(Code),[{-1,0}],[]) ++
	?ITE(?NO_WALL_RIGHT(Code),[{1,0}],[]).

code_delta(Dx,Dy,Code) ->
    if Dx < 0, ?NO_WALL_LEFT(Code) ->
	    {-1,0};
       Dx > 0, ?NO_WALL_RIGHT(Code) ->
	    {1,0};
       Dy < 0, ?NO_WALL_BELOW(Code) ->
	    {0,-1};
       Dy > 0, ?NO_WALL_ABOVE(Code) ->
	    {0,1};
       true ->
	    {0,0}
    end.

select_start_position(Type,State) ->
    Positions =
	case Type of
	    player -> State#state.player_start;
	    ghost  -> State#state.ghost_start
	end,
    N = length(Positions),
    lists:nth(rand:uniform(N), Positions).
    
kill_entity(Mon, State) ->
    ToDelete =
	ets:foldl(fun(E,Acc) ->
			  unmap_entity(E, State#state.entity_map),
			  if E#entity.mon =:= Mon -> [E#entity.id|Acc];
			     true -> Acc
			  end
		  end, [], State#state.entities),
    if ToDelete =:= [] ->
	    false;
       true ->
	    lists:foreach(fun(Id) ->
				  delete_entity(Id, State#state.entities)
			  end, ToDelete),
	    State
    end.

%%
%% Create a new unique entity id
%%
make_entity_id(Tab) ->
    Id = rand:uniform(16#1000000)-1,
    case ets:member(Tab, Id) of
	true -> make_entity_id(Tab);
	false -> Id
    end.

delete_entity(Id, Table) ->
    ets:delete(Table, Id).

load_entity(Id, Table) ->
    case ets:lookup(Table, Id) of
	[] -> false;
	[E] -> E
    end.
    
save_entity(E, Table) when is_record(E, entity) ->
    ets:insert(Table, E).

%%
%% Manage map
%%
unmap_entity(E,Table) ->
    unmap_entity(E#entity.x div ?BlockSize, E#entity.y div ?BlockSize, 
		 E#entity.id, Table).
unmap_entity(Xm,Ym,Id,Table) ->
    Pos = {Xm,Ym},
    case ets:lookup(Table, Pos) of
	[] -> true;
	[{_,IdList}] ->
	    ets:insert(Table, {Pos,IdList--[Id]})
    end.

map_entity(E,Table) ->
    map_entity(E#entity.x div ?BlockSize, E#entity.y div ?BlockSize, 
	       E#entity.id, Table).
map_entity(Xm,Ym,Id,Table) ->
    Pos = {Xm,Ym},
    IDs = case ets:lookup(Table, Pos) of
	      [] -> [];
	      [{_,IDs0}] -> IDs0
	  end,
    ets:insert(Table, {Pos,[Id|IDs]}),
    IDs.
