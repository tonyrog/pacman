%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Draw the pacman characters
%%% @end
%%% Created : 14 Aug 2016 by Tony Rogvall <tony@rogvall.se>

-module(pacman_create).

-compile(export_all).

-define(BLINKY, red).
-define(CLYDE,  orange).
-define(PINKY,  pink).
-define(INKY,   cyan).

start() ->
    start(100).

start(W) ->
    H0 = W div 4,
    H = W+H0,
    epx:start(),
    {ok,Game} = epx_sprite:start_link([]),
    Ghosts = 
	lists:map(
	  fun({Who,I}) ->
		  Y = 10,
		  X = 20+I*W+I*10,
		  {ok,Ref} = epx_sprite:create(Game, W, H, 
					       [{x,X},{y,Y},
						{vx,rndv(10,100)},
						{vy,rndv(10,100)}]),
		  Pixels = make_ghost(Who,W,H0,0,0),
		  epx_sprite:set_image(Game,Ref,Pixels),
		  {Ref,Who,Pixels}
	  end, [{?CLYDE,0},{?BLINKY,1},{?PINKY,2},{?INKY,3}]),
    Pacmans = 
	lists:map(
	  fun({Color,I}) ->
		  Y = 50,
		  X = 20+I*W+I*10,
		  {ok,Ref} = epx_sprite:create(Game, W, H, 
					       [{x,X},{y,Y},
						{vx,rndv(10,100)},
						{vy,rndv(10,100)}]),
		  Pixels = make_pacman(Color,W,0,0),
		  epx_sprite:set_image(Game,Ref,Pixels),
		  {Ref,Color,Pixels}
	  end, [{yellow,0},{blue,1}]),
    loop(Game, W, H0, Ghosts, Pacmans).


%% generate a random number in -AbsMax..-AbsMin AbsMin..AbsMax
rndv(AbsMin,AbsMax) when is_integer(AbsMin), is_integer(AbsMax),
			 AbsMin > 0, AbsMax > AbsMin ->
    ((rand:uniform(AbsMax-AbsMin+1)-1)+AbsMin)*((rand:uniform(2)*2) - 3).


loop(Game, W, H0, Ghosts, Pacmans) ->
    lists:foreach(
      fun({Ref,Color,Pixels}) ->
	      {Vx,Vy} = epx_sprite:get_velocity(Game, Ref),
	      Dx = if Vx < 0 -> -3;
		      Vx > 0 -> 3;
		      true -> 0
		   end,
	      Dy = if Vy < 0 -> -3;
		      Vy > 0 -> 3;
		      true -> 0
		   end,
	      draw_ghost(Pixels,Color,W,H0,Dx,Dy),
	      epx_sprite:set_image(Game,Ref,Pixels)
      end, Ghosts),

    lists:foreach(
      fun({Ref,Color,Pixels}) ->
	      {Vx,Vy} = epx_sprite:get_velocity(Game, Ref),
	      Dir0 =
		  if Vx == 0, Vy == 0 -> 0;
		     true -> trunc((180/math:pi())*math:atan2(Vy, Vx))
		  end,
	      Dir1 = ((Dir0+360) rem 360),
	      T = erlang:system_time(milli_seconds),
	      Yam = abs( ((T rem 100) - 50 ) ) ,
	      draw_pacman(Pixels,Color,W,Dir1,Yam),
	      epx_sprite:set_image(Game,Ref,Pixels)
      end, Pacmans),
    timer:sleep(100),
    loop(Game, W, H0, Ghosts, Pacmans).


make_ghost(Color,W,H0,Dx,Dy) ->
    H = W+H0,
    Pixels = epx:pixmap_create(W, H),
    draw_ghost(Pixels,Color,W,H0,Dx,Dy).

draw_ghost(Pixels,Color,W,H0,Dx,Dy) ->
    %% W = epx:pixmap_info(Pixels, width),
    %% H = epx:pixmap_info(Pixels, height),
    X0 = 1,
    Y0 = 1,
    W3 = W div 3,
    W2 = W div 2,
    W4 = W div 4,
    W8 = W div 8,
    W16 = W div 16,
    W32 = W3 div 2,
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(Color),
    %% head
    epx:draw_ellipse(Pixels, X0, Y0, W-2, W-2),
    Y1 = W2-1,
    H2 = W2, %% H-W-20,
    %% body
    epx:draw_rectangle(Pixels, 1, Y1, W-1, H2),
    Y3 = Y1+H2,
    Y4 = Y3+H0,
    X1 = W32,
    %% dress
    epx:draw_triangle(Pixels, {X0, Y3}, {X0, Y4}, {X1, Y3}),
    X2 = X1+W32,
    X3 = X2+W32,
    epx:draw_triangle(Pixels, {X1, Y3}, {X2, Y4}, {X3, Y3}),
    X4 = X3+W32,
    X5 = X4+W32,
    epx:draw_triangle(Pixels, {X3, Y3}, {X4, Y4}, {X5, Y3}),
    X6 = W-1,
    epx:draw_triangle(Pixels, {X5, Y3}, {X6, Y4}, {X6, X3}),
    %% eyes
    epx_gc:set_fill_color(white),
    Eoffs = 4,
    Ew  = W4+W16,
    Eh =  W4+W16+2,
    Ex0 = X0+W2-Ew-Eoffs,
    Ex1 = X0+W2+Eoffs,
    Ey  = Y0+W2-(Eh div 2),
    epx:draw_ellipse(Pixels, Ex0, Ey, Ew, Eh),
    epx:draw_ellipse(Pixels, Ex1, Ey, Ew, Eh),
    epx_gc:set_fill_color(black),
    Pw  = W8,
    Ph  = W8+1,
    Px0 = Ex0+((Ew div 2) - (Pw div 2))+Dx,
    Px1 = Ex1+((Ew div 2) - (Pw div 2))+Dx,
    Py  = Ey+((Eh div 2) - (Ph div 2))+Dy,
    epx:draw_ellipse(Pixels, Px0, Py, Pw, Ph),
    epx:draw_ellipse(Pixels, Px1, Py, Pw, Ph),
    Pixels.

make_pacman(Color,W,Dir,Yam) ->
    H = W,
    Pixels = epx:pixmap_create(W, H),
    draw_pacman(Pixels,Color,W,Dir,Yam).

%% Dir = 0 = left, 90 = up, 180 = right, 270 = down
%% Yam = mouth angle (/2)
draw_pacman(Pixels,Color,W,Dir,Yam) ->
    X0 = 2,
    Y0 = 2,
    W2 = W div 2,
    %% _W4 = W div 4,
    W8 = W div 8,
    %% _W16 = W div 16,
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(Color),
    epx_gc:set_border_color(black),
    epx_gc:set_border_width(if W >= 32 -> 2; true -> 1 end),

    %% draw body
    epx:draw_ellipse(Pixels, X0, Y0, W-4, W-4),
    epx_gc:set_border_width(0),
    %% draw mouth
    Y1 = Y0+W2,    %% center
    X1 = X0+W2,    %% center

    DegRad = math:pi()/180,

    Y2 = trunc(Y1+W2*math:sin((Dir+Yam)*DegRad)),
    Y3 = trunc(Y1+W2*math:sin((Dir-Yam)*DegRad)),
    X2 = trunc(X1+W*math:cos((Dir)*DegRad)),

    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color({0,0,0,0}),
    epx:draw_triangle(Pixels, {X1,Y1}, {X2,Y2}, {X2, Y3}),

    %% draw eye
    Pw  = W8,
    Ph  = W8+1,

    %% Px = trunc(X1+W8*math:cos(-((Dir+Yam)+30)*DegRad)),
    %% Py = trunc(Y1+W8*math:sin(-((Dir+Yam)+30)*DegRad)),
    Px = trunc(X1+W8*math:cos((Dir+45)*DegRad)),
    Py = trunc(Y1+W8*math:sin((Dir+45)*DegRad)),

    epx_gc:set_fill_color(black),
    epx:draw_ellipse(Pixels, Px, Py, Pw, Ph),
    
    Pixels.
