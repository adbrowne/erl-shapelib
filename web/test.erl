generate path:

f().
cd("c:/code/erl-shapelib").
c("shapelib.erl").
Aus = shapelib:get_shapes("australia/australia.shp").
{_,_,[S0,S1|_]} = Aus.
{_, {Xmin, Ymin, Xmax, Ymax, Parts, Points}} = S1.

[Part1, Part2, Part3] = Points.

XScale = 300/(Xmax - Xmin).                                                  
YScale = 300/(Ymax - Ymin).                                                  
Scale = YScale.                                                              
ScaledPoints = [{(X-Xmin)*YScale, (Y - Ymin) * YScale} || {X,Y} <- Part1].  
Output = [lists:map(fun(I) -> {X,Y} = I, [io_lib:format("L~p,~p",[X,Y])] end, ScaledPoints)].
{ok, S} = file:open("test1.data", write). 
io:format(S, "~s~n", [Output]).
file:close(S). 