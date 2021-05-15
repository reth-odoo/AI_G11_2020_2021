/*
The program just needs to be able to make a decision based on the state of the game
*/

/*
parse_pawn_move(L):-split_string(L," "," ",["move",SN1,SN2]),number_string(N1,SN1),number_string(N2,SN2).
parse_place_wall(L):-split_string(L," "," ",["wall",SN1,SN2,"v"]),number_string(N1,SN1),number_string(N2,SN2).
parse_place_wall(L):-split_string(L," "," ",["wall",SN1,SN2,"h"]),number_string(N1,SN1),number_string(N2,SN2).
parse_move(L):-parse_pawn_move(L);parse_place_wall(L).
*/

/*UTILITY*/

/*biggest on 3, smallest on 4*/
biggest_of_2(X,Y,X,Y):- X>Y.
biggest_of_2(X,Y,Y,X):- Y>X.

/*insert in a list, 1 is the first element*/

insertBefore(1,[],X,[X]).
insertBefore(1,[L|OL],X,[X,L|OL]).
insertBefore(N,[L|OL],X,[L|NL]):- N>1, NN is N-1, insertBefore(NN,OL,X,NL).

insertAfter([],X,[X]).
insertAfter([L|OL],X,[L|FL]):- insertAfter(OL,X,FL).


manhattanDistance(X1,Y1,X2,Y2,D):- DX is X2-X1, DY is Y2-Y1, D is abs(DX)+abs(DY).

unzip_3([[A,B,C]],[A],[B],[C]).
unzip_3([[A,B,C]|L],[A|X],[B|Y],[C|Z]):- unzip_3(L,X,Y,Z).

unzip_4([[A,B,C,D]],[A],[B],[C],[D]).
unzip_4([[A,B,C,D]|L],[A|W],[B|X],[C|Y],[D|Z]):- unzip_4(L,W,X,Y,Z).


/*DATA Representation
NWaals = [NWaals1, NWaals2, NWalls3, NWaals4]

Positions = [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]]

Walls = [[X1,Y1,'v'],[X2,Y2,'h']....] (top-left corner)

*/
player_separator(PLAYER_NUMBER, Positions, NWalls, PlayerNWalls, OtherNWalls, PlayerPos, OtherPos):- 
 nth1(PLAYER_NUMBER, Positions, PlayerPos, OtherPos), 
 nth1(PLAYER_NUMBER, NWalls, PlayerNWalls, OtherNWalls).






/*Valid States (Placements)*/

/*Pawn placement*/



/*check that the pawn is at a valid coordinate*/
board_coordinate(X):- member(X,[0,1,2,3,4,5,6,7,8]).
on_board(X,Y):- board_coordinate(X),board_coordinate(Y).


/*check that no two pawns are on the same space
for i in {1,2,3,4}{
    pick the i-th [X,Y] and for all remaining [X1,Y1]{
        check either X!=X1 OR Y!=Y1
    }
}
*/
not_twice_pawn(Positions):- 
 forall(member(PLAYER_NUMBER,[1,2,3,4]),
     (
         forall(
         (nth1(PLAYER_NUMBER, Positions, [X,Y], OtherPos),member([X1,Y1], OtherPos)), 
         (X\=X1; Y\=Y1))
        )
        ).

pawn_on(X,Y,Positions):-member([X,Y],Positions).

/*call all state validity check*/
valid_position(Positions):- 
 forall(member([X,Y], Positions),(on_board(X,Y))), 
 not_twice_pawn(Positions).





/*Walls placement*/

valid_NWall(N):-member(N,[0,1,2,3,4,5]).
valid_NWalls([N1,N2,N3,N4]):- valid_NWall(N1),valid_NWall(N2),valid_NWall(N3),valid_NWall(N4).


intersecting_walls(X1,Y1,'h',X2,Y2,'h'):- Y1=Y2, (X1=X2 ; X1 is X2-1 ; X1 is X2 + 1).
intersecting_walls(X1,Y1,'v',X2,Y2,'v'):- X1=X2, (Y1=Y2 ; Y1 is Y2-1 ; Y1 is Y2 + 1).
intersecting_walls(X1,Y1,'v',X2,Y2,'h'):- X1=X2, Y1=Y2.
intersecting_walls(X1,Y1,'h',X2,Y2,'v'):- intersecting_walls(X2,Y2,'v',X1,Y1,'h').

valid_wall_placement(X1,Y1,'v'):- on_board(X1,Y1), X1<6, Y1<7, X1>=0, Y1>=0.
valid_wall_placement(X1,Y1,'h'):- on_board(X1,Y1), X1<7, Y1<6, X1>=0, Y1>=0.


%at_least_one_path(P,Goal):-.

%path_find(X1,Y1,X2,Y2):-X1=X2,Y1=Y2.
%path_find(X1,Y1,X2,Y2):-move_one(X1,Y1,X3,Y3),path_find(X3,Y3,X2,Y2).

/*for all Walls, the one abov*/
wall_ok(X1,Y1,'v', Walls):- valid_wall_placement(X1,Y1,'v'),
                            forall(member([X2,Y2,Orientation],Walls),
                                (not(intersecting_walls(X1,Y1,'v',X2,Y2,Orientation)))
                            ).
wall_ok(X1,Y1,'h', Walls):- valid_wall_placement(X1,Y1,'h'), 
                            forall(member([X2,Y2,Orientation],Walls),
                                (not(intersecting_walls(X1,Y1,'h',X2,Y2,Orientation)))
                            ).

walls_ok(Walls):- forall(
    member([X,Y,D],Walls),
    forall(
        (member([X1,Y1,D1],Walls),(X1\=X;Y1\=Y;D\=D1)),
        (not(intersecting_walls(X,Y,D,X1,Y1,D1),valid_wall_placement(X1,Y1,D1),valid_wall_placement(X,Y,D)))
        )
    ).



/*for all walls starting on this Y or the level above, check that X not between the coordinates*/
no_walls_x(XTarg,XOrig,Y,Walls):-forall((Y2 is Y-1, (member([X,Y,'v'], Walls) ; member([X,Y2,'v'],Walls)))
                            , (biggest_of_2(XTarg,XOrig,BigX,SmallX), (BigX =< X ; SmallX > X))).

no_walls_y(YTarg,YOrig,X,Walls):-forall((X2 is X+1, (member([X,Y,'h'], Walls) ; member([X2,Y,'h'],Walls)))
                            , (biggest_of_2(YTarg,YOrig,BigY,SmallY), (BigY =< Y ; SmallY > Y))).

no_walls_v(X, Y, Walls):- 
    Y2 is Y-1,
    \+ member([X,Y,'v'], Walls),
    \+ member([X,Y2,'v'],Walls).

no_walls_h(X, Y, Walls):- 
    X2 is X+1,
    \+ member([X,Y,'h'], Walls),
    \+ member([X2,Y,'h'],Walls).




/*Valid Transitions (Moves)*/



/*true if you can move one space in any cardinal direction*/
move_one(Positions, XTarg, YTarg, X, Y, Walls):- 
on_board(XTarg,YTarg), not(pawn_on(XTarg,YTarg,Positions)),
(
    (move_left(XTarg,X,Y,Walls),YTarg=Y); 
    (move_right(XTarg,X,Y,Walls),YTarg=Y); 
    (move_up(YTarg,X,Y,Walls),XTarg=X);
    (move_down(YTarg,X,Y,Walls),XTarg=X)
).

move_left(XTarg, X, Y,Walls):- XTarg is X-1, no_walls_x(XTarg, X,Y,Walls).
move_up(YTarg, X, Y,Walls):- YTarg is Y+1, no_walls_y(YTarg, Y,X,Walls).
move_right(XTarg, X, Y,Walls):- XTarg is X+1, no_walls_x(XTarg, X,Y,Walls).
move_down(YTarg, X, Y,Walls):- YTarg is Y-1, no_walls_y(YTarg, Y,X,Walls).


move_diagonal(Positions,XTarg,YTarg,X,Y,Walls):-
diagonal_up_right(Positions,XTarg,YTarg,X,Y,Walls);
diagonal_up_right(Positions,XTarg,YTarg,X,Y,Walls);
diagonal_up_right(Positions,XTarg,YTarg,X,Y,Walls);
diagonal_up_right(Positions,XTarg,YTarg,X,Y,Walls)
.

/*can't move nor jump to right or up, but can move to the space from the other pawn's space*/
diagonal_up_right(Positions,NewX,NewY,X,Y,Walls):- 
X2 is X+2, Y2 is Y+2, X1 is X+1, Y1 is Y+1, NewX = X1, NewY = Y1,
(
    (pawn_on(X1,Y,Positions),not(jump_over_right(Positions, X2, Y, X, Y, Walls)), move_one(Positions, X1, Y1, X1, Y, Walls));
    (pawn_on(X,Y1,Positions),not(jump_over_top(Positions, X, Y2, X, Y, Walls)), move_one(Positions, X1, Y1, X, Y1, Walls))
).
diagonal_up_left(Positions,NewX,NewY,X,Y,Walls):- 
X2 is X-2, Y2 is Y+2, X1 is X-1, Y1 is Y+1, NewX = X1, NewY = Y1,
(
    (pawn_on(X1,Y,Positions),not(jump_over_left(Positions, X2, Y, X, Y, Walls)), move_one(Positions, X1, Y1, X1, Y, Walls));
    (pawn_on(X,Y1,Positions),not(jump_over_top(Positions, X, Y2, X, Y, Walls)), move_one(Positions, X1, Y1, X, Y1, Walls))
).
diagonal_down_right(Positions,NewX,NewY,X,Y,Walls):- 
X2 is X+2, Y2 is Y-2, X1 is X+1, Y1 is Y-1, NewX = X1, NewY = Y1,
(
    (pawn_on(X1,Y,Positions),not(jump_over_right(Positions, X2, Y, X, Y, Walls)), move_one(Positions, X1, Y1, X1, Y, Walls));
    (pawn_on(X,Y1,Positions),not(jump_over_bottom(Positions, X, Y2, X, Y, Walls)), move_one(Positions, X1, Y1, X, Y1, Walls))
).
diagonal_down_left(Positions,NewX,NewY,X,Y,Walls):- 
X2 is X-2, Y2 is Y-2, X1 is X-1, Y1 is Y-1, NewX = X1, NewY = Y1,
(
    (pawn_on(X1,Y,Positions),not(jump_over_left(Positions, X2, Y, X, Y, Walls)), move_one(Positions, X1, Y1, X1, Y, Walls));
    (pawn_on(X,Y1,Positions),not(jump_over_bottom(Positions, X, Y2, X, Y, Walls)), move_one(Positions, X1, Y1, X, Y1, Walls))
).


/*check if the space between start position and target position is a player and there is no wall behind it*/
jump_over(Positions,XTarg,YTarg,X,Y,Walls):- 
jump_over_right(Positions, XTarg,YTarg,X,Y,Walls);
jump_over_left(Positions, XTarg,YTarg,X,Y,Walls);
jump_over_top(Positions, XTarg,YTarg,X,Y,Walls);
jump_over_bottom(Positions, XTarg,YTarg,X,Y,Walls)
.
/*is 2 away in that direction, pawn next to current pawn in that direction, can move one from that space.*/
jump_over_right(Positions, XTarg,YTarg,X,Y,Walls):-XTarg is X+2, X1 is X+1, pawn_on(X1,Y,Positions), move_one(Positions,XTarg,YTarg,X1,Y,Walls).
jump_over_left(Positions, XTarg,YTarg,X,Y,Walls):-XTarg is X-2, X1 is X-1, pawn_on(X1,Y,Positions), move_one(Positions,XTarg,YTarg,X1,Y,Walls).
jump_over_top(Positions, XTarg,YTarg,X,Y,Walls):-YTarg is Y-2, Y1 is Y-1, pawn_on(X,Y1,Positions), move_one(Positions,XTarg,YTarg,X,Y1,Walls).
jump_over_bottom(Positions, XTarg,YTarg,X,Y,Walls):-YTarg is Y+2, Y1 is Y+1, pawn_on(X,Y1,Positions), move_one(Positions,XTarg,YTarg,X,Y1,Walls).

/*
Other Jump implem
jump_over_right(Positions, XTarg,X,Y,Walls):-XTarg is X1+1, X1 is X+1,valid_position(XTarg,Y),no_walls_x(X1,X,Y,Walls),no_walls_x(XTarg,X1,Y,Walls).
jump_over_left(Positions, XTarg,X,Y,Walls):-XTarg is X1-1, X1 is X-1,valid_position(XTarg,Y),no_walls_x(X1,X,Y,Walls),no_walls_x(XTarg,X1,Y,Walls).
jump_over_down(Positions, YTarg,X,Y,Walls):-YTarg is Y1-1, Y1 is Y-1,valid_position(X,YTarg),no_walls_x(Y1,Y,X,Walls),no_walls_x(YTarg,Y1,X,Walls).
jump_over_up(Positions, YTarg,X,Y,Walls):-YTarg is Y1+1, Y1 is Y+1,valid_position(X,YTarg),no_walls_x(Y1,Y,X,Walls),no_walls_x(YTarg,Y1,X,Walls).
*/

move(PLAYER_NUMBER, Positions, XTarg, YTarg, Walls, NewPositions):- 
nth1(PLAYER_NUMBER, Positions, [X,Y], OPos),
(
    move_one(Positions, XTarg, YTarg, X, Y, Walls);
    jump_over(Positions, XTarg,YTarg, X, Y, Walls); 
    move_diagonal(Positions, XTarg,YTarg, X, Y, Walls)
),
insertBefore(PLAYER_NUMBER,OPos,[XTarg,YTarg],NewPositions),
valid_position(NewPositions).

/*wall placement*/

/*Player can place wall such that the Walls list is as below if*/
place_wall(PNB, X1,Y1, Direction, Walls, NWalls, NewWalls, NewNWalls):-
/*a wall can go there*/
wall_ok(X1,Y1,Direction, Walls),  
/*player still has walls and compute new NWalls for them*/
take_wall_from(PNB,NWalls,NewNWalls),
insertAfter(Walls, [X1,Y1,Direction], NewWalls).


/*true if NewNWalls is the updated list of number of walls*/
take_wall_from(PNB,NWalls,NewNWalls):-
nth1(PNB,NWalls,PNWalls,ONWalls), member(PNWalls,[1,2,3,4,5]), NewPNWalls is PNWalls-1, insertBefore(PNB,ONWalls,NewPNWalls,NewNWalls).


/*next player in turn*/

/*enumerated so it can go both ways*/
next_player_number(4,1).
next_player_number(3,4).
next_player_number(2,3).
next_player_number(1,2).
/*next_player_number(NB,NewNB):- NB>=0, NB<4, NewNB is NB+1.*/

/*the game has to not be over*/

/*use format(atom(Result),'~s ~d ~d', ['move', X, Y]). to get a String for Move*/
possible_move(PLAYER_NUMBER, Positions, Walls, NWalls, Move, NewPlayerNumber, NewPositions, NewWalls, NewNWalls):-
forall(member(PNB,[1,2,3,4]), not(player_has_goal(PNB,Positions))),
next_player_number(PLAYER_NUMBER,NewPlayerNumber),
(
    (place_wall(PLAYER_NUMBER, X, Y, D, Walls, NWalls, NewWalls, NewNWalls), NewPositions = Positions, format(atom(Move),'~s ~d ~d ~s', ['wall', X, Y, D]))
    ;
    (move(PLAYER_NUMBER, Positions, X, Y, Walls, NewPositions), NewWalls = Walls, NewNWalls = NWalls, format(atom(Move),'~s ~d ~d', ['move', X, Y]))
).




%choose_evaluation Jeremie
/*evaluation should represent the utility of a certain state for:
-The current player
-All other participants (as one entity -> just sum or more complex?)*/

evaluate(PLAYER_NUMBER, Positions/*,[NW1,NW2,NW3,NW4]*/, Walls, Eval):- /*Some changes based on minimax(OGPNB, PLAYER_NUMBER, Positions, Walls, _, Eval) body */
    nth1(PLAYER_NUMBER, Positions, [PX,PY]), /*get current player position*/
    evaluate_min_real_distance([PX,PY], PLAYER_NUMBER, Walls, MinPDistance),
    /*manhattanDistance(PX,PY,PGX,PGY,MinPDistance),*/ /*distance for current player */
    findall(Distance, (
        member(ENTITY,[1,2,3,4]),
        ENTITY \= PLAYER_NUMBER,
        nth1(ENTITY, Positions, [EX,EY]),
        evaluate_min_real_distance([EX,EY], ENTITY, Walls, Distance)
        /*manhattanDistance(EX,EY,EGX,EGY,Distance)*/
    ), [D1,D2,D3]), /*All min distances for AI*/
    Min2 is min(D2, D3),
    MinEDistance is min(D1, Min2), /*Min goal distance for AI */
    Eval is MinEDistance - MinPDistance, !. /* Eval = difference between Player Min distance and AI Min distance. More Player distance = better eval, More AI distance = worse eval*/

    /*forall((member(ENTITY,[1,2,3,4]), ENTITY \= PLAYER_NUMBER), (
        nth0(ENTITY, Positions, EntityPos),
        evaluate_min_distance(EntityPos, [G1,G2,G3,G4], MinEDistance),
    ) )*/

/*evaluate(1, [[1,3],[2,4],[3,5],[8,0]], [4,4,4,4], [[1,5,'v'], [3,4,'h'], [4,8,'v'], [7,2,'v']], Eval).*/
/*member(D, [1,2,3,4]), nth1(D, [[1,3],[2,4],[3,5],[8,0]], [PX,PY]), evaluate_min_real_distance([PX,PY], D, [[1,5,'v'], [3,4,'h'], [4,8,'v'], [7,2,'v']], MinPDistance).*/

/* enemywins > playerwins > player_distance_from_goal = ennemi_distance_from_goal > NWalls */
/*getDirection([X1,Y1], [X2,Y2], Direction):-
    (
        X1 != X2 -> Direction = 'h'; Direction = 'v'
    ).*/
    
/* evaluate_real_distance([1,0], [2,1], [[0,1,'h'],[1,1,'v'],[2,0,'v']], D).*/
evaluate_real_distance(PlayerPos, GoalPos, Walls, Distance):-
    evaluate_min_path_to_point(PlayerPos, GoalPos, Walls, MinPath),
    length(MinPath, Distance).

/*nth1(3, [[1,3],[2,4],[3,5],[8,0]], [PX,PY]), goal(3, PGX, PGY), (PGY = PY ; PGX = PX), evaluate_min_path_to_point([PX,PY], [PGX,PGY], [[1,5,'v'], [3,3,'v'], [4,8,'v'], [7,2,'v']], PATH).*/
evaluate_min_path_to_point(PlayerPos, GoalPos, Walls, MinPath):-
    findall(PATH, (
            path(PlayerPos, GoalPos, [], Walls, PATH),
            last(PATH, Goal),
            Goal = GoalPos
        ),
        PATHS
    ),
    shortest_list(PATHS, MinPath).

shortest_list(Matrix, MinList) :-
    findall(LENGTH, (
            member(List, Matrix),
            length(List, LENGTH)
        ),
        LENGTHS
    ),
    min_list(LENGTHS, MinLength),
    nth0(Index, LENGTHS, MinLength),
    nth0(Index, Matrix, MinList), !.

/*member(D, [1,2,3,4]), nth1(D, [[1,3],[2,4],[3,5],[8,0]], [PX,PY]), evaluate_min_path([PX,PY], D, [[1,5,'v'], [3,4,'h'], [4,8,'v'], [7,2,'v']], MinPath).*/

/*nth1(D, [[1,3],[2,4],[3,5],[8,0]], [PX,PY]), evaluate_min_path([PX,PY], D, [[1,5,'v'], [3,3,'v'], [4,8,'v'], [7,2,'v']], PATH).*/
evaluate_min_path([PX,PY], PLAYER_NUMBER, Walls, MinPath):-
    findall(PATH, (
            goal(PLAYER_NUMBER, PGX, PGY),
            evaluate_min_path_to_point([PX,PY], [PGX,PGY], Walls, PATH)
        ),
        PATHS
    ),
    shortest_list(PATHS, MinPath).

evaluate_min_real_distance([PX,PY], PLAYER_NUMBER, Walls, Distance):-
    evaluate_min_path([PX,PY], PLAYER_NUMBER, Walls, MinPath),
    length(MinPath, Distance).

/*path([0,0], [2,0], [[0,0]], [[1,0,'h'],[1,1,'v']], P).*/
/*path([1,0], [2,1], [[1,0]], [[0,1,'h'],[1,1,'v'],[2,0,'v']], D).*/
path(PlayerPos, [GX,GY], RPATH, Walls, FPATH):-
    ((last(RPATH, Goal),
     append([],[GX,GY], Goal)) -> append([],FPATH, RPATH);
    findall(PATH,
        (next_position(PlayerPos, RPATH, Walls, PATH)),
        PATHS
    ),
    findall(Distance, (
            member(PATH, PATHS),
            last(PATH, [PX, PY]),
            manhattanDistance(PX,PY,GX,GY,Distance)
        ),
        Distances
    ),
    min_list(Distances, MinDistance),
    nth0(Index, Distances, MinDistance),
    nth0(Index, PATHS, NEPATH),
    last(NEPATH, Pos),
    path(Pos, [GX,GY], NEPATH, Walls, FPATH)).
/*path(PlayerPos, [GX,GY], RPATH, Walls, RPATH).*/

head([H,_], H).
/*next_position([0,0], [[1,1],[0,1]], [], P).*/
next_position([StartX, StartY], CURRENT_PATH, Walls, NEW_PATH):-
    on_board(X,Y),
    abs(X-StartX, AbsX),
    abs(Y-StartY, AbsY),
    (
        (AbsX is 1, AbsY is 0);
        (AbsX is 0, AbsY is 1)
    ),
    (AbsX = 0 -> no_walls_v(X,Y,Walls) ; no_walls_h(X,Y,Walls)),
    /*\+ member([X,Y, Direction ], Walls),*/
    \+ member([X,Y], CURRENT_PATH),
    append(CURRENT_PATH, [[X,Y]], NEW_PATH).


goal(1,X,Y):- Y is 8, on_board(X,Y).
goal(2,X,Y):- X is 0, on_board(X,Y).
goal(3,X,Y):- Y is 0, on_board(X,Y).
goal(4,X,Y):- X is 8, on_board(X,Y).

player_has_goal(PNB,Positions):-nth1(PNB,Positions,[X,Y]),goal(PNB,X,Y).

other_has_goal(PNB,Positions):-nth1(PNB,Positions,_,[[AX,AY],[BX,BY],[CX,CY]]),nth1(PNB,[1,2,3,4],_,[A,B,C]),
(goal(A,AX,AX);goal(B,BX,BY);goal(C,CX,CY)).

evaluate_min_distance(EntityPos, [G1,G2,G3,G4], MinDistance):-
    evaluate_distance_from_goal(EntityPos, G1, D1), /*G1 == Goal 1 (Pos)*/
    evaluate_distance_from_goal(EntityPos, G2, D2),
    evaluate_distance_from_goal(EntityPos, G3, D3),
    evaluate_distance_from_goal(EntityPos, G4, D4),
    Min1 is min(D3, D4),
    Min2 is min(D2, Min1),
    MinDistance is min(D1, Min2). /* Minimal distance between entity and one goal */

evaluate_distance_from_goal([PX,PY], [GX,GY], Distance):- /*P == player,  G == goal*/
    Distance is sqrt((GX-PX)^2 + (GY-PY)^2).

evaluate_distance_from_goal(PLAYER_NUMBER, Positions, GOAL_NUMBER, GoalsPos, Distance):-
    nth0(PLAYER_NUMBER, Positions, [X1,Y1]), /*get player position */
    nth0(GOAL_NUMBER, GoalsPos, [X2,Y2]), /* get goal position (this not work : goal(GOAL_NUMBER, X2, Y2)) */
    Distance is sqrt((X2-X1)^2 + (Y2-Y1)^2). /* set distance to sqrt((X2-X1)² + (Y2-Y1)²). */




%evaluate_distance_from_goal(1, [[1,2],[1,3],[2,4],[3,5]], 2, [[4,5],[3,8],[1,7],[3,0]], D).


/*search space*/

/*TEMPORARY EVALUATION*/
%temp_eval().

/*at max depth, just evaluate*/
minmax(_, PLAYER_NUMBER, Positions, Walls, NWalls, NextMove, U, 0):- U is 1.%evaluate(PLAYER_NUMBER, Positions, NWalls, Walls, U).

/*go down one in depth and find best utility/move starting from this state*/
minmax(OGPNB, PLAYER_NUMBER, Positions, Walls, NWalls, NextMove, U, Depth):-  Depth>0, D2 is Depth-1, find_best_from(OGPNB, PLAYER_NUMBER, Positions, Walls, NWalls, NextMove, U, D2).

find_best_from(OGPNB, PLAYER_NUMBER, Positions, Walls, NWalls, NextMove, U, Depth):- 
    /*get all possible states from current state*/
    findall([NewPositions,NewNWalls,NewWalls,Move], 
    possible_move(PLAYER_NUMBER, Positions, Walls, NWalls, Move, NewPlayerNumber, NewPositions, NewWalls, NewNWalls), 
    NewList),
    unzip_4(NewList,PositionsList,NWallsList,WallsList,MoveList),
    next_player_number(PLAYER_NUMBER, NewPlayerNumber),
    /*pick max from these states*/
    best_state(OGPNB, NewPlayerNumber, PositionsList, WallsList, NWallsList, MoveList, Depth, NextPositions, NextWalls, NextNWalls, NextMove, Eval).

/*if only one move, best move*/
best_state(OGPNB, PLAYER_NUMBER, [Positions], [Walls], [NWalls], [Move], Depth, Positions, Walls, Nwalls, Move, Eval) :-
    minmax(OGPNB, PLAYER_NUMBER, Positions, Walls, NWalls, Move, Eval, Depth).
/*else do minimax to get best value, compare to best value for other states*/
best_state(OGPNB, PLAYER_NUMBER, [Positions|OtherPos], [Walls|OtherWalls], [NWalls|OtherNWalls], [Move|Moves], Depth, BestPos, BestWalls, BestNWalls, BestMove, BestEval) :-
    minmax(OGPNB, PLAYER_NUMBER, Positions, Walls, NWalls, _, Eval1, Depth),
    best_state(OGPNB, PLAYER_NUMBER, OtherPos, OtherWalls, OtherNWalls, Moves, Depth, Pos2, Walls2, NWalls2, Move2, Eval2),
    best_of_2(OGPNB, PLAYER_NUMBER, Positions, Walls, NWalls, Move, Eval1, Pos2, Walls2, NWalls2, Move2, Eval2, BestPos, BestWalls, BestNWalls, BestMove, BestEval).


    /*if player has reached the goal, we don't care about values*/
best_of_2(OGPNB,PLAYER_NUMBER, Positions, Walls, NWalls, Move, Eval1, _, _, _, _, Eval2, Positions, Walls, NWalls, Move, Eval1):-
    /*Adjust because PLAYER_NUMBER reflects the number in the new state, not the player for whom we evaluate the move*/
    next_player_number(CurrentPNB, PLAYER_NUMBER),
    (
    (player_has_goal(OGPNB,Positions))
    ;
    (CurrentPNB = OGPNB,                
    Eval1 >= Eval2)                           
    ;
    (CurrentPNB \= OGPNB,                        
    Eval2 > Eval1)
    ). 
/*if not the first, the second*/
best_of_2(_,_,_,_,_,_,_, Positions, Walls, NWalls, Move, Eval, Positions, Walls, NWalls, Move, Eval).
/*
:-best_state(4,1,[[[5,0],[8,5],[5,8],[0,5]],[[5,0],[8,5],[5,8],[0,5]]],[[],[]],[[5,5,5,5],[5,5,5,5]],[_,_],1,P,W,NW,_,E).
minmax(4,1,[[5,0],[4,5],[5,8],[2,5]],[],[5,5,5,5],D,E,1).
*/