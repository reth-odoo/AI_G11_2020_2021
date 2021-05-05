/*
The program just needs to be able to make a decision based on the state of the game
*/


/*UTILITY*/

/*biggest on 3, smallest on 4*/
biggest_of_2(X,Y,X,Y):- X>Y.
biggest_of_2(X,Y,Y,X):- Y>X.



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
on_board([X,Y]):- X>0, X<9, Y>0, Y<9.


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


/*call all state validity check*/
valid_position(Positions):- 
 forall(member(P, Positions),(on_board(P))), 

 not_twice_pawn(Positions).





/*Walls placement*/



intersecting_walls(X1,Y1,'h',X2,Y2,'h'):- Y1=Y2, (X1=X2 ; X1 is X2-1 ; X1 is X2 + 1).
intersecting_walls(X1,Y1,'v',X2,Y2,'v'):- X1=X2, (Y1=Y2 ; Y1 is Y2-1 ; Y1 is Y2 + 1).
intersecting_walls(X1,Y1,'v',X2,Y2,'h'):- X1=X2, Y1=Y2.
intersecting_walls(X1,Y1,'h',X2,Y2,'v'):- intersecting_walls(X2,Y2,'v',X1,Y1,'h').

valid_wall_placement(X1,Y1,'v'):- X1<7, Y1<8, X1>=0, Y1>=0.
valid_wall_placement(X1,Y1,'h'):- X1<8, Y1<7, X1>=0, Y1>=0.


%at_least_one_path(P,Goal):-.

%path_find(X1,Y1,X2,Y2):-X1=X2,Y1=Y2.
%path_find(X1,Y1,X2,Y2):-move_one(X1,Y1,X3,Y3),path_find(X3,Y3,X2,Y2).


wall_ok(X1,Y1,'v', Walls):-  forall(member([X2,Y2,Orientation],Walls),
                                (valid_wall_placement(X1,Y2,'v'),
                                not(intersecting_walls(X1,Y1,'v',X2,Y2,Orientation)))
                            ).




/*for all walls starting on this Y or the level above, check that X not between the coordinates*/
no_walls_x(XTarg,XOrig,Y,Walls):-forall((Y2 is Y-1, (member([X,Y,'v'], Walls) ; member([X,Y2,'v'],Walls)))
                            , (biggest_of_2(XTarg,XOrig,BigX,SmallX), (BigX =< X ; SmallX > X))).

no_walls_x(YTarg,YOrig,X,Walls):-forall((X2 is X-1, (member([X,Y,'h'], Walls) ; member([X2,Y,'h'],Walls)))
                            , (biggest_of_2(YTarg,YOrig,BigY,SmallY), (BigY =< Y ; SmallY > Y))).




/*Valid Transitions (Moves)*/




move_one(PLAYER_NUMBER, Positions, XTarg,YTarg,X,Y,Walls):- valid_position(XTarg,YTarg), move_left(XTarg,X,Walls) ; move_right(XTarg,X,Walls); move_up(YTarg,Y,Walls); move_down(YTarg,Y,Walls).
move_left(PLAYER_NUMBER, Positions, XTarg, X, Y,Walls):- XTarg is X-1, no_walls_x(XTarg, X,Y,Walls).
move_up(PLAYER_NUMBER, Positions, YTarg, X, Y,Walls):- YTarg is Y+1, no_walls_y(YTarg, Y,X,Walls).
move_right(PLAYER_NUMBER, Positions, XTarg, X, Y,Walls):- XTarg is X+1, no_walls_x(XTarg, X,Y,Walls).
move_down(PLAYER_NUMBER, Positions, YTarg, X, Y,Walls):- YTarg is Y-1, no_walls_y(YTarg, Y,X,Walls).



/*DiagonaleTopRight(X,Y,NewX,NewY):- [X is NewX-1]  AND  [position_invalide(NewX,NewY)] AND [cant_jump(X,Y,X+2,Y)] AND [(NoWall(X,Y,X,Y+1) OR NoWall(NewX,Y,NewX,Y+1))]
*/

/*move_diag(XTarg,YTarg,X,Y,Walls):- (move_left;move_right),(move_up,move_down),...
move_diag(XTarg,Ytarg,X,Y,Walls):- (move_left;move_right),(move_up;move_down).*/

/*check if the case between start position and target position is a player and there is no wall behind it*/
%jump_over(PLAYER_NUMBER, Positions, XTarg,YTarg,X,Y,Walls):- not(notTwicePawn(Positions)), valid_position(XTarg,YTarg)
jump_over_right(PLAYER_NUMBER, Positions, XTarg,YTarg,X,Y,Walls):-X2 is X1+1, X1 is X+1,Y1 is Y+1, not(empty_case(Positions,X1,Y1)), valid_position(XTarg,YTarg),no_walls_x(X2,Y,Walls),no_walls_x(X1,Y,Walls).
%or
jump_over_right(PLAYER_NUMBER, Positions,X,Y,Walls):-X2 is X1+1, X1 is X+1,Y1 is Y+1, not(empty_case(Positions,X1,Y1)), valid_position(X2,Y),no_walls_x(X2,Y,Walls),no_walls_x(X1,Y,Walls).

%place_wall():- wall_ok, .



next_player_number(3,0).
next_player_number(NB,NewNB):- NB>=0, NB<3, NewNB is NB+1.

%possible_move(StateIn, StateOut, Move):- place_wall ; move_one ; jump_over ; move_diag.





%choose_evaluation Jeremy
/*evaluation should represent the utility of a certain state for:
-The current player
-All other participants (as one entity -> just sum or more complex?)*/

%evaluate(PlayePos,Walls,[NW1,NW2,NW3,NW4], evaluation):- .
/* enemywins > playerwins > player_distance_from_goal = ennemi_distance_from_goal > NWalls */



goal(1,X,Y):- Y is 8, on_board(X,Y).
goal(2,X,Y):- X is 0, on_board(X,Y).
goal(3,X,Y):- Y is 0, on_board(X,Y).
goal(4,X,Y):- X is 8, on_board(X,Y).

%evaluate_distance_from_goal(PLAYER_NUMBER).





%alpha_beta Marien

/*check evaluation of final state (at some depth) for every possible move from:
-The current player
-All other participants (as one entity)
*/

/*OGPNB = original player number, the actual current player*/
/*minimax(OGPNB, PLAYER_NUMBER, Positions, Walls, NextMove, Eval) :-*/
    /*get all possible states*/
/*  findall(NewPositions, possible_move(PLAYER_NUMBER, Positions, Walls, NewPlayerNumber, NewPositions, NewWalls, Move), PositionList),
    findall(NewWalls, possible_move(PLAYER_NUMBER, Positions, Walls, NewPlayerNumber, NewPositions, NewWalls, Move), NewWalls),
    findall(Move, possible_move(PLAYER_NUMBER, Positions, Walls, NewPlayerNumber, NewPositions, NewWalls, Move), Moves),
    next_player_number(PLAYER_NUMBER, NewPlayerNumber),*/
    /*find the best amongst them*/
/*   best(OGPNB, NewPlayerNumber, Positions, Walls, Moves, NextPNB, NextPositions, NextWalls, NextMove, Eval), !.*/
/*
minimax(OGPNB, PLAYER_NUMBER, Positions, Walls, _, Eval) :-
    evaluate(PLAYER_NUMBER, Positions, Walls, Eval).


best(OGPNB, PLAYER_NUMBER, [Positions], [Walls], [Move], State, Eval) :-
    minimax(PLAYER_NUMBER, Position, Wall, _, Eval), !.

best(OGPNB, PLAYER_NUMBER, [Positions|OtherPos], [Walls|OtherWalls], [Move|Moves] , BestPNB, BestPos, BestWalls, BestMove, BestEval) :-
    minimax(PLAYER_NUMBER, Positions, Walls, _, Eval1),
    best(PLAYER_NUMBER, OtherPos, OtherWalls, Moves, PlayerNB2, Pos2, Walls2, Move2, Eval2),
    betterOf(PLAYER_NUMBER, Positions, Walls, Move, Eval1, PlayerNB2, Pos2, Walls2, Move2, Eval2, BestPNG, BestPos, BestWalls, BestMove, BestEval).


betterOf(OGPNB, PNB, Positions, Walls, Move, Eval, _, _, _, _, Eval1, PNB, Positions, Walls, Move, Eval) :- */
    /*minimize for all others, maximize for self*/
/*    PNB = OGPNB,                         
    Eval0 > Eval1, !                             
    ;
    PNB \= OGPNB,                        
    Eval0 < Val1, !.                            

betterOf(_, _, _, _, _, _, _, PNB, Positions, Walls, Move, Eval, Eval1, State1, Eval1). */