/*
The program just needs to be able to make a decision based on the state of the game
*/



/*DATA Representation
NWaals = [NWaals1, NWaals2, NWalls3, NWaals4]

Positions = [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]]

Walls = [[X1,Y1,'v'],[X2,Y2,'h']....] (top-left corner)

*/
player_separator(PLAYER_NUMBER, Positions, NWalls, PlayerNWalls, OtherNWalls, PlayerPos, OtherPos):- 
 nth1(PLAYER_NUMBER, Positions, PlayerPos, OtherPos), 
 nth1(PLAYER_NUMBER, NWalls, PlayerNWalls, OtherNWalls).






%-VALID STATES-

/*check that the pawn is at a valid coordinate*/
on_board([X,Y]):- X>0, X<9, Y>0, Y<9.
/*check that no two pawns are on the same space
for i in {1,2,3,4}{
    pick the i-th [X,Y] and for all remaining [X1,Y1]{
        check either X!=X1 OR Y!=Y1
    }
}
*/
notTwicePawn(Positions):- 
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

 notTwicePawn(Positions).



%valid_transitions Simon
ROUGH START/EXAMPLE
%valid_move(XTarg,YTarg,X,Y):- 
%valid_position(XTarg,YTarg),
%.

%move_one(PLAYER_NUMBER, Positions, XTarg,YTarg,X,Y,Walls):- valid_position(XTarg,YTarg), move_left(XTarg,X,Walls) ; move_right(XTarg,X,Walls); move_up(YTarg,Y,Walls); move_down(YTarg,Y,Walls);jump_over().
move_left(PLAYER_NUMBER, Positions, XTarg, X, Y,Walls):- XTarg is X, noWallsX(XTarg,X,Y,Walls).
move_up(PLAYER_NUMBER, Positions, YTarg, X, Y,Walls):- YTarg is Y, noWallsY(YTarg,X,Y,Walls).
move_right(PLAYER_NUMBER, Positions, XTarg, X, Y,Walls):- XTarg is X+1, noWallsX(XTarg,X,Y,Walls).
move_down(PLAYER_NUMBER, Positions, YTarg, X, Y,Walls):- YTarg is Y-1, noWallsY(YTarg,X,Y,Walls).



noWallsX(XTarg,X,Y,Walls):-forall(member([XTarg,Y1,'v'], Walls), (Y2 is Y1+1, Y1\=Y, Y2 \= Y)).
noWallsY(YTarg,X,Y,Walls):-forall(member([YTarg,X1,'h'], Walls), (X2 is X1-1, X1\=X, X2 \= X)).


% DiagonaleTopRight(X,Y,NewX,NewY):- [X is NewX-1]  AND  [position_invalide(NewX,NewY)] AND [cant_jump(X,Y,X+2,Y)] AND [(NoWall(X,Y,X,Y+1) OR NoWall(NewX,Y,NewX,Y+1))]


%'move_left and move_up will check for walls, remains to check for players'
%move_diag(XTarg,YTarg,X,Y,Walls):- (move_left;move_right),(move_up,move_down),...
%move_diag(XTarg,Ytarg,X,Y,Walls):- (move_left;move_right),(move_up;move_down).

%check if the case between start position and target position is a player and there is no wall behind it
%jump_over(PLAYER_NUMBER, Positions, XTarg,YTarg,X,Y,Walls):- not(notTwicePawn(Positions)), valid_position(XTarg,YTarg)


place_wall():- .

next_move():- .





%choose_evaluation Jeremy
/*evaluation should represent the utility of a certain state for:
-The current player
-All other participants (as one entity -> just sum or more complex?)*/

evaluate(PlayePos,Walls,[NW1,NW2,NW3,NW4], evaluation):- .
/* enemywins > playerwins > player_distance_from_goal = ennemi_distance_from_goal > NWalls */



%alpha_beta Marien

/*check evaluation of final state (at some depth) for every possible move from:
-The current player
-All other participants (as one entity)
*/
alpha_beta():- .