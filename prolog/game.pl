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


%valid_states
on_board([X,Y]):- X>0, X<9, Y>0, Y<9.
notTwicePawn(Positions):- 
 forall(member(PLAYER_NUMBER,[1,2,3,4]),
     (
         forall(
         (nth1(PLAYER_NUMBER, Positions, [X,Y], OtherPos),member([X1,Y1], OtherPos)), 
         (X\=X1; Y\=Y1))
        )
        ).

valid_position(Positions):- 
 forall(member(P, Positions),(on_board(P))), 

 notTwicePawn(Positions).

%valid_transitions Simon
move(XDif,YDif,X,Y,):- .
jump_over():- .






next_move():- .

%choose_evaluation Jeremy
evaluate(PlayePos,Walls,[NW1,NW2,NW3,NW4]):- .
/* enemywins > playerwins > player_distance_from_goal = ennemi_distance_from_goal > NWalls */

%alpha_beta Marien
alpha_beta():- .