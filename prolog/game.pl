/*
The program just needs to be able to make a decision based on the state of the game
*/

/*DATA Representation
NWaals = [NWaals1, NWaals2, NWaals3]

Positions = [[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]]

Walls = [[X1,Y1,'v'],[X2,Y2,'h']....] (top-left corner)

*/
player_separator(PLAYER_NUMBER, Positions, NWalls, PlayerNWalls, OtherNWalls, PlayerPos, OtherPos):- 
 nth1(PLAYER_NUMBER, Positions, PlayerPos, OtherPos), 
 nth1(PLAYER_NUMBER, , PlayerPos, OtherPos).


%valid_states
state():- .

%valid_transitions
transition_():- .

next_move():- .

evaluate(PlayePos,Walls,[NW1,NW2,NW3,NW4]):- .
/* enemywins > playerwins > player_distance_from_goal = ennemi_distance_from_goal > NWalls */