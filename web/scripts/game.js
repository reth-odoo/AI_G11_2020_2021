//CONSTANTS
//C_board
let BOARD = null

//C_Color
const Red = "rgba(255, 0, 0, 1)"
const Yellow = "rgba(255, 155, 0, 1)"
const Blue = "rgba(0, 0, 255, 1)"
const Green = "rgba(0, 255, 0, 1)"
const Grey = "rgba(100,100,100,1)"
const Black = "rgba(0,0,0,1)"

const SpaceColor = Grey
const WallColor = Black

//C_Display
const PawnRatio = 0.5

//C_Fonts
const Font = '20px serif'

//C_Players
const BaseWalls = 20;


//utility
function intDiv(divid, divis) {
    return Math.floor(divid / divis)
}

function manhattanDistance(x1, y1, x2, y2) {
    return absolute(x1 - x2) + absolute(y1 - y2)
}

function absolute(x) {
    if (x < 0)
        return -x;
    return x;
}

//startup
$(window).on("load", () => {
        let canvas = $("#gameCanvas")[0]
        let game_board = new Board(canvas, 50, 50)
        game_board.init_board()
        game_board.draw()
        BOARD = game_board
    }

)





class Board {
    constructor(canv, x_offset, y_offset) {
            this.canvas = canv
            this.context = canv.getContext("2d")

            this.space_size = 30;
            this.slit_size = 5;
            this.x_offset = x_offset;
            this.y_offset = y_offset;

            this.size = 9;

            //board_rep is a 2D-array of pawnSpaces
            this.board_rep = this.init_board()
            this.pawns = this.init_pawns()

        }
        //returns null if invalid (parent_space must not be utmost right or down), new wall if valid
    add_wall(parent_space, placed_by, goes_down) {


        let x = parent_space.x
        let y = parent_space.y
        let board = this.board_rep

        if (placed_by.wall_nb <= 0) {
            console.error("pawn tried to place a wall they didn't have")
            return null
        }
        if (parent_space === null) {
            console.error("parent space for wall is null")
            return null
        }
        if ((x >= this.size - 1 && goes_down) || (y >= this.size - 1 && !goes_down)) {
            console.error("tried to place a wall with an origin that would put it outside the board")
            return null
        }
        //check no wall starting on same space
        let origin_space = parent_space
        let o_walls = origin_space.walls


        for (let i = 0; i < o_walls.length; i++) {
            let wall = o_walls[i]
            if (wall.space === parent_space) {
                console.log("wall already set on that space")
                return null
            }
        }

        //check no wall in the same direction starting next space
        let next_space = null
        let next_x = x
        let next_y = y
        if (goes_down)
            next_y = y + 1
        else
            next_x = x + 1

        if (next_x < BOARD.size && next_y < BOARD.size) {
            next_space = board[next_x][next_y]

            let n_walls = next_space.walls;

            for (let i = 0; i < n_walls.length; i++) {
                let wall = n_walls[i]
                if (wall.space === next_space)
                    if (wall.goes_down === goes_down) {
                        console.log("overlapping walls")
                        return null
                    }
            }
        }
        //check no wall in the same direction starting previous space
        let prev_space = null
        let prev_x = x
        let prev_y = y
        if (goes_down)
            prev_y = y - 1
        else
            prev_x = x - 1

        if (prev_x >= 0 && prev_y >= 0) {
            prev_space = board[prev_x][prev_y]

            let n_walls = prev_space.walls;

            for (let i = 0; i < n_walls.length; i++) {
                let wall = n_walls[i]
                if (wall.space === prev_space)
                    if (wall.goes_down === goes_down) {
                        console.log("overlapping walls")
                        return null
                    }
            }
        }
        //known OK the fact that wall only go down and right
        let new_wall = new Wall(parent_space, goes_down)

        origin_space.walls.push(new_wall)
        next_space.walls.push(new_wall)
        if (goes_down) {
            board[x + 1][y + 1].walls.push(new_wall)
            board[x + 1][y].walls.push(new_wall)
        } else {
            board[x][y + 1].walls.push(new_wall)
            board[x + 1][y + 1].walls.push(new_wall)
        }

        placed_by.wall_nb -= 1

        new_wall.draw()

        return new_wall
    }

    init_board() {
        let board_rep = []
        let size = this.size

        for (let x = 0; x < size; x++) {
            board_rep.push([])
            for (let y = 0; y < size; y++) {
                board_rep[x].push(new PawnSpace(this, x, y))
            }
        }

        return board_rep
    }

    init_pawns() {
        let board = this.board_rep

        let size = this.size
        let pawns = []

        let s_half = intDiv(size, 2)
            //space and goals are set after this

        let red = new Pawn(null, Red, null, null, false, BaseWalls);
        let blue = new Pawn(null, Blue, null, null, true, BaseWalls);
        let green = new Pawn(null, Green, null, null, false, BaseWalls);
        let yellow = new Pawn(null, Yellow, null, null, true, BaseWalls);

        //add to Pawns and set parameters in the same order
        pawns = [red, blue, green, yellow]
        let spaces = [board[s_half][0], board[0][s_half], board[s_half][size - 1], board[size - 1][s_half]];
        let goals_y = [size - 1, 0, size - 1, 0]
        let goals_x = [0, size - 1, 0, size - 1]

        for (let i = 0; i < pawns.length; i++) {
            let pawn = pawns[i]
            pawn.setSpace(spaces[i])
            pawn.goal_x = goals_x[i]
            pawn.goal_y = goals_y[i]
        }
        return pawns
    }

    draw() {
        let slit_size = this.slit_size
        let space_size = this.space_size
        let size = this.size

        let ctx = this.context

        let x_offset = this.x_offset
        let y_offset = this.y_offset

        this.drawPawnSpaces()

        let H_x = x_offset + intDiv(space_size, 2) - slit_size
        let H_y = y_offset + intDiv(space_size, 2) + ((1 + size) * slit_size) + (size * space_size)
        let V_x = x_offset - space_size
        let V_y = y_offset + ((1 + size) * slit_size) + (size * space_size) - intDiv(space_size, 2)
        this.drawAxis(V_x, V_y, 1, 0, -1, false)
        this.drawAxis(H_x, H_y, 'A', 1, 0, true)


        let pawns = this.pawns
        for (let i = 0; i < pawns.length; i++) {
            pawns[i].draw()
        }

    }

    /**
     * draws axis with sequential characters/integer for each grid row/collumn
     * @param {int/char} init_char, initial character/number (if character, use alphabet) 
     * @param {int} x_inc, incrementation factor for x coordinate in number of grid spaces
     * @param {int} y_inc, ^same for y
     * @param {bool} alphabet, false if displaying sequential integers 
     * @example drawAxis(ctx, 0, 0, 12, -1, 0, false) =>   12 13 14
     */
    drawAxis(start_x, start_y, init_char, x_inc, y_inc, alphabet) {

        let ctx = this.context

        let space_size = this.space_size
        let slit_size = this.slit_size

        let size = this.size

        let A_char = ' '

        if (alphabet) {
            A_char = init_char.charCodeAt(0)
        } else {
            A_char = init_char
        }

        ctx.font = Font

        for (let i = 0; i < size; i++) {
            let disp_char = ' '
            if (alphabet) {
                disp_char = String.fromCharCode(A_char + i)
            } else {
                disp_char = (A_char + i).toString()
            }
            let x = start_x + ((1 + i * x_inc) * slit_size) + (i * x_inc * space_size)
            let y = start_y + ((1 + i * y_inc) * slit_size) + (i * y_inc * space_size)
            ctx.fillText(disp_char, x, y, space_size)
        }

    }

    drawPawnSpaces() {
        let ctx = this.context
        let size = this.size

        let start_x = this.x_offset
        let start_y = this.y_offset

        let board_rep = this.board_rep

        ctx.fillStyle = SpaceColor

        for (let x = 0; x < size; x++) {
            for (let y = 0; y < size; y++) {
                board_rep[x][y].draw()
            }
        }


    }
}

class PawnSpace {
    /**
     * Create a pawn space
     * @param {number} x, the position of the space
     * @param {number} y, the position of the space
     * @param {Pawn} pawn, the pawn on the space or null
     * @param {[Wall]} walls, the walls adjacent to the piece
     */
    constructor(board, x, y) {
        this.board = board

        this.x = x;
        this.y = y;
        this.pawn = null;
        this.walls = [];


        //compute real x and y (shouldn't need to change)
        let start_x = board.x_offset
        let start_y = board.y_offset
        let space_size = board.space_size
        let slit_size = board.slit_size

        this.real_x = start_x + ((1 + x) * slit_size) + (x * space_size);
        this.real_y = start_y + ((1 + y) * slit_size) + (y * space_size);
    }

    equals(space) {
        if (this.board === space.board && this.x === space.x && this.y === space.y)
            return true
        return false
    }

    draw() {

        let board = this.board
        let ctx = board.context


        let space_size = board.space_size


        ctx.fillStyle = SpaceColor;
        ctx.fillRect(this.real_x, this.real_y, space_size, space_size);

    }

}
class Pawn {
    /**
     * Create a pawn 
     * @param {PawnSpace} space 
     * @param {hex_colorcode} color
     * @param {number} goal_y, null if any space
     * @param {number} goal_x , null if any space
     * @param {number} wall_nb
     */
    constructor(space, color, goal_x, goal_y, AI, wall_nb) {
        this.space = space;
        this.color = color;
        this.goal_x = goal_x;
        this.goal_y = goal_y;
        this.is_AI = AI;
        this.wall_nb = wall_nb;
    }
    setSpace(space) {
        //if doesn't exist or already has pawn
        if (!space || space.pawn) {
            console.log("illegal pawn space set")
            return false
        }
        //if already on a space, empty it
        if (this.space)
            this.space.pawn = null;
        this.space = space;
        space.pawn = this;
    }
    moveTo(space) {
        if (!space) {
            console.error('Pawn.moveTo : no space provided')
            return false
        }
        if (space.pawn) {
            console.error('Pawn.moveTo : tried moving onto another pawn')
            return false
        }
        //if not on a space, just set it
        if (!this.space) {
            this.setSpace(space)
            return true
        }

        let current_x = this.space.x
        let current_y = this.space.y
        let target_x = space.x
        let target_y = space.y

        let dist = manhattanDistance(current_x, current_y, target_x, target_y)
        if (dist == 1) {
            //if going right next, check that no wall blocking on this space
            let walls = this.space.walls;
            for (let i = 0; i < walls.length; i++) {
                if (walls[i].blocks(this.space, space)) {
                    console.error('Pawn.moveTo : wall blocking this path')
                    return false
                }
            }
            this.setSpace(space)
            return true
        } else if (dist == 2) {
            this.setSpace(space)
            return true
                //TODO check that pawn between third space in that cardinal direction
                //AND (wall blocking the way between that space and the third ''') OR (pawn on the third ''') 
        } else if (dist == 3) {
            this.setSpace(space)
            return true
                //TODO check that other pawn in front
        }
        console.error('Pawn.moveTo : tried to move too far ', dist)
        return false
    }

    draw() {
        let space = this.space
        let x = space.real_x
        let y = space.real_y

        let board = this.space.board

        let space_size = board.space_size
        let h_size = space_size / 2

        let display_x = x + h_size
        let display_y = y + h_size

        let ctx = board.context

        let radius_size = (board.space_size * PawnRatio) / 2

        ctx.beginPath()
        ctx.fillStyle = this.color
        ctx.arc(display_x, display_y, radius_size, 0, 2 * Math.PI);
        ctx.fill();
        ctx.closePath()



    }
}
class Wall {
    //if it doesn't go down, it goes right
    constructor(parent_space, goes_down) {
        this.space = parent_space
        this.goes_down = goes_down

    }
    blocks(space1, space2) {
        //if they're offset on the same axis as the wall blocks
        if ((this.goes_down && absolute(space1.x - space2.x) > 0) || (!this.goes_down && absolute(space1.y - space2.y) > 0)) {
            //if both spaces are adjacent to the wall true
            let s1w = space1.walls
            let s2w = space2.walls
            for (let i1 = 0; i1 < s1w.length; i1++) {
                for (let i2 = 0; i2 < s2w.length; i2++) {
                    if (s1w[i1] === s2w[i2] && s1w[i1] === this)
                        return true
                }
            }
        }
        return false
    }
    draw() {
        let space = this.space
        let board = space.board
        let space_size = board.space_size
        let slit_size = board.slit_size

        let slit_length = space_size + slit_size + space_size

        let orig_x = space.real_x
        let orig_y = space.real_y
        let size_x = 0
        let size_y = 0

        if (this.goes_down) {
            orig_x = orig_x + space_size
            size_x = slit_size
            size_y = slit_length
        } else {
            orig_y = orig_y + space_size
            size_x = slit_length
            size_y = slit_size
        }

        let ctx = board.context
        ctx.fillStyle = WallColor
        ctx.fillRect(orig_x, orig_y, size_x, size_y)
    }

}

//Turn settings

//in order of play(clockwise)
NbPlayers = 4
PlayersAre = ["human", "ai", "human", "ai"]
ColorTurn = ["Red", "Yellow", "Green", "Blue"]
TurnOf = 0

function nextTurn() {
    if (TurnOf >= NbPlayers - 1) {
        TurnOf = 0
    } else if (TurnOf >= 0) {
        TurnOf += 1
    } else {
        TurnOf = 0
        alert("Turns were scrambled, back to 0")
    }
    document.getElementById("turnIndicator").innerHTML = `Turn of ${ColorTurn[TurnOf]}`
}

//simplified interface

function displayError(errorString) {
    document.getElementById("gameErrorBox").innerHTML = errorString
}

function moveTo(x, y) {
    x = x - 1
    y = BOARD.size - y

    if (x < 0 || x > BOARD.size - 1 || y < 0 || y > BOARD.size - 1)
        alert("invalid coordinates")

    let p = BOARD.pawns[TurnOf]

    if (!p.moveTo(BOARD.board_rep[x][y])) {
        displayError("Illegal move")
    } else {
        BOARD.draw()
        nextTurn()
    }
}

function placeWall(x, y, vert) {
    x = x - 1
    y = BOARD.size - y

    let p = BOARD.pawns[TurnOf]

    if (p.wall_nb > 0) {
        if (BOARD.add_wall(BOARD.board_rep[x][y], p, vert)) {
            BOARD.draw()
            nextTurn()
        } else
            displayError("invalid wall position")
    } else {
        displayError("you do not have any more walls")
    }
    BOARD.draw()
}