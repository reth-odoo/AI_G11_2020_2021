//"constants" for drawing
let Canvas = null
let Context = null

//CONSTANTS
//C_Color
const Red = "rgba(255, 0, 0, 1)"
const Yellow = "rgba(255, 155, 0, 1)"
const Blue = "rgba(0, 0, 255, 1)"
const Green = "rgba(0, 255, 0, 1)"
const Grey = "rgba(100,100,100,1)"

const SpaceColor = Grey

//C_Fonts
const Font = '20px serif'

//C_Display
const SpaceSize = 30;
const SlitSize = 5;
const BoardOffsetX = 50;
const BoardOffsetY = 50;

//C_Board
const BoardSize = 9;

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
        Canvas = $("#gameCanvas")[0]
        Context = Canvas.getContext("2d")
        initBoard(BoardSize)
        initPawns(BoardSize)
        drawBoard(Context)
    }

)

//useful global variables
let Board = []
let Pawns = []
let Walls = []

function initBoard(size) {
    for (let x = 0; x < size; x++) {
        Board.push([])
        for (let y = 0; y < size; y++) {
            Board[x].push(new PawnSpace(x, y))
        }
    }
}

function initPawns(size) {

    let s_half = intDiv(size, 2)
        //space and goals are set after this

    let red = new Pawn(null, Red, null, null, false, BaseWalls);
    let blue = new Pawn(null, Blue, null, null, true, BaseWalls);
    let green = new Pawn(null, Green, null, null, false, BaseWalls);
    let yellow = new Pawn(null, Yellow, null, null, true, BaseWalls);

    //add to Pawns and set parameters in the same order
    Pawns = [red, blue, green, yellow]
    let spaces = [Board[s_half][0], Board[0][s_half], Board[s_half][size - 1], Board[size - 1][s_half]];
    let goals_y = [size - 1, 0, size - 1, 0]
    let goals_x = [0, size - 1, 0, size - 1]

    for (let i = 0; i < Pawns.length; i++) {
        let pawn = Pawns[i]
        pawn.setSpace(spaces[i])
        pawn.goal_x = goals_x[i]
        pawn.goal_y = goals_y[i]
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
    constructor(x, y) {
        this.x = x;
        this.y = y;
        this.pawn = null;
        this.walls = null;
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
            return false
        }
        if (space.pawn) {
            console.log("tried moving a pawn on a pawn")
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
            let walls = this.space;
            for (let i = 0; i < walls.length; i++) {
                if (walls[i].blocks(this.space, space))
                    return false
            }
            this.setSpace(space)
            return true
        } else if (dist == 2) {
            //TODO
        }
    }
}
class Wall {
    //if it doesn't go down, it goes right
    constructor(x, y, goes_down) {
        this.x = x;
        this.y = y;
        this.goes_down = goes_down;
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
    setWall(x, y, goes_down, placed_by) {
        if (placed_by.wall_nb <= 0) {
            console.log("pawn tried to place a wall they didn't have")
            return false
        }
        //check no wall starting on same space
        let origin_space = Board[x][y];
        let o_walls = origin_space.walls;

        for (let i = 0; i < o_walls.length; i++) {
            let wall = o_walls[i]
            if (wall.x === x && wall.y === y)
                return false
        }

        //check no wall in the same direction starting next space
        let next_space = null
        if (goes_down)
            next_space = Board[x][y - 1]
        else
            next_space = Board[x + 1][y]

        let n_walls = next_space.walls;

        for (let i = 0; i < n_walls.length; i++) {
            let wall = n_walls[i]
            if (wall.x === x && wall.y === y)
                if (wall.goes_down === goes_down)
                    return false
        }
        //known OK the fact that wall only go down and right
        //TODO: Add the wall to the list of adjacent walls for all relevant spaces
        //TODO: Add to the wall list if we decide to use it(mostly for drawing)
    }

}


/**
 * Draws everything
 * @param {html5-canvas-context} context 
 */
function drawBoard(ctx) {

    drawPawnSpaces(ctx, BoardOffsetX, BoardOffsetY)

    let H_x = BoardOffsetX + intDiv(SpaceSize, 2) - SlitSize
    let H_y = BoardOffsetY + intDiv(SpaceSize, 2) + ((1 + BoardSize) * SlitSize) + (BoardSize * SpaceSize)
    let V_x = BoardOffsetX - SpaceSize
    let V_y = BoardOffsetY + ((1 + BoardSize) * SlitSize) + (BoardSize * SpaceSize) - intDiv(SpaceSize, 2)
    drawAxis(ctx, V_x, V_y, 1, 0, -1, false)
    drawAxis(ctx, H_x, H_y, 'A', 1, 0, true)

}

function drawPawnSpaces(ctx, start_x, start_y) {

    ctx.fillStyle = SpaceColor

    display_x = 0
    display_y = 0

    for (let x = 0; x < BoardSize; x++) {
        for (let y = 0; y < BoardSize; y++) {
            drawPawnSpace(ctx, Board[x][y], start_x, start_y);
        }
    }


}

function drawPawnSpace(ctx, space, start_x, start_y) {
    let x = space.x;
    let y = space.y;

    let display_x = start_x + ((1 + x) * SlitSize) + (x * SpaceSize);
    let display_y = start_y + ((1 + y) * SlitSize) + (y * SpaceSize);

    ctx.fillStyle = SpaceColor;
    ctx.fillRect(display_x, display_y, SpaceSize, SpaceSize);

}

function drawAxis(ctx, start_x, start_y, init_char, x_inc, y_inc, alphabet) {

    let A_char = ' '

    if (alphabet) {
        A_char = init_char.charCodeAt(0)
    } else {
        A_char = init_char
    }

    ctx.font = Font

    for (let i = 0; i < BoardSize; i++) {
        console.log(A_char)
        let disp_char = ' '
        if (alphabet) {
            disp_char = String.fromCharCode(A_char + i)
        } else {
            disp_char = (A_char + i).toString()
        }
        let x = start_x + ((1 + i * x_inc) * SlitSize) + (i * x_inc * SpaceSize)
        let y = start_y + ((1 + i * y_inc) * SlitSize) + (i * y_inc * SpaceSize)
        ctx.fillText(disp_char, x, y, SpaceSize)
    }

}

//may not be needed, draw them when drawing spaces
function drawPawns() {

}

//could be drawn while spaces are drawn
function drawWalls() {

}