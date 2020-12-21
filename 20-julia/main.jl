TOP = 1
RIGHT = 2
BOTTOM = 3
LEFT = 4

struct Tile
    id :: Int
    rows :: Array{String, 1}
    sides :: Array{String, 1}
end

function sides_of_rows(rows :: Array{String, 1}) :: Array{String, 1}
    sides = Array{Char,1}[]

    for _ in 1:4
        push!(sides, Array{Char, 1}())
    end

    for i in 1:10
        push!(sides[TOP], rows[1][i])
        push!(sides[RIGHT], rows[i][10])
        push!(sides[BOTTOM], rows[10][i])
        push!(sides[LEFT], rows[i][1])
    end

    result = String[]
    for i in 1:4
        push!(result, String(sides[i]))
    end
    return result
end

function rotate_lines(lines :: Array{String, 1}) :: Array{String, 1}
    h = length(lines)
    w = length(lines[1])

    result = String[]
    for i in 1:w
        row = Char[]
        for j in 1:h
            push!(row, lines[h - j + 1][i])
        end
        push!(result, String(row))
    end
    return result
end

function rotate(tile :: Tile) :: Tile
    rows = rotate_lines(tile.rows)
    # rows = String[]
    # for i in 1:10
    #     row = Char[]
    #     for j in 1:10
    #         push!(row, tile.rows[10 - j + 1][i])
    #     end
    #     push!(rows, String(row))
    # end

    return Tile(tile.id, rows, sides_of_rows(rows))
end

# 1 2 3    3 2 1
# 4 5 6 => 6 5 4
# 7 8 9    9 8 7
#
# 7 8 9
# 4 5 6  
# 1 2 3

function flip_lines(lines :: Array{String, 1}) :: Array{String, 1}
    rows = String[]
    h = length(lines)
    for i in 1:h
        push!(rows, lines[h - i + 1])
    end
    return rows
end

function flip(tile :: Tile) :: Tile
    # rows = String[]
    # for i in 1:10
    #     push!(rows, tile.rows[10 - i + 1])
    # end
    rows = flip_lines(tile.rows)
    return Tile(tile.id, rows, sides_of_rows(rows))
end

function parse_file(file_path :: String) :: Array{Tile, 1}
    tiles = Tile[]
    open(file_path) do file
        parsing_tile = true

        id = 0
        rows = String[]
        for line in eachline(file)
            if parsing_tile
                id = parse(Int, split(split(line, " ")[2], ":")[1])
                parsing_tile = false
            else
                if length(line) > 0
                    push!(rows, line)
                else
                    push!(tiles, Tile(id, rows, sides_of_rows(rows)))
                    rows = String[]
                    parsing_tile = true
                end
            end
        end
        push!(tiles, Tile(id, rows, sides_of_rows(rows)))
    end
    return tiles
end

function find_right_bottom(left :: Tile, top :: Tile, tiles :: Array{Tile, 1}) :: Tile
    for tile in tiles
        if top.id != tile.id && left.id != tile.id
            result = tile

            for i in 1:4
                if top.sides[BOTTOM] == result.sides[TOP] && left.sides[RIGHT] == result.sides[LEFT]
                    return result
                end
                result = rotate(result)
            end

            result = flip(tile)

            for i in 1:4
                if top.sides[BOTTOM] == result.sides[TOP] && left.sides[RIGHT] == result.sides[LEFT]
                    return result
                end
                result = rotate(result)
            end
        end
    end

    @assert(false, "Could not find solution for find_right_bottom")
end

function find_bottom(top :: Tile, tiles :: Array{Tile, 1}) :: Tile
    for tile in tiles
        if top.id != tile.id
            bottom = tile

            for i in 1:4
                if top.sides[BOTTOM] == bottom.sides[TOP]
                    return bottom
                end
                bottom = rotate(bottom)
            end

            bottom = flip(tile)

            for i in 1:4
                if top.sides[BOTTOM] == bottom.sides[TOP]
                    return bottom
                end
                bottom = rotate(bottom)
            end
        end
    end

    @assert(false, "Could not find solution for find_right")
end

function find_right(left :: Tile, tiles :: Array{Tile, 1}) :: Tile
    for tile in tiles
        if left.id != tile.id
            right = tile
            for i in 1:4
                if left.sides[RIGHT] == right.sides[LEFT]
                    return right
                end
                right = rotate(right)
            end

            right = flip(tile)

            for i in 1:4
                if left.sides[RIGHT] == right.sides[LEFT]
                    return right
                end
                right = rotate(right)
            end
        end
    end

    @assert(false, "Could not find solution for find_right")
end

function side_matches(candidate :: Tile, tiles :: Array{Tile, 1}) :: Array{Int, 1}
    cs = fill(0, (4))
    for tile in tiles
        if candidate.id != tile.id
            for left in 1:4
                for right in 1:4
                    if candidate.sides[left] == tile.sides[right]
                        cs[left] += 1 
                    end
                    if candidate.sides[left] == reverse(tile.sides[right])
                        cs[left] += 1
                    end
                end
            end
        end
    end

    # SIDES:
    # TOP = 1
    # RIGHT = 2
    # BOTTOM = 3
    # LEFT = 4
    #############
    # CORNERS:
    # 0     1
    #
    # 3     2

    return cs
end

function is_corner(candidate :: Tile, tiles :: Array{Tile, 1}) :: Bool
    cs = side_matches(candidate, tiles)

    res = 0
    for c in cs
        if c == 0
            res += 1
        end
    end

    return res == 2
end

function part_1(tiles :: Array{Tile, 1}) :: Int
    result = 1
    for tile in tiles
        if is_corner(tile, tiles)
            result *= tile.id
        end
    end
    return result
end

function count_hash(lines :: Array{String, 1}) :: Int
    result = 0
    for line in lines
        for x in line
            if x == '#'
                result += 1
            end
        end
    end
    return result
end


monster = ["                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

function contains_monster(drow :: Int, dcol :: Int, picture :: Array{String, 1}) :: Bool
    for row0 in 0:(3 - 1)
        for col0 in 0:(20 - 1)
            if monster[row0 + 1][col0 + 1] == '#'
                if picture[row0 + drow][col0 + dcol] != '#'
                    return false
                end
            end
        end
    end
    return true
end

function count_monsters(picture :: Array{String, 1}) :: Int
    result = 0
    for drow in 1:(96 - (3))
        for dcol in 1:(96 - (20))
            if contains_monster(drow, dcol, picture)
                result += 1
            end
        end
    end
    return result
end

function part_2(tiles :: Array{Tile, 1}) :: Int
    puzzle = Array{Array{Tile, 1}, 1}()

    push!(puzzle, Array{Tile, 1}())
    for tile in tiles
        if side_matches(tile, tiles) == [0, 1, 1, 0]
            push!(puzzle[1], tile)
            break
        end
    end

    for i in 2:12
        right = find_right(puzzle[1][i - 1], tiles)
        push!(puzzle[1], right)
    end

    for i in 2:12
        push!(puzzle, Array{Tile, 1}())
        bottom = find_bottom(puzzle[i - 1][1], tiles)
        push!(puzzle[i], bottom)

        for j in 2:12
            right_bottom = find_right_bottom(puzzle[i][j - 1],
                                             puzzle[i - 1][j],
                                             tiles)
            push!(puzzle[i], right_bottom)
        end
    end

    picture = Array{String, 1}()

    for row in puzzle
        for i in 2:9
            picture_row = Array{String, 1}()
            for tile in row
                push!(picture_row, tile.rows[i][2:9])
            end
            push!(picture, join(picture_row))
        end
    end

    picture = flip_lines(picture)

    return count_hash(picture) - count_monsters(picture) * count_hash(monster)
end

function solve_file(file_path :: String)
    println("Input file: $(file_path)")
    tiles = parse_file(file_path)
    println("Part 1: $(part_1(tiles))")
    println("Part 2: $(part_2(tiles))")
end

for file_path in ARGS
    solve_file(file_path)
end
