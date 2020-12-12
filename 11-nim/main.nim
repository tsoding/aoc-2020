import os

type NborsCounter = proc(board: seq[string], row0, col0: int): int

proc countNborsV1(board: seq[string], row0, col0: int): int =
  let h = board.len
  let w = board[0].len
  for drow in -1..1:
    for dcol in -1..1:
      if drow != 0 or dcol != 0:
        let row = row0 + drow
        let col = col0 + dcol
        if (0 <= row and row < h) and (0 <= col and col < w):
          if board[row][col] == '#':
            result += 1

proc countNborsV2(board: seq[string], row0, col0: int): int =
  let h = board.len
  let w = board[0].len
  for drow in -1..1:
    for dcol in -1..1:
      if drow != 0 or dcol != 0:
        var row = row0 + drow
        var col = col0 + dcol
        while (0 <= row and row < h) and (0 <= col and col < w):
          case board[row][col]
          of '#':
            result += 1
            break
          of 'L':
            break
          else:
            row += drow
            col += dcol

proc nextRound(board: seq[string],
               tolerance: int,
               countNbors: NborsCounter): seq[string] =
  for row in 0..board.len - 1:
    result.add(board[row])
    for col in 0..board[row].len - 1:
      let nbors = countNbors(board, row, col)
      case board[row][col]:
        of 'L':
          if nbors == 0:
            result[row][col] = '#'
        of '#':
          if nbors >= tolerance:
            result[row][col] = 'L'
        else:
          discard

proc solveBoard(board: seq[string],
                tolerance: int,
                countNbors: NborsCounter): int =
  var input = board
  var input1 = nextRound(input, tolerance, countNbors)
  while (input1 != input):
    input = input1
    input1 = nextRound(input, tolerance, countNbors)

  for row in input:
    for seat in row:
      if seat == '#':
        result += 1

proc part1(board: seq[string]): int =
  result = solveBoard(board, 4, countNborsV1)

proc part2(board: seq[string]): int =
  result = solveBoard(board, 5, countNborsV2)

proc solveFile(filePath: string) =
  var board: seq[string] = @[]
  for line in filePath.lines:
    board.add(line)

  echo "Input file: ", filePath
  echo "Part 1: ", part1(board)
  echo "Part 2: ", part2(board)

proc main() =
  for i in 1..paramCount():
    solveFile(paramStr(i))

main()
