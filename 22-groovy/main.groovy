class Decks implements Cloneable {
    List<Integer> player1 = []
    List<Integer> player2 = []

    Decks(player1, player2) {
        this.player1 = player1
        this.player2 = player2
    }

    @Override
    String toString() {
        return "{ ${player1}, ${player2} }"
    }

    @Override
    Decks clone() throws CloneNotSupportedException {
        return new Decks(player1.clone(), player2.clone());
    }

    @Override
    boolean equals(Object o) {
        if (o instanceof Decks) {
            def that = o as Decks;

            return this.player1.equals(that.player1) &&
                this.player2.equals(that.player2);
        }

        return false;
    }

    @Override
    int hashCode() {
        int result = player1.hashCode();
        result = 31 * result + player2.hashCode();
        return result;
    }
}

def parseFile(filePath) {
    def decks = new Decks([], [])
    def lines = new File(filePath).readLines()
    def index = 1

    while (lines[index].length() > 0) {
        decks.player1.add(lines[index].toInteger())
        index += 1
    }

    index += 2

    while (index < lines.size()) {
        decks.player2.add(lines[index].toInteger())
        index += 1
    }

    return decks
}

def recWinnerCondition(decks, visited) {
    if (visited.contains(decks)) {
        return 1
    } else if (decks.player1.size() == 0 && decks.player2.size() == 0) {
        assert false : "Unreachable. Both players somehow ended up with no cards"
    } else if (decks.player1.size() > 0 && decks.player2.size() == 0) {
        return 1;
    } else if (decks.player2.size() > 0 && decks.player1.size() == 0) {
        return 2;
    } else {
        return 0;
    }
}

def simulateGameRec(decks, game) {
    def winner = 0;
    def round = 1;
    def visited = new HashSet<Decks>()
    while ((winner = recWinnerCondition(decks, visited)) == 0) {
        // println "DEBUG: Round ${round} (Game ${game})"
        // println "DEBUG: ${decks}"

        visited.add(decks.clone())
        def a1 = decks.player1.pop()
        def a2 = decks.player2.pop()

        if (decks.player1.size() >= a1 && decks.player2.size() >= a2) {
            def subWinner = simulateGameRec(
                new Decks(decks.player1.take(a1), decks.player2.take(a2)),
                game + 1)

            // println "DEBUG: Winner: $subWinner"

            switch (subWinner) {
                case 1: 
                    decks.player1.add(a1)
                    decks.player1.add(a2)
                    break;
                case 2:
                    decks.player2.add(a2)
                    decks.player2.add(a1)
                    break;
                default:
                    assert false : "Unexpected sub game results: ${subWinner}"
            }
        } else if (a1 > a2) {
            decks.player1.add(a1)
            decks.player1.add(a2)
            // println "DEBUG: Winner: 1"
        } else if (a1 < a2) {
            decks.player2.add(a2)
            decks.player2.add(a1)
            // println "DEBUG: Winner: 2"
        } else {
            assert false : "Unreachable state of the game. Players drew equal cards"
        }

        round += 1;
    }

    return winner;
}

def simulateGame(decks) {
    while (decks.player1.size() != 0 && decks.player2.size() != 0) {
        def a1 = decks.player1.pop()
        def a2 = decks.player2.pop()
        
        if (a1 > a2) {
            decks.player1.add(a1)
            decks.player1.add(a2)
        } else if (a1 < a2) {
            decks.player2.add(a2)
            decks.player2.add(a1)
        } else {
            assert false : "unreachable state of the game"
        }
    }
}

def winnerScore(decks) {
    def deck = decks.player1.size() > 0 ?
        decks.player1 :
        decks.player2
    def n = deck.size()
    def result = 0;

    for (int i = 1; i <= n; ++i) {
        result += i * deck.removeLast()
    }
    return result;
}

def part1(decks) {
    simulateGame(decks)
    return winnerScore(decks)
}

def part2(decks) {
    simulateGameRec(decks, 1);
    return winnerScore(decks);
}

def solveFile(filePath) {
    println "Input file: ${filePath}"
    def decks = parseFile(filePath)
    println "Part 1: ${part1(decks.clone())}"
    println "Part 2: ${part2(decks.clone())}"
}

for (filePath in args) {
    solveFile(filePath)
}
