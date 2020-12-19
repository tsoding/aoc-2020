import java.io.File

enum class TokenType {
    OpenParen,
    CloseParen,
    Plus,
    Mult,
    Number
}

data class Token(val type: TokenType, val text: String)

fun String.tokenize(): List<Token> {
    @OptIn(kotlin.ExperimentalStdlibApi::class)
    return buildList {
        var input = this@tokenize.trimStart()
        while (input.isNotEmpty()) {
            val token: Token = when (input[0]) {
                '(' -> Token(TokenType.OpenParen, "(")
                ')' -> Token(TokenType.CloseParen, ")")
                '+' -> Token(TokenType.Plus, "+")
                '*' -> Token(TokenType.Mult, "*")
                else -> {
                    Token(TokenType.Number, input.takeWhile(Char::isDigit))
                }
            }
            add(token)
            input = input.drop(token.text.length).trimStart()
        }
    }
}

object Part1 {
    fun evalSeq(tokens: List<Token>): Pair<Long, List<Token>> {
        var result = evalExpr(tokens)
        var acc = result.first.toLong()
        var input = result.second

        while (input.isNotEmpty() && input.first().type != TokenType.CloseParen) {
            result = evalExpr(input.drop(1));
            when (input.first().type) {
                TokenType.Plus -> {
                    acc += result.first.toLong()
                }

                TokenType.Mult -> {
                    acc *= result.first.toLong()
                }
            }

            input = result.second;
        }

        return Pair(acc, input)
    }

    fun evalExpr(tokens: List<Token>): Pair<Long, List<Token>> {
        return when (tokens.first().type) {
            TokenType.Number -> {
                Pair(tokens.first().text.toLong(), tokens.drop(1))
            }

            TokenType.OpenParen -> {
                val result = evalSeq(tokens.drop(1))

                if (result.second.first().type != TokenType.CloseParen) {
                    throw Exception("Unexpected token ${result.second.first().text}")
                }

                Pair(result.first, result.second.drop(1))
            }

            else -> {
                throw Exception("Unexpected token ${tokens.first().text}. Expected Number or OpenParen")
            }
        }
    }

    fun evalLine(line: String): Long {
        return evalSeq(line.tokenize()).first
    }

    fun evalFile(filePath: String): Long {
        var acc = 0L
        File(filePath).forEachLine {
            acc += evalLine(it)
        }
        return acc
    }
}

object Part2 {
    fun evalPrimary(tokens: List<Token>): Pair<Long, List<Token>> {
        return when (tokens.first().type) {
            TokenType.Number -> {
                Pair(tokens.first().text.toLong(), tokens.drop(1))
            }

            TokenType.OpenParen -> {
                val result = evalMult(tokens.drop(1))

                if (result.second.first().type != TokenType.CloseParen) {
                    throw Exception("Unexpected token ${result.second.first().text}. Expected ClosedParen. ${result.second.map({ it.text })}")
                }

                Pair(result.first, result.second.drop(1))
            }

            else -> {
                throw Exception("Unexpected token ${tokens.first().text}. Expected Number or OpenParen")
            }
        }
    }

    fun evalPlus(tokens: List<Token>): Pair<Long, List<Token>> {
        var result = evalPrimary(tokens)
        var acc = result.first
        var input = result.second

        while (input.isNotEmpty() && input.first().type == TokenType.Plus) {
            result = evalPrimary(input.drop(1))

            acc += result.first
            input = result.second
        }

        return Pair(acc, input)
    }

    fun evalMult(tokens: List<Token>): Pair<Long, List<Token>> {
        var result = evalPlus(tokens)
        var acc = result.first
        var input = result.second

        while (input.isNotEmpty() && input.first().type == TokenType.Mult) {
            result = evalPlus(input.drop(1))

            acc *= result.first
            input = result.second
        }

        return Pair(acc, input)
    }

    fun evalLine(line: String): Long {
        return evalMult(line.tokenize()).first
    }

    fun evalFile(filePath: String): Long {
        var acc = 0L
        File(filePath).forEachLine {
            val result = evalLine(it)
            acc += result
        }
        return acc
    }
}

fun solveFile(filePath: String) {
    println("Input file: $filePath")
    println("Part 1: ${Part1.evalFile(filePath)}")
    println("Part 2: ${Part2.evalFile(filePath)}")
}

fun main(args: Array<String>) =
    args.forEach(::solveFile)
