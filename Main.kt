package processor

import java.lang.Exception
import java.text.DecimalFormat
import java.util.*
import kotlin.math.pow
import kotlin.math.round

class Matrix(val n: Int, val m: Int) {
    protected var data = Array(n) { DoubleArray(m) }
    private var currentRow = 0

    fun addRowData(rowData: DoubleArray, row: Int = -1) {
        var rowInd = currentRow
        if (row > -1)
            rowInd = row
        data[rowInd] = rowData.clone()
        currentRow = rowInd + 1
    }

    fun addRowData(rowString: String, row: Int = -1): Boolean {
        try {
            val nums = rowString.trim().split(" ").map { it.toDouble() }.toDoubleArray()
            addRowData(nums, row)
            return currentRow < n
        } catch (e: NumberFormatException) {
            return false
        }
    }

    fun getColumn(col: Int): DoubleArray {
        val column = DoubleArray(n)
        for (r in 0 until n) {
            column[r] = data[r][col]
        }
        return column
    }

    fun rowColumnRemoved(row: Int, col: Int): Matrix {
        val newMatrix = Matrix(n - 1, m - 1)
        val remainingRows = data.toMutableList().filterIndexed() { index,_ -> index != row - 1 }
        val remainingData = remainingRows.map { it.toMutableList().filterIndexed() { index,_ -> index != col - 1 } }
        for (r in remainingData) {
            newMatrix.addRowData(r.toDoubleArray())
        }
        return newMatrix
    }

    fun determinant(): Double {
        if (n == 2 && m == 2) {
            return data[0][0] * data[1][1] - data[1][0] * data[0][1]
        }

        var total = 0.0
        for (col in 0 until m) {
            total += (-1.0).pow(col + 2.0) * data[0][col] * rowColumnRemoved(1, col + 1).determinant()
        }
        return total
    }

    fun inverse(): Matrix {
        val det = determinant()
        if (det == 0.0)
            throw ArithmeticException("Inverse matrix doesn't exist!")

        if (n == 2 && m == 2) {
            val nm = Matrix(2, 2)
            nm.data[0][0] = data[1][1]
            nm.data[0][1] = -1 * data[0][1]
            nm.data[1][0] = -1 * data[1][0]
            nm.data[1][1] = data[0][0]
            return nm
        }

        val newMatrix = Matrix(n, m)
        for (r in 0 until n) {
            for (c in 0 until m) {
                val cof = (-1.0).pow(r + c + 2.0) * rowColumnRemoved(r + 1, c + 1).determinant()
                newMatrix.data[r][c] = cof
            }
        }
        return newMatrix.transposed("main") * (1 / det)
    }

    private fun getRow(row: Int) = data[row]

    fun transposed(type: String = "main"): Matrix {
        var newMatrix: Matrix
        when (type) {
            "main" -> {
                newMatrix = Matrix(m, n)
                for (c in 0 until m) {
                    newMatrix.addRowData(getColumn(c))
                }
            }
            "side" -> {
                newMatrix = Matrix(m, n)
                for (c in m-1 downTo 0) {
                    newMatrix.addRowData(getColumn(c).reversed().toDoubleArray())
                }
            }
            "vertical" -> {
                newMatrix = Matrix(n, m)
                for (r in 0 until n) {
                    newMatrix.addRowData(getRow(r).reversed().toDoubleArray())
                }
            }
            "horizontal" -> {
                newMatrix = Matrix(n, m)
                for (r in n-1 downTo 0) {
                    newMatrix.addRowData(getRow(r))
                }
            }
            else -> newMatrix = this
        }
        return newMatrix
    }

    operator fun plus(other: Matrix): Matrix {
        if (n != other.n || m != other.m)
            throw ArithmeticException("Dimensions are not the same!")
        val newMatrix = Matrix(n, m)
        for (r in 0 until n) {
            for (c in 0 until m) {
                newMatrix.data[r][c] = data[r][c] + other.data[r][c]
            }
        }
        return newMatrix
    }

    operator fun times(other: Double): Matrix {
        val newMatrix = Matrix(n, m)
        for (r in 0 until n) {
            for (c in 0 until m) {
                newMatrix.data[r][c] = data[r][c] * other
            }
        }
        return newMatrix
    }

    operator fun times(other: Matrix): Matrix {
        val newArr = Array(n) { DoubleArray(other.m) }
        val newMatrix = Matrix(n, other.m)
        newMatrix.data = newArr
        if (m != other.n)
            throw ArithmeticException("Row and column count do not match!")
        for (r in 0 until n) {
            val row = data[r]
            for (c in 0 until other.m) {
                val col = other.getColumn(c)
                var sum = 0.0
                for ((v1, v2) in row zip col) {
                    sum += v1 * v2
                }
                newArr[r][c] = sum
            }
        }
        return newMatrix
    }

    override fun toString(): String {
        var out = ""
        for (r in 0 until n) {
            var fields = data[r].joinToString(" ") {
                val rounded = if (it == round(it)) it.toInt().toString() else DecimalFormat("#.##").format(it)
                "%7s".format(rounded)
            }
            out += "$fields\n"
        }
        return out
    }
}

fun timesConstant(scanner: Scanner, m1: Matrix) {
    println("Enter constant: ")
    val cons = scanner.nextDouble()
    println(m1 * cons)
}

fun getMatrices(scanner: Scanner, matrices: MutableList<Matrix>, singular: Boolean = false) {
    var howMany = "first"
    if (matrices.size > 0)
        howMany = "second"
    if (singular)
        howMany = ""

    println("Enter size of $howMany matrix: ")
    val (n, m) = IntArray(2) { scanner.nextInt() }
    println("Enter $howMany matrix: ")
    val matrix = Matrix(n, m)
    do {
        val line = readLine()!!
    } while (matrix.addRowData(line))
    matrices.add(matrix)
}

fun transposes(scanner: Scanner, matrices: MutableList<Matrix>) {
    println("1. Main diagonal")
    println("2. Side diagonal")
    println("3. Vertical line")
    println("4. Horizontal line")
    println("Your choice: ")
    val choice = scanner.nextInt()
    getMatrices(scanner, matrices, singular = true)
    val newMatrix = when (choice) {
        1 -> matrices[0].transposed("main")
        2 -> matrices[0].transposed("side")
        3 -> matrices[0].transposed("vertical")
        4 -> matrices[0].transposed("horizontal")
        else -> matrices[0]
    }
    println("The result is:")
    println(newMatrix)
}

fun determined(scanner: Scanner, matrices: MutableList<Matrix>) {
    getMatrices(scanner, matrices, singular = true)
    println("The result is:")
    println(matrices[0].determinant())
}

fun main() {
    val scanner = Scanner(System.`in`)
    val matrices = mutableListOf<Matrix>()
    loop@ while (true) {
        println("1. Add matrices")
        println("2. Multiply matrix to a constant")
        println("3. Multiply matrices")
        println("4. Transpose matrix")
        println("5. Calculate a determinant")
        println("6. Inverse matrix")
        println("0. Exit")
        println("Your choice: ")
        matrices.clear()
        try {
            when (scanner.nextInt()) {
                1 -> {
                    getMatrices(scanner, matrices)
                    getMatrices(scanner, matrices)
                    println("The addition result is:")
                    println(matrices[0] + matrices[1])
                }
                2 -> {
                    getMatrices(scanner, matrices, singular = true)
                    timesConstant(scanner, matrices[0])
                }
                3 -> {
                    getMatrices(scanner, matrices)
                    getMatrices(scanner, matrices)
                    println("The multiplication result is:")
                    println(matrices[0] * matrices[1])
                }
                4 -> transposes(scanner, matrices)
                5 -> determined(scanner, matrices)
                6 -> {
                    getMatrices(scanner, matrices, singular = true)
                    println("The result is:")
                    println(matrices[0].inverse())
                }
                0 -> break@loop
                else -> println("Huh?")
            }
        } catch (e: Exception) {
            println("ERROR")
        }
    }
}
