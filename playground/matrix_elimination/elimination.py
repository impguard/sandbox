import math


class Vector:
    def __init__(self, values):
        self.values = values

    def __getitem__(self, j):
        return self.values[j - 1]

    def __mul__(self, s):
        return Vector([s * v for v in self.values])

    def __rmul__(self, s):
        return self * s

    def __add__(self, other):
        if len(self.values) != len(other.values):
            raise ValueError("Bad Vector Sum")
        return Vector([v1 + v2 for v1, v2 in zip(self.values, other.values)])


class Matrix:
    def __init__(self, values):
        n = int(math.sqrt(len(values)))
        if n * n != len(values):
            raise ValueError("Matrix is not square")
        self.values = values
        self.n = n

    def __getitem__(self, i):
        start = (i - 1) * self.n
        end = i * self.n
        return Vector(self.values[start:end])

    def __setitem__(self, i, new_values):
        if len(new_values) != self.n:
            raise ValueError("Bad Matrix row set")
        start = (i - 1) * self.n
        end = i * self.n
        self.values[start:end] = new_values

    def __repr__(self):
        result = ""
        for i in range(1, self.n + 1):
            for j in range(1, self.n + 1):
                result += f"{round(self[i][j], 2)}\t"
            result += "\n"
        return result


def generate_identity(n):
    values = []
    for i in range(n):
        row = [0] * n
        row[i] = 1

        values.extend(row)

    return Matrix(values)


def elimination(matrix):
    multipliers1 = []
    ut_matrix = generate_identity(matrix.n)
    pivot_row = matrix[1]
    for i in range(1, matrix.n):
        pivot = pivot_row[i]
        scaling_multiplier = 1 / pivot
        summing_multiplier = -matrix[i + 1][i]

        next_row = (scaling_multiplier * summing_multiplier) * pivot_row + matrix[i + 1]

        ut_matrix[i] = (pivot_row * scaling_multiplier).values
        ut_matrix[i + 1] = next_row.values
        multipliers1.append((summing_multiplier, scaling_multiplier))

        pivot_row = next_row

    multipliers2 = []
    pivot_row = ut_matrix[ut_matrix.n]
    for i in range(ut_matrix.n, 1, -1):
        pivot = pivot_row[i]
        scaling_multiplier = 1 / pivot
        summing_multiplier = -ut_matrix[i - 1][i]

        next_row = (scaling_multiplier * summing_multiplier) * pivot_row + ut_matrix[i - 1]

        multipliers2.append((summing_multiplier, scaling_multiplier))

        pivot_row = next_row

    identity = generate_identity(matrix.n)
    for i, (summing_multiplier, scaling_multiplier) in enumerate(multipliers1):
        row = i + 1
        identity[row] = (scaling_multiplier * identity[row]).values
        identity[row + 1] = (identity[row + 1] + identity[row] * summing_multiplier).values

    for inv_i, (summing_multiplier, scaling_multiplier) in enumerate(multipliers2):
        row = identity.n - inv_i
        identity[row] = (scaling_multiplier * identity[row]).values
        identity[row - 1] = (identity[row - 1] + identity[row] * summing_multiplier).values

    return identity













