from elimination import Matrix, Vector, generate_identity, elimination


def test_matrix_get():
    m = Matrix([1, 2, 3, 4])
    assert m[1][1] == 1
    assert m[2][2] == 4


def test_matrix_set():
    m = Matrix([1, 2, 3, 4])
    m[1] = [0, 1]
    assert m[1][1] == 0
    assert m[1][2] == 1


def test_vector_sum():
    v1 = Vector([1, 2])
    v2 = Vector([3, 4])
    v3 = v1 + v2
    assert v3.values == [4, 6]


def test_vector_mul():
    v = Vector([1, 2])
    v2 = 3 * v
    assert v2.values == [3, 6]


def test_generate_identity():
    m = generate_identity(10)

    for i in range(1, 11):
        for j in range(1, 11):
            if i == j:
                assert m[i][j] == 1
            else:
                assert m[i][j] == 0


def test_elimination():
    m = Matrix([4, 7, 2, 6])
    m_inv = elimination(m)
    assert [round(v, 1) for v in m_inv.values] == [0.6, -0.7, -0.2, 0.4]


if __name__ == "__main__":
    test_matrix_get()
    test_matrix_set()
    test_vector_sum()
    test_vector_mul()
    test_elimination()
    test_generate_identity()
