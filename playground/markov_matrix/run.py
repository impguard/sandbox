""" Run a markov problem solver.

Usage:
    run N
"""

from matplotlib import pyplot as plt
import numpy as np
from docopt import docopt

arguments = docopt(__doc__)

A = np.matrix([[0.8, 0.3], [0.2, 0.7]])
u0 = np.matrix([1, 0]).T
v0 = np.matrix([0, 1]).T

un = [u0]
vn = [v0]
for i in range(int(arguments["N"])):
    un.append(A * un[-1])
    vn.append(A * vn[-1])

plt.scatter([u[0,0] for u in un], [u[1,0] for u in un], label="u")
plt.scatter([v[0,0] for v in vn], [v[1,0] for v in vn], label="v")
plt.legend()
plt.savefig("graph.png")
