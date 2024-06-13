import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FormatStrFormatter

sns.set(context="poster")
sns.set_style("whitegrid", {"font.family":"sans-serif", "font.sans-serif":"Verdana"})

FIXED_POINT = 13
TAU_M = 20.0

def fixed_mul(a, b):
    assert a.dtype == np.int16
    assert b.dtype == np.int16
    return ((a.astype(np.int32) * b) >> FIXED_POINT).astype(np.int16)

# Current ramp
i = np.linspace(0.0, 0.1, 32)

alpha = np.exp(-1.0 / TAU_M)

v = [np.zeros(32)]
for t in range(100):
    v_new = (v[-1] * alpha) + i
    v_new[v_new > 1.0] = 0.0
    v.append(v_new)

scale = 2 ** FIXED_POINT
i_fixed = np.rint(scale * i).astype(np.int16)
alpha_fixed = np.rint(scale * alpha).astype(np.int16)
print(i_fixed)
print(alpha_fixed)
v_fixed = [np.zeros(32, dtype=np.int16)]
for t in range(100):
    v_new = fixed_mul(v_fixed[-1], alpha_fixed) + i_fixed
    v_new[v_new > scale] = 0
    v_fixed.append(v_new)

v = np.asarray(v)
v_fixed = np.asarray(v_fixed)

#fig, axes = plt.subplots(32, sharex=True)
fig, axis = plt.subplots()
for a in range(32):
    act = axis.plot((a * 2) + v[:,a])[0]
    axis.plot((a*2) + (v_fixed[:,a] / scale), linestyle="--", color=act.get_color())

sns.despine(ax=axis)
axis.xaxis.grid(False)
axis.set_xlabel("Time [ms]")
axis.set_ylabel("Input current")
axis.set_yticks(np.arange(0, 64, 2))
axis.set_yticklabels(["%.3f" % f for f in i])
plt.show()
