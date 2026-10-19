"""PDP vs. ALE for correlated features.

x1 ~ U(0,1), x2 = x1 + delta, delta ~ N(0, 0.02)   (0.02 used as std. dev.)
y  = x1 + eps
f(x) = x1 + 10 (x1 - x2)^2
"""
import numpy as np
import matplotlib.pyplot as plt

rng = np.random.default_rng(0)
n = 2000
x1 = rng.uniform(0, 1, n)
x2 = x1 + rng.normal(0, 0.02, n)
X = np.column_stack([x1, x2])


def f(X):
    return X[:, 0] + 10 * (X[:, 0] - X[:, 1]) ** 2


def pdp(X, j, grid):
    out = []
    for g in grid:
        Xg = X.copy()
        Xg[:, j] = g  # replace feature j for ALL instances -> breaks correlation
        out.append(f(Xg).mean())
    return np.array(out)


def ale(X, j, n_bins=20):
    edges = np.quantile(X[:, j], np.linspace(0, 1, n_bins + 1))
    idx = np.clip(np.searchsorted(edges, X[:, j], side="right") - 1, 0, n_bins - 1)
    effects = np.zeros(n_bins)
    counts = np.zeros(n_bins)
    for k in range(n_bins):
        m = idx == k
        lo, hi = X[m].copy(), X[m].copy()
        lo[:, j], hi[:, j] = edges[k], edges[k + 1]  # move only within local bin
        effects[k] = (f(hi) - f(lo)).mean()
        counts[k] = m.sum()
    acc = np.concatenate([[0], np.cumsum(effects)])
    # center: weighted mean of bin-midpoint values = 0
    mid = (acc[:-1] + acc[1:]) / 2
    acc -= (mid * counts).sum() / counts.sum()
    return edges, acc


fig, axes = plt.subplots(2, 2, figsize=(10, 8), sharey="row")
for j, name in enumerate(["x1", "x2"]):
    grid = np.linspace(X[:, j].min(), X[:, j].max(), 50)
    pd_vals = pdp(X, j, grid)
    edges, ale_vals = ale(X, j)

    ax = axes[0, j]
    ax.plot(grid, pd_vals - pd_vals.mean(), lw=2, label="PDP (centered)")
    ax.plot(edges, ale_vals, lw=2, label="ALE")
    truth = grid - 0.5 if j == 0 else np.zeros_like(grid)
    ax.plot(grid, truth, "k--", lw=1, label="true effect on y")
    ax.set_title(f"Feature effect of {name}")
    ax.set_xlabel(name)
    ax.legend()
    ax.plot(X[:, j], np.full(n, ax.get_ylim()[0]), "|", color="gray", alpha=0.1)

# Bottom row: data vs. where the PDP evaluates the model
ax = axes[1, 0]
xx, yy = np.meshgrid(np.linspace(-0.1, 1.1, 200), np.linspace(-0.1, 1.1, 200))
zz = f(np.column_stack([xx.ravel(), yy.ravel()])).reshape(xx.shape)
cs = ax.contourf(xx, yy, zz, levels=30, cmap="viridis")
fig.colorbar(cs, ax=ax, label="f(x)")
ax.scatter(x1, x2, s=2, c="white", alpha=0.5, label="training data")
ax.axvline(0.2, color="red", lw=2, label="PDP at x1=0.2 averages here")
ax.set_xlabel("x1"); ax.set_ylabel("x2")
ax.set_title("Model surface & data manifold")
ax.legend(loc="lower right", fontsize=8)

ax = axes[1, 1]
ax.axis("off")
ax.text(0, 0.95,
        "Analytical results (x2 approx. U(0,1)):\n\n"
        "PDP(x1) = x1 + 10 E[(x1 - X2)^2]\n"
        "        = 10 x1^2 - 9 x1 + 10/3   (U-shape)\n\n"
        "PDP(x2) = 10 (x2^2 - x2 + 1/3) + 1/2  (U-shape)\n\n"
        "ALE: df/dx1 = 1 + 20 (x1 - x2),  E[. | x1] = 1\n"
        "     -> ALE(x1) = x1 - 1/2   (linear)\n"
        "     df/dx2 = -20 (x1 - x2),  E[. | x2] = 0\n"
        "     -> ALE(x2) = 0         (flat)",
        va="top", family="monospace", fontsize=9)

fig.tight_layout()
fig.savefig("pdp_vs_ale.png", dpi=130)
print("saved pdp_vs_ale.png")
