"""
Create a combined side-by-side plot comparing PFI for Linear Regression and Gradient Boosting
on test data.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os

from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import GradientBoostingRegressor

# Plotting defaults
plt.rcParams['figure.figsize'] = (12, 6)
plt.rcParams['axes.spines.top'] = False
plt.rcParams['axes.spines.right'] = False
plt.rcParams['axes.grid'] = False

RANDOM_SEED_GLOBAL = 7
np.random.seed(RANDOM_SEED_GLOBAL)


def permutation_feature_importance(model, X, y, n_perm=40, random_state=0):
    """Compute permutation feature importance."""
    rng = np.random.RandomState(random_state)
    base_pred = model.predict(X)
    base_mse = mean_squared_error(y, base_pred)
    means, stds, cols = [], [], list(X.columns)
    for col in cols:
        deltas = []
        for _ in range(n_perm):
            Xp = X.copy()
            Xp[col] = rng.permutation(Xp[col].values)
            perm_pred = model.predict(Xp)
            deltas.append(mean_squared_error(y, perm_pred) - base_mse)
        means.append(float(np.mean(deltas)))
        stds.append(float(np.std(deltas)))
    df = pd.DataFrame({
        'feature': cols,
        'mean_delta_mse': means,
        'std': stds
    }).sort_values('mean_delta_mse', ascending=False)
    return base_mse, df


def main():
    # 1) Generate data (simplified DGP)
    rng = np.random.RandomState(RANDOM_SEED_GLOBAL)
    n = 1200

    x1 = rng.normal(0, 1, n)
    x2 = x1 + rng.normal(0, 0.25, n)  # correlated with x1 (redundant)
    x3 = rng.normal(0, 1, n)
    x4 = rng.normal(0, 1, n)
    x5 = rng.normal(0, 1, n)  # noise
    x6 = rng.normal(0, 1, n)  # noise

    signal = (
        3.0 * x1  # strong linear
        + 2.0 * np.sin(2.0 * x4)  # smoother sinusoidal (no interaction)
        + 1.5 * (x3 ** 2)  # quadratic nonlinearity
    )

    y = signal + rng.normal(0, 0.7, n)

    X = pd.DataFrame({'x1': x1, 'x2': x2, 'x3': x3, 'x4': x4, 'x5': x5, 'x6': x6})
    y = pd.Series(y, name='y')

    # Train/test split
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.35, random_state=11
    )

    # Fit Linear Regression
    lin = LinearRegression().fit(X_train, y_train)
    lin_te_mse, lin_te_pfi = permutation_feature_importance(
        lin, X_test, y_test, n_perm=40, random_state=2
    )

    # Fit Gradient Boosting
    gbr = GradientBoostingRegressor(
        n_estimators=500,
        max_depth=5,
        learning_rate=0.1,
        subsample=0.9,
        random_state=11
    )
    gbr.fit(X_train, y_train)
    gbr_te_mse, gbr_te_pfi = permutation_feature_importance(
        gbr, X_test, y_test, n_perm=40, random_state=4
    )

    # Ensure both dataframes have the same feature order
    feature_order = ['x1', 'x2', 'x3', 'x4', 'x5', 'x6']
    lin_te_pfi = lin_te_pfi.set_index('feature').reindex(feature_order).reset_index()
    gbr_te_pfi = gbr_te_pfi.set_index('feature').reindex(feature_order).reset_index()

    # Create side-by-side horizontal bar plot
    y_pos = np.arange(len(feature_order))
    height = 0.35

    # Adjusted figure size for horizontal bars
    fig, ax = plt.subplots(figsize=(5, 2.5))

    # Plot horizontal bars
    bars1 = ax.barh(
        y_pos - height / 2,
        lin_te_pfi['mean_delta_mse'],
        height,
        xerr=lin_te_pfi['std'],
        label='Lin. Reg.',
        alpha=0.8
    )
    bars2 = ax.barh(
        y_pos + height / 2,
        gbr_te_pfi['mean_delta_mse'],
        height,
        xerr=gbr_te_pfi['std'],
        label='Boosting',
        alpha=0.8
    )

    # Add vertical line at PFI = 0
    ax.axvline(0, color='gray', linestyle='--', linewidth=0.8, zorder=0)

    # Labels and formatting with increased font sizes for readability
    ax.set_xlabel('Δ MSE after permutation', fontsize=14)
    ax.set_ylabel('Feature', fontsize=14)
    ax.set_title('PFI: Lin. Reg. vs. Boosting (Test)', fontsize=16)
    ax.set_yticks(y_pos)
    ax.set_yticklabels(feature_order, fontsize=12)
    ax.tick_params(axis='x', labelsize=12)
    ax.legend(fontsize=12)

    plt.tight_layout()

    # Save figure with higher resolution for readable text
    output_dir = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'fig')
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, 'pfi_combined_test.png')
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"Figure saved to: {output_path}")

    # Print MSEs for reference
    print(f"\nLinear Regression - Test MSE: {lin_te_mse:.4f}")
    print(f"Gradient Boosting - Test MSE: {gbr_te_mse:.4f}")


if __name__ == '__main__':
    main()

