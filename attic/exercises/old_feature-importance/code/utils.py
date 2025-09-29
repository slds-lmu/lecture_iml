import numpy as np
from scipy.stats import multivariate_normal
import torch
from torch.distributions import Normal, MultivariateNormal, Distribution
from numpy import linalg as la
import logging

logger = logging.getLogger(__name__)

def nearestPD(A):
    """Find the nearest positive-definite matrix to input
    A Python/Numpy port of John D'Errico's `nearestSPD` MATLAB code [1], which
    credits [2].
    [1] https://www.mathworks.com/matlabcentral/fileexchange/42885-nearestspd
    [2] N.J. Higham, "Computing a nearest symmetric positive semidefinite
    matrix" (1988): https://doi.org/10.1016/0024-3795(88)90223-6
    """

    B = (A + A.T) / 2
    _, s, V = la.svd(B)

    H = np.dot(V.T, np.dot(np.diag(s), V))

    A2 = (B + H) / 2

    A3 = (A2 + A2.T) / 2

    if isPD(A3):
        return A3

    spacing = np.spacing(la.norm(A))
    # The above is different from [1]. It appears that MATLAB's `chol` Cholesky
    # decomposition will accept matrixes with exactly 0-eigenvalue, whereas
    # Numpy's will not. So where [1] uses `eps(mineig)` (where `eps` is Matlab
    # for `np.spacing`), we use the above definition. CAVEAT: our `spacing`
    # will be much larger than [1]'s `eps(mineig)`, since `mineig` is usually on
    # the order of 1e-16, and `eps(1e-16)` is on the order of 1e-34, whereas
    # `spacing` will, for Gaussian random matrixes of small dimension, be on
    # othe order of 1e-16. In practice, both ways converge, as the unit test
    # below suggests.
    Imat = np.eye(A.shape[0])
    k = 1
    while not isPD(A3):
        mineig = np.min(np.real(la.eigvals(A3)))
        A3 += Imat * (-mineig * k**2 + spacing)
        k += 1

    return A3


def isPD(B):
    """Returns true when input is positive-definite, via Cholesky"""
    try:
        _ = la.cholesky(B)
        return True
    except la.LinAlgError:
        return False

class GaussianConditionalEstimator:
    """
    Conditional density estimation for joint normal distribution
    """
    default_hparam_grid = {}

    def __init__(self, **kwargs):
        pass
        
    def __check_target_1d(self):
        """Assess whether target distribution is univariate.
        Throws an RuntimeError if not.
        """
        if np.prod(self.Sigma.shape[0] != 1):
            raise RuntimeError('The target distribution is required '
                               'to be univariate. Dimensionality of '
                               'the target distribution: '
                               '{}'.format(self.Sigma.shape[0]))
        else:
            logger.debug('Passed: Target distribution dimensionality '
                         '= {}. Continue.'.format(self.Sigma.shape[0]))

    def __check_positive_variance(self, adjust=False):
        all_positive = np.prod(np.diag(self.Sigma) > 0) == 1
        if not all_positive and adjust:  # take absolute values on diagonal
            diag_ixs = np.diag_indices(self.Sigma.shape[0], ndim=2)
            diag_vals = np.abs(np.diag(self.Sigma)) + np.finfo(float).eps
            self.Sigma[diag_ixs] = diag_vals
        return all_positive

    def fit(self, train_inputs: np.array, train_context: np.array, **kwargs):
        """Fit Gaussian Sampler.
        Args:
            train_inputs: variables to be resampled
            train_context: conditioning set
        """
        # make sure arrays are 2d and concatenate into one array
        train_inputs = train_inputs.reshape((train_inputs.shape[0], -1))
        train_context = train_context.reshape((train_context.shape[0], -1))
        X_train = np.concatenate([train_inputs, train_context], axis=1)

        mean = np.mean(X_train, axis=0)
        cov = np.cov(X_train.T)

        n_in, n_co = train_inputs.shape[1], train_context.shape[1]
        cov = cov.reshape((n_in + n_co, n_in + n_co))  # make sure its 2d
        inp_ind = np.arange(0, n_in, 1)
        cont_ind = np.arange(n_in, n_in + n_co, 1)

        return self.fit_mean_cov(mean, cov, inp_ind, cont_ind)

    def fit_mean_cov(self, joint_mean, joint_cov, inp_ind, cont_ind):
        """Fit using mean vector and covariate matrix.
        Args:
            joint_mean: means for all variables
            joint_cov: cov for all variables
            inp_ind: indices of variables to be sampled
            cont_ind: "context" variable indexes (conditioning set)
        """
        self.inp_ind = inp_ind
        if cont_ind.shape[0] == 0:
            self.RegrCoeff = np.zeros((inp_ind.shape[0], 0))
        else:
            if cont_ind.shape[0] == 1:
                Sigma_GG_inv = 1 / joint_cov[np.ix_(cont_ind, cont_ind)]
            else:
                cov_context = joint_cov[np.ix_(cont_ind, cont_ind)]
                Sigma_GG_inv = np.linalg.pinv(cov_context)
            cov_ip_con = joint_cov[np.ix_(inp_ind, cont_ind)]
            tmp = cov_ip_con @ Sigma_GG_inv
            self.RegrCoeff = tmp.reshape((len(inp_ind), len(cont_ind)))

        cov_inp = joint_cov[np.ix_(inp_ind, inp_ind)]
        cov_cont_inp = joint_cov[np.ix_(cont_ind, inp_ind)]
        self.Sigma = cov_inp - self.RegrCoeff @ cov_cont_inp
        mean_inp, mean_cont = joint_mean[inp_ind], joint_mean[cont_ind]
        self.mu_part = mean_inp - self.RegrCoeff @ mean_cont
        if not isPD(self.Sigma):
            logger.info('Making Sigma positive definite')
            self.Sigma = nearestPD(self.Sigma)
        return self

    def conditional_distribution(self,
                                 context: np.array = None) -> Distribution:

        mu_part2 = self.RegrCoeff @ context.T
        mu = self.mu_part.reshape((-1, 1)) + mu_part2
        return MultivariateNormal(torch.tensor(mu).T,
                                  torch.tensor(self.Sigma))

    def sample(self, context: np.array = None, num_samples=1) -> np.array:
        m = self.conditional_distribution(context)
        smpl = m.sample((num_samples,)).numpy()
        res = np.swapaxes(smpl, 0, 1)
        return res