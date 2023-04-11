
*Updated: 2023-04-11.*

## Model

$$\left(x_i \left| \left\lbrace \mathbf{r}_i, \mathbf{p}_i\right\rbrace, u_i\right.\right)
\sim {\rm Poisson}\left(\Lambda_{i,4}\left|\left\lbrace\mathbf{r}_i,\mathbf{p}_i, u_i\right\rbrace\right.\right)$$

$$\left(y_i \left| \left\lbrace \mathbf{r}_i, \mathbf{p}_i\right\rbrace, u_i\right.\right)
\sim {\rm Poisson}\left(\Lambda_{i,3}\left|\left\lbrace\mathbf{r}_i,\mathbf{p}_i, u_i\right\rbrace\right.\right)$$

$$\left(w_i^{(\rm{obs})} \left| \left\lbrace \mathbf{r}_i, p_i^{(1)}, u_i\right\rbrace \right.\right)
\sim {\rm Poisson}\left(w_{i}\right)$$

where
$w_{i}= p_{i}^{(1)}\cdot \sum_{j=1}^{i-1} \left[ r_{j}^{(1)} \cdot \left(1 - u_{j}\right)\right] + r_i^{(2)}$
and
$w_{i}^{({\rm obs})} = p_{i}^{(1, {\rm obs})}\cdot\sum_{j=1}^{i-1} \left[ r_{j}^{(1,{\rm obs})} \cdot \left(1 - u_{j}\right)\right] + r_i^{(2, {\rm obs})}$

## Priors

$$\delta_i^{(1)}\sim N\left(\delta_{i-1}^{(1)}, \sigma_\delta^2\right),$$
$$\delta_i^{(2)}\sim N\left(\delta_{i-1}^{(2)}, \sigma_\delta^2\right),$$
and
$$\delta_i^{(3)}\sim N\left(\delta_{i-1}^{(3)}, \sigma_\delta^2\right)$$

where $\delta_0^{(1)}=\rm{logit}\left(0.015\right)$,
$\delta_0^{(2)}=\rm{logit}\left(0.01\right)$,
$\delta_0^{(1)}=\rm{logit}\left(0.0005\right)$, and
$\sigma_\delta=\lvert\sigma_\delta'\rvert$,
$\sigma_\delta'\sim\left(0, 1\right)$

$$\gamma_i^{(1)}\sim N\left(\gamma_{i-1}^{(1)}, \sigma_\gamma^2\right)$$
and
$$\gamma_i^{(2)}\sim N\left(\gamma_{i-1}^{(2)}, \sigma_\gamma^2\right)$$

where $\gamma_0^{(1)}=\log\left(0.1 \cdot m_0\right)$,
$\gamma_0^{(2)}=\log\left(0.00015 \cdot m_0\right)$, and
$\sigma_\gamma=\lvert\sigma_\gamma'\rvert$, $\sigma_\gamma'\sim (0, 1)$

$$\phi_1\sim N\left(0, 1\right)\text{ and }\phi_2\sim N\left(0, 1\right)$$

$u_i$ for $i=1,\ldots,5$ represent annual crude mortality rates among
the respective population and $m_0$ is the general population size at
time 0.

Assumptions:

- $r_0^{(1)}$ is approximately 10% of the population
- $r_0^{(2)}$ is approximately 0.15 per thousand population
- with a high level of uncertainty (weakly informative)

## Results

### Incidences

- Curves on the left margin indicate prior distributions where
  available.
- Connected solid diamonds indicate posterior medians with 80% and 95%
  credible intervals indicated with vertical lines.

|                    | r1                                                                                             | r2                                                                                             |
|:-------------------|:-----------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|
| Non-Hispanic black | <a href="./black/02-posterior-over-time-r1.png">![](./black/02-posterior-over-time-r1.png)</a> | <a href="./black/02-posterior-over-time-r2.png">![](./black/02-posterior-over-time-r2.png)</a> |
| Non-Hispanic white | <a href="./white/02-posterior-over-time-r1.png">![](./white/02-posterior-over-time-r1.png)</a> | <a href="./white/02-posterior-over-time-r2.png">![](./white/02-posterior-over-time-r2.png)</a> |

### Transition probabilities

- Curves on the left margin indicate prior distributions where
  available.
- Connected solid diamonds indicate posterior medians with 80% and 95%
  credible intervals indicated with vertical lines.
- White diamonds indicate the ratio estimates.

|                    | p1                                                                                             | p2                                                                                             | p3                                                                                             |
|:-------------------|:-----------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|
| Non-Hispanic black | <a href="./black/02-posterior-over-time-p1.png">![](./black/02-posterior-over-time-p1.png)</a> | <a href="./black/02-posterior-over-time-p2.png">![](./black/02-posterior-over-time-p2.png)</a> | <a href="./black/02-posterior-over-time-p3.png">![](./black/02-posterior-over-time-p3.png)</a> |
| Non-Hispanic white | <a href="./white/02-posterior-over-time-p1.png">![](./white/02-posterior-over-time-p1.png)</a> | <a href="./white/02-posterior-over-time-p2.png">![](./white/02-posterior-over-time-p2.png)</a> | <a href="./white/02-posterior-over-time-p3.png">![](./white/02-posterior-over-time-p3.png)</a> |

### Observed counts

- Connected solid diamonds indicate posterior medians with 80% and 95%
  credible intervals indicated with vertical lines.
- Crosses indicate the observed counts.

|                    | W                                                                                            | Y                                                                                            | X                                                                                            |
|:-------------------|:---------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------|
| Non-Hispanic black | <a href="./black/02-posterior-over-time-w.png">![](./black/02-posterior-over-time-w.png)</a> | <a href="./black/02-posterior-over-time-y.png">![](./black/02-posterior-over-time-y.png)</a> | <a href="./black/02-posterior-over-time-x.png">![](./black/02-posterior-over-time-x.png)</a> |
| Non-Hispanic white | <a href="./white/02-posterior-over-time-w.png">![](./white/02-posterior-over-time-w.png)</a> | <a href="./white/02-posterior-over-time-y.png">![](./white/02-posterior-over-time-y.png)</a> | <a href="./white/02-posterior-over-time-x.png">![](./white/02-posterior-over-time-x.png)</a> |

### Hyperparameters

### Initial prevalences

- The values represent logit-transformed proportion of the general
  subpopulation.

|                    | State 1                                                                                                          | State 2                                                                                                          |
|:-------------------|:-----------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------------------|
| Non-Hispanic black | <a href="./black/01-posterior-single-init_s1_logitp.png">![](./black/01-posterior-single-init_s1_logitp.png)</a> | <a href="./black/01-posterior-single-init_s2_logitp.png">![](./black/01-posterior-single-init_s2_logitp.png)</a> |
| Non-Hispanic white | <a href="./white/01-posterior-single-init_s1_logitp.png">![](./white/01-posterior-single-init_s1_logitp.png)</a> | <a href="./white/01-posterior-single-init_s2_logitp.png">![](./white/01-posterior-single-init_s2_logitp.png)</a> |

### Random walk standard deviations

|                    | $\sigma_\delta$                                                                                      | $\sigma_\gamma$                                                                                      |
|:-------------------|:-----------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------------|
| Non-Hispanic black | <a href="./black/01-posterior-single-sigdelta.png">![](./black/01-posterior-single-sigdelta.png)</a> | <a href="./black/01-posterior-single-siggamma.png">![](./black/01-posterior-single-siggamma.png)</a> |
| Non-Hispanic white | <a href="./white/01-posterior-single-sigdelta.png">![](./white/01-posterior-single-sigdelta.png)</a> | <a href="./white/01-posterior-single-siggamma.png">![](./white/01-posterior-single-siggamma.png)</a> |

### Bias adjustment terms

|                    | $\phi_1$                                                                                     | $\phi_2$                                                                                     |
|:-------------------|:---------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------|
| Non-Hispanic black | <a href="./black/01-posterior-single-phi1.png">![](./black/01-posterior-single-phi1.png)</a> | <a href="./black/01-posterior-single-phi2.png">![](./black/01-posterior-single-phi2.png)</a> |
| Non-Hispanic white | <a href="./white/01-posterior-single-phi1.png">![](./white/01-posterior-single-phi1.png)</a> | <a href="./white/01-posterior-single-phi2.png">![](./white/01-posterior-single-phi2.png)</a> |

## Diagnostic plots

Several diagnostic plots are available in subdirectories for [the
non-Hispanic black model](black/) and [the non-Hispanic white
model](white/).
