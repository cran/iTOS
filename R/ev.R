ev <- function (sc, z, m, g, method)
{
  bigN <- length(z)
  n <- sum(z)
  stopifnot((bigN > n) & (bigN > m) & (1 <= n) & (1 <= m))
  q <- sort(sc)
  u <- c(rep(0, bigN - m), rep(1, m))
  if (method == "AD") {
    if ((bigN > 200) & (n > 50) & ((bigN - n) > 50) & (m >
                                                       50) & ((bigN - m) > 50))
      method <- "LS"
    else method <- "BU"
  }
  if ((method == "LS") | (method == "BU")) {
    if (method == "LS") {
      quad <- function(ets) {
        (ets * ets * (g - 1)) + (g * n * m) - ets * ((g -
                                                        1) * (n + m) + bigN)
      }
      et <- stats::uniroot(quad, lower = max(0, m + n -
                                               bigN), upper = min(n, m))$root
      vt <- 1/((1/et) + (1/(m - et)) + (1/(n - et)) + (1/((bigN +
                                                             et) - (m + n))))
    }
    else {
      et <- BiasedUrn::meanFNCHypergeo(m, bigN - m, n,
                                       g)
      vt <- BiasedUrn::varFNCHypergeo(m, bigN - m, n, g)
    }
    qb1 <- sum(q[u == 1])/m
    qb0 <- sum(q[u == 0])/(bigN - m)
    if (m == 1)
      w1 <- 0
    else w1 <- stats::var(q[u == 1])
    if ((bigN - m) == 1)
      w0 <- 0
    else w0 <- stats::var(q[u == 0])
    expect <- et * qb1 + (n - et) * qb0
    term <- (w1 - w0) * et
    term <- term - (et * et + vt) * ((w1/m) + (w0/(bigN -
                                                     m)))
    term <- term + n * (bigN + 2 * et - (m + n)) * w0/(bigN -
                                                         m)
    vari <- term + vt * ((qb1 - qb0)^2)
  }
  else {
    o <- computep(bigN, n, m, g)
    expect <- sum(((o$p1 * u) + (o$p0 * (1 - u))) * q)
    cv <- (outer(u, u, "*") * (o$p11 - o$p1 * o$p1)) + (outer(u,
                                                              1 - u, "*") * (o$p10 - o$p1 * o$p0)) + (outer(1 -
                                                                                                              u, u, "*") * (o$p10 - o$p0 * o$p1)) + (outer(1 -
                                                                                                                                                             u, 1 - u, "*") * (o$p00 - o$p0 * o$p0))
    diag(cv) <- ((o$p1 - o$p1^2) * u) + ((o$p0 - o$p0^2) *
                                           (1 - u))
    vari <- 0
    for (i in 1:bigN) {
      for (j in 1:bigN) {
        vari <- vari + q[i] * cv[i, j] * q[j]
      }
    }
  }
  list(expect = expect, vari = vari)
}
