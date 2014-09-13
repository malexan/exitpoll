conf <- function(scs, trls, N, conf) {
  scs <- scs / trls
  M <- sqrt(((scs * (1 - scs)) / trls) * (1 - trls/N))
  Z <- qnorm(1 - conf / 2)
  M * Z
}
  