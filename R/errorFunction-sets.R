
errorAFunc <- function(params, abc){
  integrateA = sapply(abc$TimeGrid,function(xt)(integrate(function(tt)pulseModel(params,tt), lower=xt-abc$tL, upper=xt,abs.tol=1e-100)$value))
  errorL = sum((integrateA-abc$l)^2)
  error0 = (pulseModel(params,(params[4]-params[5]/2))-params[1])^2
  error1 = (pulseModel(params,0)-params[1])^2
  return(errorL + error0 + error1)
}

errorBCFunc <- function(params, abc){
  pa = abc$pre_a
  pbc = matrix(c(abc$pre_a[1],params))
  res = sapply(abc$TimeGrid,function(xt){
    (integrate(function(tt)pulseModel(pa,tt), lower=0, upper=xt,abs.tol=-Inf)$value) -
      (integrate(function(tt)pulseModel(pbc,tt), lower=0, upper=xt,abs.tol=-Inf)$value)
  })
  errorBC = sum((res+abc$y[1]-abc$y)^2)
  error0 = (pulseModel(pbc,(pbc[4]-pbc[5]/2))-pbc[1])^2
  error1 = (pulseModel(pbc,0)-pbc[1])^2
  return(errorBC + error0 + error1)
}

errorBFunc <- function(params, abc){
  pa = abc$pre_a
  pbc = matrix(c(abc$pre_a[1],params[1],abc$pre_a[3], params[2:4]))

  res = sapply(abc$TimeGrid,function(xt){
    (integrate(function(tt)pulseModel(pa,tt), lower=0, upper=xt,abs.tol=1e-100)$value) -
      (integrate(function(tt)pulseModel(pbc,tt), lower=0, upper=xt,abs.tol=1e-100)$value)
  })
  errorBC = sum((res+abc$y[1]-abc$y)^2)
  error0 = (pulseModel(pbc,(pbc[4]-pbc[5]/2))-pbc[1])^2
  error1 = (pulseModel(pbc,0)-pbc[1])^2
  return(errorBC + error0 + error1)
}


abcFun <- function(gene,labexon, totexon, totintr, TimeGrid, tL, loopnumber=100){
  TL = as.matrix(labexon[gene,])
  TT = as.matrix(totexon[gene,])
  PT = as.matrix(totintr[gene,])
  res_A = t(sapply(1:loopnumber,function(x)splitA6Params(TL, tL, TimeGrid)))

  bestres_a = res_A[which(res_A[,7]==min(res_A[,7]))[1], 1:6]

  res_C = t(sapply(1:loopnumber,function(x)splitC5Params(PT, bestres_a, TimeGrid)))
  bestres_c = res_C[which(res_C[,6]==min(res_C[,6]))[1], 1:5]

  res_B = t(sapply(1:loopnumber,function(x)splitB4Params(TT, bestres_a, TimeGrid)))
  bestres_b = res_B[which(res_B[,5]==min(res_B[,5]))[1], 1:4]

  return(list(para = bestres_a, parb=bestres_b, parc=bestres_c))
}
