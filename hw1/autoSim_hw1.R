# autoSim.R

nVals = seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")
for (type in distTypes) {
  for (n in nVals) {
    oFile = paste( type, "_n", n, ".txt", sep="")
    arg = paste( 280, n, type, 50, sep=" ")
    sysCall = paste("nohup Rscript runSim_hw1.R", arg, " > ", oFile)
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}



