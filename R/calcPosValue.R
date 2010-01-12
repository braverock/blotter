`calcPosValue` <-
function(PosQty, ClosePrice, ConMult=1) {
  PosValue <- PosQty * ClosePrice * ConMult
  return(PosValue)
}
