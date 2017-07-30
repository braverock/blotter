#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Function definitions */
SEXP calcPosAvgCost (SEXP PrevPosQty, SEXP PrevPosAvgCost, SEXP TxnValue, SEXP PosQty, SEXP ConMult);

static const
R_CallMethodDef callMethods[] = {
  {"calcPosAvgCost", (DL_FUNC) &calcPosAvgCost, 5},
  {NULL, NULL, 0}
};

void R_init_blotter(DllInfo *info)
{
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);  /* C code not callable outside blotter */
}
