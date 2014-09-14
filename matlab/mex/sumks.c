#include <mex.h>
#include <matrix.h>
#include <math.h>

/* A Matlab Executable (MEX) test.

Some information:

(#) ~/src/matlab/mex % file sumks.mexa64 
sumks.mexa64: ELF 64-bit LSB  shared object, x86-64, version 1 (SYSV), 
dynamically linked, BuildID[sha1]=927a6ec46d667de3ab79fe0e92e4367ba5e6b5b5, not stripped

(#) ldd sumks.mexa64 
    linux-vdso.so.1 =>  (0x00007fff3fbfe000)
    libmx.so => not found
    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f2a2562f000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f2a25268000)
    /lib64/ld-linux-x86-64.so.2 (0x00007f2a25a69000)

(#) ~/src/matlab/mex % nm sumks.mexa64 
0000000000201068 b __bss_start
0000000000201068 b completed.6972
                 w __cxa_finalize@@GLIBC_2.2.5
00000000000007a0 t deregister_tm_clones
0000000000000810 t __do_global_dtors_aux
0000000000200db8 t __do_global_dtors_aux_fini_array_entry
0000000000201060 d __dso_handle
0000000000200dc8 d _DYNAMIC
0000000000201068 d _edata
0000000000201070 b _end
0000000000000984 t _fini
0000000000000850 t frame_dummy
0000000000200db0 t __frame_dummy_init_array_entry
0000000000000a98 r __FRAME_END__
0000000000201000 d _GLOBAL_OFFSET_TABLE_
                 w __gmon_start__
00000000000006d8 t _init
                 w _ITM_deregisterTMCloneTable
                 w _ITM_registerTMCloneTable
0000000000200dc0 d __JCR_END__
0000000000200dc0 d __JCR_LIST__
                 w _Jv_RegisterClasses
0000000000000000 A MEX
                 U mexErrMsgIdAndTxt
0000000000000885 T mexFunction
                 U mxCreateDoubleMatrix_700
                 U mxGetNumberOfElements
                 U mxGetPr
                 U mxGetScalar
                 U mxIsComplex
                 U mxIsNumeric
00000000000007d0 t register_tm_clones
0000000000201068 d __TMC_END__

Matlab arrays (type mxArray) has type struct:
  
Matlab functions are a 4-arity function named mexFunction which returns void.
It's rguments are:

 #   Type              Description
 --------------------------------------------------------------------------
 1   int               Number of outputs; i.e. the size of second argument
 2   *mxArray[]        Putput arguments
 3   int               Number of inputs; i.e. the size of fourth argument
 4   const *mxArray[]  Input arguments
 ---------------------------------------------------------------------------
*/


/* nlhs: number of left hand side
 * plhs: pointer to left hand side
 * nrhs: number of right hand side
 * plhs: pointer to right hand side
 */
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
    if (nrhs != 1) {
        mexErrMsgIdAndTxt("MATLAB:sumks:invalidNumInputs",
                          "Need (just) one argument!");
    }

    if (!mxIsNumeric(prhs[0]) ||
         mxGetNumberOfElements(prhs[0]) != 1 ||
         mxIsComplex(prhs[0])) {

        mexErrMsgIdAndTxt("MATLAB:sumks:invalidArg",
                "Need a single non-complex scalar.");

    }

    double Nd = mxGetScalar(prhs[0]);
    double sum = 0;
    int n = 1;
    for (; n <= Nd; n++) {
        sum += 1/pow(n, 2);
    }

    plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
    *(mxGetPr(plhs[0])) = sum;
}
