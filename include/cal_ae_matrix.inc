C Definition of MATRIX elements used for cal solution
C
       integer*2    mat(2,max_vis), tmat(0:max_aes-1,max_aes)
       common /local_cal_matrices/ mat, tmat

C flag to be set when matrices initialised
       logical      matrix_initialised
C flag to be set for amplitude solution
       logical      calculate_amp_solution
C reference antenna defined as an aerial index
       integer      refant
C index list from antenna numbers used (indexed) to actual antennas
       integer      ae_list(max_aes)
       common /local_cal_solution/ matrix_initialised,
     *                             calculate_amp_solution,
     *                             refant, ae_list
