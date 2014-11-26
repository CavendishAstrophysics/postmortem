*+ flag_docalc

       subroutine flag_docalc( flag_lsf, flag_calculation )
C      ----------------------------------------------------
C
C Set the calculation of the flagging array
C
C Given:
C   LSF key for the flag-table array
       integer     flag_lsf
C   key to specify calculation of the flag table
       logical     flag_calculation
C
C Calculation of the flagging array is triggeed if:
C         flag_calculation = TRUE
C   AND
C         flag_lsf =|= the lsf of the flagging array already constructed
C-
       include '/mrao/post/include/flag_definition.inc'

       flg_calculation = ( flag_calculation .and.
     *                     (flag_lsf.ne.flg_lsf) ) .or. flg_calculation
       if (flag_calculation) flg_lsf = flag_lsf

       end
