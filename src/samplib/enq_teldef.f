

C+ ENQ_TELDEF

       subroutine enq_teldef ( lun, nae_max, nsp_max, nba_max, nch_max,
     :                                                               s )
C
C returns the definition of the telescope
C
C Input
C   LUN           -     I4     -   logical unit number
C
C Returned
C   NAE_MAX       -     I4     -   max. number of aerial
C   NSP_MAX       -     I4     -   max. number of spacings
C   NBA_MAX       -     I4     -   max. number of sub-bands
C   NCH_MAX       -     I4     -   max. number of channel
C   S             -     I4     -   error return code
C
C The description of the telescope is returned as the maximum number of
C aerials, spacings, sub-bands and channels which the hardware can allow
C for any observation. The actual numbers of each of these can be found
C using the enq_obsdef routine.
C
C [PA, 1/11/88]
*-

       integer     s
       integer     lun, nae_max, nsp_max, nba_max, nch_max

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_teldef( nae_max, nsp_max, nba_max, nch_max, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_teldef( nae_max, nsp_max, nba_max, nch_max, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_TELDEF' )

       end
