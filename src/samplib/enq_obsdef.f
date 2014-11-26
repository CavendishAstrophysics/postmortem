

C+ ENQ_OBSDEF

       subroutine enq_obsdef ( lun, nae_o, nsp_o, nba_o, nch_o, s )
C
C returns the definition of the current observation
C
C Input
C   LUN           -     I4     -   logical unit number
C
C Returned
C   NAE_O         -     I4     -   actual number of aerial
C   NSP_O         -     I4     -   actual number of spacings
C   NBA_O         -     I4     -   actual number of sub-bands
C   NCH_O         -     I4     -   actual number of channel
C   S             -     I4     -   error return
C
C The description of the telescope is returned as the number of
C aerials, spacings, sub-bands and channels which are used in the
C current observation.
C
C
C [PA, 1/11/88]
*-

       integer     s
       integer     lun, nae_o, nsp_o, nba_o, nch_o

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_obsdef( nae_o, nsp_o, nba_o, nch_o, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_obsdef( nae_o, nsp_o, nba_o, nch_o, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_OBSDEF' )

       end
