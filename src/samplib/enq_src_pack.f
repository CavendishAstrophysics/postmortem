

C+ENQ_SRC_PACK

      subroutine enq_src_pack ( lun, src_num, src_pack_record, s )
C
C     Returns source packing information for a given sample file.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun
C         The source number in the sample file
              integer         src_num

C     Returned:
C         Source packing record - see 'src_pack.inc'
              integer         src_pack_record(*)
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Possible return status's are:
C         ILL_CONTTAB - wrong version of control tables for this routine
C
C-

C     Global includes -
C

       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if ( s .ne. 0 ) return

C determine CT type
       call read_ct( lun, s )
       if ( s .ne. 0 ) goto 999

C call appropriate enquiry routine version
       if (ct_vers .le. 1) then
         call enq_v1_src_pack( lun, src_num, src_pack_record, s )
       elseif (ct_vers .eq. 2) then
         call enq_v2_src_pack( lun, src_num, src_pack_record, s )
       else
         s = ILL_CONTTAB
         goto 999
       endif

       return

 999   call smp_wrerr( s, 'in subroutine ENQ_SRC_PACK' )

       end
