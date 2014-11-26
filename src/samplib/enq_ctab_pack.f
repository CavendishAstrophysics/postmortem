

C+ENQ_CTAB_PACK

      subroutine enq_ctab_pack ( lun, ctab_pack_record, s )
C
C     Returns control table packing information for a given sample file.
C
C     Given:
C         The logical unit number of the sample file.
              integer         lun

C     Returned:
C         Control table packing record - see 'ctab_pack.inc'
              integer         ctab_pack_record(*)
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Possible return status's are:
C         ILL_CONTTAB - wrong version of control tables for this routine
C
C-

C     Global includes -
C
      include  '/mrao/post/include/control_tables.inc'
      include  '/mrao/post/include/samplib_errors.inc'

      integer   pointer
      integer   i

      if (s.ne.0) return

      call read_ct ( lun, s )
      if (s.ne.0) goto 999

C     Check that this is not a version zero sample file

      if ( ct_vers .eq. 1 ) then
         pointer = ctv1_pack_ptr
      elseif ( ct_vers .eq. 2 ) then
         pointer = ctv2_pack_ptr
      else
         s = ILL_CONTTAB
         goto 999
      end if

      do i = 1, ct_all( pointer+1 )
         ctab_pack_record( i ) = ct_all( pointer+i )
      enddo

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_CTAB_PACK' )

      end
