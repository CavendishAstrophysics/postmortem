

C+ ENQ_GT_REC

       subroutine enq_gt_rec( lun, record, s )
C
C      Enquire the gains table record from the control tables
C
C      Given:
C         sample file logical unit number
                 integer      lun
C      Returned:
C         gains table record information
                 integer*2    record(2)
C         error status
                 integer      s
C
C Information required for the gain table record is found from the control
C tables of the sample file.  At present this routine is only applicable
C to the Ryle telescope and is a null operation for the CLFST.
C
C PA 20/04/90
C-
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call io_wrout('** No gains table information available' )
      elseif (ct_vers .eq. 2) then
         call enq_v2_gt_rec ( record, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_GT_REC' )

      end
