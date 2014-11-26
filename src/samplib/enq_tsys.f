
C+ ENQ_TSYS

       subroutine enq_tsys( lun, temps, nominal, s )
C
C      Enquire the telescope system temperatures from the control tables
C
C      Given:
C         sample file logical unit number
                 integer      lun
C      Returned:
C         eight nominal telescope system temperatures
                 real*4       temps( 1 )
C         nominal value of rain gauge reading (usually 8 V)
                 real*4       nominal
C         error status
                 integer      s
C
C Information required for the system temps is found from the control
C tables of the sample file.  This routine is only applicable
C to the Ryle telescope and returns all 1's for the CLFST.
C
C MEJ 08/09/92
C-
      include  '/mrao/post/include/control_tables.inc'
      include  '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_tsys( temps, nominal, s )
      elseif (ct_vers .eq. 2) then
         call enq_v2_tsys( temps, nominal, s )
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return
 999  call smp_wrerr( s, 'in subroutine ENQ_TSYS' )
      return

      end
