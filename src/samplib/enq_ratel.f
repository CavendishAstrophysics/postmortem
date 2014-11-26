

C+ENQ_RATEL

       subroutine enq_ratel ( lun, RAtel, s )
C
C     Returns RAtel
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun

C     Returned:
C         RAtel
              real*8          RAtel
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Possible return status's are:
C         ILL_CONTTAB     - Error in control tables.
C
C-

C     Global includes -
C
      include '/mrao/include/constants.inc'
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers.eq.0 .or. ct_vers.eq.1) then
C         Normal sample file with one source
              RAtel  = ramc
      else if (ct_vers .eq. 2) then
C         New 5km sample file
          call enq_v2_RAtel( RAtel, s )
      else
          s = ILL_CONTTAB
      end if

      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_RATEL' )

      end
