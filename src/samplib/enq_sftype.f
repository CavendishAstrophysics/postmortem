
C+ENQ_SFTYPE

       subroutine enq_sftype ( lun, sf_type, s )
C
C     Returns a code indicating the sample file type.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C
C     Returned:
C         The sample file type (1=phys sf, 2=remove, 3=calibration)
              integer         sf_type
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     The information is obtained from the control tables so the
C     source does not have to be open.
C
C     Possible return status's are:
C         ILL_CONTTAB     - Error in control tables.
C         NO_SRCNUM       - No such source. (Error not logged)
C
C-

C     Global includes -
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/ctab_pack.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .eq. 0) then
          if ( abs(ismtyp) .eq. 1 ) then
C             Normal sample file with one source
              sf_type = 1
          else if ( abs(ismtyp) .eq. 2 ) then
C             Old calibration file
              s = ILL_CONTTAB
              goto 999
          else if ( abs(ismtyp) .eq. 3 ) then
C             Old remove file
              sf_type = 2
          else
              s = ILL_CONTTAB
          end if
      else if (ct_vers .ge. 1) then
          call enq_ctab_pack( lun, ctab_pack, s )
          sf_type = ct_type
      else
          s = ILL_CONTTAB
      end if

      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_SFTYPE' )

      end
