C
C+monitor_system
C
      SUBROUTINE monitor_system( def_sf, def_lsf, s )

C
C     The monitor system within postmortem.
C
C     Given:
C         Current sample file name
              character*(*)       def_sf
C         Current logical sample file key
              integer             def_lsf

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Opens sample file to find telescope name, calls appropriate
C     monitor sub-system.
C
C-
C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Constants
          include '/mrao/include/iolib_errors.inc'
          include '/mrao/post/include/samplib_errors.inc'

C     Exit command code
          integer             exit_cmd
          parameter         ( exit_cmd = 0 )
C
C     Variables, equivalences and commons
C         Telescope name and code
              character*6     tscope_name
              integer         tscope_code
C         Physical sample file logical unit
              integer         sf_lun
C         Sample file name
              character*64    old_sf
C         Current command
              integer         command
C         General purpose string and length
              character*80    string
              integer         ls

C     ****************************************************************
C
C     Subroutine initialisation
C
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C

      if ( def_sf .ne. ' ' ) then
          call io_wrout( ' ' )
          call io_wrout( def_sf )
          call io_wrout( ' ' )
      end if
  100 continue

      call open_sf( sf_lun, def_sf, 'READ', 0, s )
      call enq_tscope( sf_lun, tscope_name, tscope_code, s )
      call close_sf( sf_lun, s )
      old_sf = def_sf

      if (tscope_name .eq. 'T151' .or. tscope_name .eq. '38MHZ') then
          call mon_sys_clfst( def_sf, def_lsf, command, s )
      else if (tscope_name .eq. 'RYLE') then
          call mon_sys_ryle( def_sf, def_lsf, command, s )
      else if ( s .eq. 0 ) then
          s = ILL_TSCOPE
      end if

C     Restore status.
      if ( s .ne. 0 ) goto 9999
      if (command .ne. exit_cmd) goto 100

      call io_enqcli( string, ls )
      if (ls.eq.0) call io_wrout( ' ' )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call io_wrerr( s, 'in subroutine MONITOR_SYSTEM' )
          return
      end
