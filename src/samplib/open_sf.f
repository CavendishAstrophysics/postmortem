
C+OPEN_SF

      subroutine open_sf ( lun, file_name, access, rep_flg, s )
C
C     Opens a sample file.
C
C     Given:
C         Input sample file name.
              character*(*)   file_name
C         Access string.
              character*(*)   access
C         Report flag, see below.
              integer         rep_flg

C     Returned:
C         Logical unit number.
              integer         lun
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     General purpose routine for opening a sample file. The file must
C     exist and have a valid set of control tables. If these conditions
C     are not satisfied use the relevant a combination of io_crefil,
C     io_opefil and WRITE_CT.
C
C     Valid access values are:
C         'READ'  - File opened access READ
C         'WRITE' - File opened access WRITE
C
C     If the job is interactive then various levels of interaction are
C     provided depending on the following allowed values of rep_flg:
C         0 - the routine prompts for retry if the open fails.
C         1 - the full opened file name is printed on the output device.
C         2 - the routine prompts for retry if the open fails.
C     Rep_flg values of 0 and 2 are identical, and are the recommended
C     values since then an interactive job must return with either a
C     status of usr_break or success.
C
C     A non-interactive job will do one prompt if the input file-name is
C     blank and then will exit after logging an error if the open fails.
C
C     The routine enq_sfname is called to establish a valid sample
C     file name so see the documentation for that routine to see what
C     are the defaults taken to do this.
C
C     On exit name contains the full sample file name.
C
C     Possible return status values are:
C         - invalid access code (ILL_ACCESS)
C         - invalid report flag code (ILL_REPFLG)
C         - invalid control tables (ILL_CONTTAB)
C         - ambiguous file name (AMB_FILE)
C         - no such file (NO_FILE)
C         - unexpected io_system file system error code.
C
C     (NPR, 17 September 1987)
C     (DJT, 29 June 1992, Unix implementation)
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include '/mrao/include/iolib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C
C     Variables, equivalences and commons
C         General purpose integer
              integer         i
C         Flag to indicate whether routine should retry on failure.
              logical         retry
C         Full sample file name and a temporary file name.
              character*80    sf_name
              character*80    name
C         Telescope name and code
              character*16    tscope_name
              integer         tscope_code
C         Block size for direct access I/O (bytes)
              integer         blocksize

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( rep_flg .lt. 0 .or. rep_flg .gt. 2 ) then
          s = ILL_REPFLG
          goto 999
      end if

      call io_nxtlun( lun, s )
      if ( s .ne. 0 ) goto 999

      call io_enqbch( retry )
      retry = ( .not.retry .and. (rep_flg.eq.0 .or. rep_flg.eq.2))

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if (file_name.eq.' ') then
          name = ' '
          call io_getc( 'Sample file name : ', '', name, s )
      else
          name = file_name
      end if

      call enq_sfname( name, sf_name, retry, s )
      if ((s.ne.0 .and..not.retry) .or.
     *    (s.eq.USR_BREAK)         .or.
     *    (s.eq.TOO_MANYSF)               ) goto 999

  10  continue
  20      if (s .eq. 0) goto 30
              call smp_wrerr( s, ' ' )
              s = 0

              call io_getc( 'Sample file name : ', '*', name, s )
              call enq_sfname( name, sf_name, retry, s )
              if (s .eq. USR_BREAK) goto 999
          goto 20
  30      continue

C         sf_name now contains an unambiguous file name.  Open the
C         file for random access I/O with a block size of 1 page.

          blocksize = (page*4)
          if (chr_chsame(access,'READ')) then
C             Open file for read only.
              call io_operan( lun, sf_name, 'READ', blocksize,
     *                                                   rep_flg, s )
          else if (chr_chsame(access,'WRITE')) then
C             Open file for write only.
              call io_operan( lun, sf_name, 'WRITE', blocksize,
     *                                                   rep_flg, s )
          else
              s = ILL_ACCESS
              goto 999
          end if

          if (s .eq. 0) then
              call read_ct( lun, s )
              call enq_tscope( lun, tscope_name, tscope_code, s )
              i = 0
              if (s .ne. 0) call close_sf( lun, i )
          end if
      if (s .ne. 0 .and. retry) goto 10

      if (s .ne. 0) goto 999

      file_name = sf_name
      if ( rep_flg .eq. 1 ) call io_wrout( file_name )

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 999  if (s .ne. USR_BREAK) then
          call smp_wrerr( s, 'in subroutine OPEN_SF' )
      end if

      end
