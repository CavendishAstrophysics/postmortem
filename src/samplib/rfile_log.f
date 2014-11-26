
C+RFILE_LOG

      subroutine rfile_log ( lun, first_samp, curr_samp, last_samp )
C
C     Routine to log every sample file read.
C
C     Given:
C         Sample file unit number.
              integer     lun
C         First, last and current sample in the file buffer.
              integer     first_samp, curr_samp, last_samp
C-

      character*(70)  file_name

      include '/mrao/include/chrlib_functions.inc'

      character*(16)  user
      integer         log_lun, s, termno, mode
      logical         log_on, log_set, io_yesno
      common          / log_rfile / log_lun, log_on, log_set
      data            log_set / .false. /

      if ( .not. log_set ) then
          log_set = .true.
          call io_enqexe( user, mode, termno )

          s = 0
c         if ( chr_chsame( user, 'postmortem') .and. (mode .eq. 0) .and.
c    *         ((termno.eq.48) .or. (termno.eq.50)))                then
             if ( io_yesno( 'Set file access log on ?', 'No' , s )) then
                  call io_opefil( log_lun,
     *                         '/mrao/post/sampfile_access.log',
     *                         'WRITE', 0, s )
                  log_on = ( s.eq.0 )
              else
                  log_on = .false.
              end if
c         else
c             log_on = .false.
c         end if
      end if

	write(*,*)log_on,log_lun
      if ( log_on ) then
          inquire( unit=lun, name=file_name )
          write( log_lun, 10 ) lun, file_name,
     *                          first_samp, last_samp, curr_samp
  10      format( 'READ  Unit=', I2, ' Name=', A, /
     *            'First, last sample in buffer', 2(I5, ','),
     *            ' Current sample: ', I4                     )
      end if

      end
