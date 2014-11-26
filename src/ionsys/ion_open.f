C
C+ion_open
C
      SUBROUTINE ion_open( sf_name, lsf_key, s )

C     Asks the user to select the current correction.
C
C     Given:
C         Sample file name.
              character*(*)       sf_name

C     Returned:
C         LSF key of new correction
              integer             lsf_key
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C-
C     ****************************************************************
C
C     Function declarations
C
      logical         yesno_num
      include        '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include        '/mrao/include/iolib_errors.inc'
      include        '/mrao/post/include/ion_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         String length
              integer         ls
C         Sample file unit number.
              integer         sf_lun
C         Number of corrections in sample file and current one.
              integer         num_corr, curr_corr
C         Current correction name
              character*80    curr_name
C         Prompt
              character*80    prompt
C         Flag set if correction is selected.
              logical         found

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call open_sf( sf_lun, sf_name, 'READ', 0, s )
      call open_source( sf_lun, 1, 0, s )
      call enq_numcorr( sf_lun, num_corr, s )

      if (num_corr .gt. 0) then
         curr_corr = 1
         found = .false.
  100    continue
            call enq_ion_name( sf_lun, curr_corr, curr_name, s )
            write(prompt,'(A,I2,2A)')
     *            'Correction number ', curr_corr,'. on ', curr_name
            ls = chr_lenb( prompt )
            curr_corr = curr_corr + 1
            prompt(ls+2:ls+2) = '?'

            if (yesno_num( prompt, 'No', curr_corr, s )) then
                found = .true.
                curr_corr  = curr_corr-1
                ion_number = curr_corr
                call enq_ion_corr(  sf_lun, ion_number,
     *                                ion_type, ion_key, ion_lsf,
     *                                ion_first, ion_last,
     *                                ion_numsrcs,
     *                                ion_source, ion_ra, ion_dec,
     *                                ion_param, s  )
                if (ion_type .eq. 0) ion_lsf = -1
            end if

            if (curr_corr.lt.1 .or. curr_corr.gt.num_corr) then
              call io_wrout( ' ' )
              call io_wrout('No more corrections - please select one.')
              call io_wrout( ' ' )
              curr_corr = 1
            end if
        if (s.eq.0 .and. .not.found) goto 100
      else
        call io_wrout( 'No corrections have been saved for this file.' )
      end if

      if (s .eq. USR_BREAK) s = 0
      call close_source( sf_lun, 1, s )
      call close_sf( sf_lun, s )
      if ( s .ne. 0 ) goto 9999

      lsf_key = ion_lsf
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call ion_wrerr( s, ' in subroutine ion_open ' )
          return
      end
