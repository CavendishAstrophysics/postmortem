
C     *****************************************************************
C
C+lsf_sel_ion_corr
C
      SUBROUTINE lsf_sel_ion_corr ( lsf_num,
     *                              s                      )

C
C     Asks the user to select the lsf ionospheric correction.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     At present simly asks for an ionospheric corection number
C     and sets it. No checking is done.
C
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         New ionospheric correction number
              integer         new_corr
C         Number of corrections in file
              integer         num_corr
C         General purpose string
              character*80    string
C         Parameters returned from enq_ion_corr enquiry routine.
              integer         ion_type, ion_key, ion_lsf, ion_numsrcs
              integer         ion_first, ion_last, ion_param(8)
              character*16    ion_source(10)
              real*8          ion_ra(10), ion_dec(10)

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if

      if (sf_type .ne. 1) then
C         Not a physical sample file.
          call io_wrout('Sample file of unexpected type.')
C          return
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      call enq_numcorr( sf_lun, num_corr, s )
      new_corr = lsf_ion_num

      call io_geti( 'Which correction : ', '*', new_corr, s )
      if ( s .ne. 0 ) goto 9999

      if (new_corr.lt.0 .or. new_corr.gt.num_corr ) then
          call io_wrout( 'That correction does not exist.' )
      else if (new_corr .eq. 0 .and. smooth_type .ge. 2) then
          call io_wrout(
     *          'You cannot switch off the correction with the'//
     *                ' current type of smooth averaging.' )
      else if (new_corr .eq. 0) then
          call io_wrout( 'Ionospheric correction switched off.' )
          lsf_ion_num = 0
          lsf_ion_key = 0
          ion_flag    = 0
          lsf_key     = 1
          lsf_name    = ' '
      else
          call enq_ion_corr(  sf_lun, new_corr,
     *                        ion_type, ion_key, ion_lsf,
     *                        ion_first, ion_last,
     *                        ion_numsrcs,
     *                        ion_source, ion_ra, ion_dec,
     *                        ion_param, s  )

          if (ion_type.ne.0 .and. ion_key .eq. 0) then
              call io_wrout( 'That correction has not yet been made.' )
          else if (s .eq. 0) then
              lsf_ion_num = new_corr
              ion_flag    = 0
              lsf_ion_key = ion_key
              lsf_key     = 1
              lsf_name    = ' '
              string = 'Correction is on '
              call enq_ion_name( sf_lun, new_corr, string(18:), s )
              call io_wrout( string )
          end if
      end if
      if (s .ne. 0) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if ( s .ne. USR_BREAK) then
              call lsf_wrerr( s, 'in subroutine LSF_SEL_ION_CORR' )
          end if
          return
      end
