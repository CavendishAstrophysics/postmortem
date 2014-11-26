C
C+lsf_sel_removes
C
      SUBROUTINE lsf_sel_removes( lsf_num,
     *                            s           )

C
C     Asks the user to select the removes to use in the lsf.
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     At present simply asks for a list of remove sources and opens the
C     remove file.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/remove_record.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         Full remove file name
              character*80    file_name
C         Number of removes in remove file
              integer         num_srcs
C         Source name for the remove
              character*20    source

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

      if ( rf_lun .eq. 0 ) then
C         Remove file is closed, open it.
          call enq_namfil( sf_lun, 'REM', file_name, s )
          if ( s .eq. NO_FILE ) then
              s = 0
              call io_wrout( 'No removes exist for this sample file.' )
              return
          end if
          call open_sf( rf_lun, file_name, 'READ', 0, s )
          if ( s .ne. 0 ) goto 9999
      else
C         Close all sources currently open.
          call close_remcal( s )

          do 10, i = 1, num_removes
              call close_source( rf_lun, lsf_rem_num(i), s )
              if ( s .ne. 0 ) goto 9999
   10     continue
      end if

C     ****************************************************************
C
C         Main Code
C         ---------
C
      lsf_key     = 1
      lsf_name    = ' '
      num_removes = 0

      call enq_numsrc( rf_lun, num_srcs, s )

      do 100, i = 1, num_srcs
          call enq_src_name( rf_lun, i, source, s )
          if ( io_yesno( 'Remove '//source//' ?', 'No' , s )) then
              if ( (num_removes .ge. max_rems   ) .or.
     *             (num_removes .ge. max_rt_rems)      ) then
                  s = ILL_BUFFSIZE
                  goto 9999
              else
                  num_removes = num_removes + 1
                  lsf_rem_num(num_removes) = i
              end if
          end if
  100 continue

      if (s .eq. USR_BREAK) s = 0

      do 200, i = 1, num_removes
          call enq_src_def( rf_lun, lsf_rem_num(i), remove_record, s )
          lsf_rem_key(i)  = rem_key
          lsf_rem_type(i) = rem_type
C  [buffer size restricted to 8 pages, DJT, 27/11/89]
          call open_source( rf_lun, lsf_rem_num(i), 8, s )
  200 continue

      if ( num_removes .eq. 0 ) then
          call close_sf( rf_lun, s )
          rf_lun = 0
      else
C         Set up the remove calibration infomation
          call open_remcal(s)
      end if

      if ( s .ne. 0 ) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SEL_REMOVES' )
          return
      end
