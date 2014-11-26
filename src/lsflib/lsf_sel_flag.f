C
C+lsf_sel_flag
C
      SUBROUTINE lsf_sel_flag( lsf_num,
     *                         s                         )

C
C     Asks the user to select flagging for the lsf
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
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
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/cal_record.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Flag-file file name
              character*64    ff_name
C         Flag-file unit number - id
              integer         flag_id
C         List of version to specify and character list
              character*80    clist
              integer         max_list
              parameter      (max_list=32)
              integer         nlist, list(max_list)

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

C Find name of flag file
      call enq_namfil( sf_lun, 'FLAG', ff_name, s )

C Open flag file
      call flag_open( ff_name, flag_id, s )

C Find list from user
      call io_getlst('Flag-file version number(s) to use : ', '0',
     *            clist, list, max_list, nlist, s )

C Find key and record for this list
      call flag_get_key( flag_id, list, nlist,
     *                   flag_key, flag_record, s )
      flag_flag = 1
      if (s.ne.0) flag_flag = 0

C force calculation of flag array
      call flag_docalc( -1, .true. )

C close flag file
      call flag_close( flag_id, s )
      if (s.ne.0) goto 9999

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_SEL_FLAG' )
          return
      end
