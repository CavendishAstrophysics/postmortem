C+flagging
C
      SUBROUTINE flagging  ( lsf_num,
     *                       sf_lun,
     *                       samp_num,
     *                       num_vis,
     *                       vis_list,
     *                       vis,
     *                       s                      )

C
C     Applies flagging to a visibility buffer
C
C     Given:
C         LSF number
              integer*4       lsf_num
C         SF logical unit number
              integer*4       sf_lun
C         sample number
              integer*4       samp_num
C         number of visibilities
              integer*4       num_vis
C         list of visibilities
              integer*4       vis_list(num_vis)
C         Visibility buffer
              real*4          vis( 2, num_vis )
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Applies flagging to the visibility buffer depending on
C     contents of the flag table.  If required the flag table
C     array will be constructed by this routine before data are
C     flagged.
C-
C
C Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/flag_definition.inc'

C
C Variables, equivalences and commons
C         Loop counters
              integer         i1, n
C         Number of visibilities in 1-sample in SF
              integer         nv
C         Function to test bit
              logical         util_tstbit
C
C         Bit array
              integer         len_array
              parameter      (len_array = max_vis*max_samp/32+1 )
              integer*4       array( len_array )
              common /save_flag_array/ array

C Check for non zero entry status
      if ( s .ne. 0 ) return

C check that flagging is enabled
      if (flag_flag.eq.0) return

C construct flag table array if required
      if (flg_calculation) then
        call flag_calculate( lsf_num, sf_lun, src_num,
     *                       flag_key, flag_record, array, s )
        if (s.ne.0) goto 9999
      end if

C do flagging
      call enq_numvis( sf_lun, nv, s )
      i1 = (samp_num-1)*nv
      do n=1,num_vis
        if (util_tstbit(array,(i1+vis_list(n)))) then
          vis(1,n) = 0.0
          vis(2,n) = 0.0
        end if
      end do

      if (s .ne. 0) goto 9999
      return

C Error Handling

 9999 continue
          call lsf_wrerr( s, 'in subroutine FLAGGING' )
          return
      end
