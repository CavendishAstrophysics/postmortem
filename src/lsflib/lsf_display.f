C
C+lsf_display
C
      SUBROUTINE lsf_display ( lsf_num,
     *                         s                      )

C
C     Displays a description of the LSF on the output device.
C
C     Given:
C         Logical sample file number
              integer             lsf_num

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C
C-
C     ****************************************************************
C
C     Function declarations

      include  '/mrao/include/chrlib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/constants.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local variables, equivalences and commons
C         Loop counter
              integer         i
C         String and length
              character*512   string
              integer         ls, ls1, ls2
C         Logical unit number of output device
              integer         lun
C         File names
              character*(80)  file_name
C         Number of spacings in physical sample file
              integer         num_sp
C         Source name
              character*16    src_name
C         List of flagging versions used
              integer         flist(32), nflist, nf, flag_id
              character*64    ff_name

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

      call io_enqout( lun )

C     ****************************************************************
C
C         Main Code
C         ---------
C
      write( lun, '(A,A60)')  ' Sample file    : ', sf_name

      if (sf_type .eq. 1) then
C         Physical sample file - write out associated files.
          call enq_namfil( sf_lun, 'REM', file_name, s )
          if (s.eq.0) then
              write( lun, '(A,A60)') ' Remove file    : ', file_name
          else
              if (s.eq. NO_FILE) s = 0
          end if

          call enq_namfil( sf_lun, 'CAL', file_name, s )
          if (s.eq.0) then
              write( lun, '(A,A60)')   ' CAL file       : ', file_name
          else
              if (s.eq. NO_FILE) s = 0
          end if

          call enq_namfil( sf_lun, 'LSF', file_name, s )
          if (s.eq.0) then
              write( lun, '(A,A60)') ' LSF definitions: ', file_name
          else
              if (s.eq. NO_FILE) s = 0
          end if
      end if
      write( lun, '(2A)') ' LSF-Name       : ', lsf_name
      call util_extdat( lsf_time_created, 1, string, ls )
      write( lun, '(4A)') ' Created by     : ',
     *       lsf_owner,' ',string(1:ls)
C      call util_extdat( lsf_last_used, 1, string, ls )
C      write( lun, '(4A)') ' Last used by   : ',lsf_last_user,
C     *                     ' ', string(1:ls)
      if ( s .ne. 0 ) goto 9999

      write( lun, '(1H )')
      string = ' Phase centre   : '//source_text
      ls = chr_lenb( string ) + 3
      string(ls:ls) = '-'
      call chr_chdtos( ref_ra/const_h2r,  3, string(ls:),       ls1 )
      call chr_chdtos( ref_dec/const_d2r, 3, string(ls+ls1+3:), ls1 )
      ls = chr_lenb(string) + 3
      write( lun, '(2A, F7.2, A)' ) string(1:ls), '(', ref_date, ')'

      call enq_numvis( sf_lun, num_sp, s )
      call bits_to_string( sp_bit_arr, num_sp, .true., string, s )
      ls  = chr_lenb( string )
      ls1 = 1
      ls2 = chr_ilstc( string(1:ls1+65), ',' )
      if (ls.le.ls1+62) ls2 = ls
      write( lun, '(A,A)') ' Spacings       : ', string(ls1:ls2)
      do while (ls2.lt.ls)
          ls1 = ls2+1
          ls2 = chr_ilstc( string(1:ls1+62), ',' )
          if (ls.le.ls1+62) ls2 = ls
          write( lun, '(17X,A)') string(ls1:ls2)
      end do

      call bits_to_string( samp_list, num_samp, .false., string, s )
      ls  = chr_lenb( string )
      ls1 = 1
      ls2 = chr_ilstc( string(1:ls1+65), ',' )
      if (ls.le.ls1+65) ls2 = ls
      write( lun, '(A,A)') ' Samples        : ', string(ls1:ls2)
      do while (ls2.lt.ls)
          ls1 = ls2+1
          ls2 = chr_ilstc( string(1:ls1+65), ',' )
          if (ls.le.ls1+65) ls2 = ls
          write( lun, '(12X,A)') string(ls1:ls2)
      end do
      if ( s .ne. 0 ) goto 9999

      write( lun, '(1H )')
      if ( lsf_ion_key .eq. 0 ) then
          write( lun, '(A)' ) ' Ion-correction : NONE'
      else
          call enq_ion_name( sf_lun, lsf_ion_num, src_name, s )
          ls = chr_lenb(src_name)
          write( lun, '(A,I2,2A)' )
     *        ' Ion-correction : No. ', lsf_ion_num,
     *        ', on ', src_name(1:ls)
      end if

      if ( num_removes .eq. 0 ) then
          write( lun, '(A)') ' Removes        : NONE'
      else
          write( lun, '(A)') ' Removes        : Source(s) removed'
          do 200, i = 1, num_removes
              write(string, '(I2,'' on '',506X)') lsf_rem_num(i)
              call enq_src_name( rf_lun, lsf_rem_num(i), string(7:21),s)
              if (lsf_rem_ion(i) .ne. 0) then
                  call enq_ion_name( sf_lun, lsf_rem_ion(i),src_name,s)
                  ls = chr_lenb(src_name)
                  write(string(22:), '(A,I2,3A)')
     *            ' - ion ', lsf_rem_ion(i), ' (',src_name(1:ls),')'
                  ls = chr_lenb(string)+1
              else
                  write(string(22:), '(A)') ' - no ION '
                  ls = chr_lenb(string)+1
              end if
              if (lsf_rem_cal(i) .ne. 0) then
                  call enq_src_name( cf_lun, lsf_rem_cal(i),src_name,s)
                  ls1= chr_lenb(src_name)
                  write(string(ls:), '(A,I2,3A)')
     *            ', cal ', lsf_rem_cal(i), ' (',src_name(1:ls1),').'
                  ls = chr_lenb(string)
              else
                  write(string(ls:), '(A)') ', - no CAL '
                  ls = chr_lenb(string)
              end if
              write(lun,'(X,A)') string(1:ls)
  200     continue
      end if
      if ( s .ne. 0 ) goto 9999

      if ( lsf_cal_key .eq. 0 ) then
          write( lun, '(A)' ) ' Calibration    : NONE'
      else
          call enq_src_name( cf_lun, lsf_cal_num, string, s )
          write( lun, '(A,I2,2A)')
     *                        ' Calibration    : No. ',
     *        lsf_cal_num, ' on ', string(1:15)
      end if

      if ( flag_flag .eq. 0 ) then
          write( lun, '(A)' ) ' Flagging       : NONE'
      else
          call enq_namfil( sf_lun, 'FLAG', ff_name, s )
          call flag_open( ff_name, flag_id, s )
          call flag_get_list( flag_id, flag_key, flist, nflist, s )
          call flag_close( flag_id, s )
          write( lun, '( A,4(12I4/28X) )')
     *                        ' Flagging       : Versions: ',
     *                        (flist(nf), nf=1,nflist)
      end if
      if ( pre_int_chop_type .eq. 0 ) then
          write(lun, '(A)') ' Interference   : No pre-processing chop'
      else
          write(lun,'(A)')
     *              ' Interference   : Pre-processing chop applied'
          call lsf_prn_int_chop( pre_int_chop_type,
     *                           pre_int_chop_params, s )
      end if

      if ( post_int_chop_type .eq. 0 ) then
          write(lun, '(A)') '                  No post-processing chop'
      else
          write(lun,'(A)')
     *              '                  Post-processing chop applied'
          call lsf_prn_int_chop( post_int_chop_type,
     *                           post_int_chop_params, s )
      end if

      if ( smooth_type .eq. 0 ) then
          write(lun, '(A)')      ' Smoothing      : NONE '
      else
          write(lun, '(A,I4,A)') ' Smoothing      : ', smooth_size,
     *                           ' samples '
          write(lun, '(A,I4,A)') ' Sampling-rate  : ',
     *                             samp_rate, ' samples'
          if (smooth_type.eq.2 .or. smooth_type.eq.3)
     *        write(lun, '(A)')
     *    '                  Scaled by the Ion correction amplitude'
          if (smooth_type.eq.3) write(lun, '(A)')
     *    '                  Weighted by the square of the amplitude'
      end if

      write(lun,*)

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_DISPLAY' )
          return
      end
