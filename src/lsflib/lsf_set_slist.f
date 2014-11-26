
C     *****************************************************************
C
C
C+lsf_set_slist
C
      SUBROUTINE lsf_set_slist ( lsf_num,
     *                           string,
     *                           list, list_len,
     *                           s                      )

C
C     Decode a sample selection into a list of samples
C
C     Given:
C         Logical sample file number
              integer*4           lsf_num
C
C         String to decode
              character*(*)       string
C
C     Returned:
C         List of samples
              integer             list(*)
C         Number of samples in the list
              integer             list_len
C
C     Updated:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C [PA, 13/8/91]
C-
C
C Function declarations

      include  '/mrao/include/chrlib_functions.inc'

C
C Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'
      include  '/mrao/post/include/lsflib_errors.inc'

C
C Local variables, equivalences and commons
C         Loop control variable
              integer*4       i
C         Count of valid samples in the list.
              integer         valid_samps
C         First and last sample in the sample range selected.
              integer         first_sample, last_sample
C         String pointers
              integer         i1, i2, i3

C  Check for non zero entry status
      if ( s .ne. 0 ) return

      if ( lsf_num .ne. curr_lsf_num ) then
          call get_lsf( lsf_num, s )
          if ( s .ne. 0 ) goto 9999
      end if
      lsf_key  = 1
      lsf_name = ' '

C Parse for any alphabetic prefix
      i1 = chr_intlc(string)
      i2 = chr_lenw(string)
      i3 = chr_lenb(string)
      if ( chr_cmatch(string(i1:i2),'UT') .or.
     *     chr_cmatch(string(i1:i2),'HA') .or.
     *     chr_cmatch(string(i1:i2),'ST')       ) then
C .. Sample list is a sample time range
          call lsf_set_srange( lsf_num, string,
     *                         first_sample, last_sample, s )
          if (s.ne.0) goto 9999
          list_len = 0
          i = first_sample
          do while (i.le.last_sample)
            list_len = list_len + 1
            list(list_len) = i
            i = i + 1
          end do

      else

C .. decode string as a standard list
          call chr_chlsti( string(i1:i3), list, num_samp, list_len, s )
          if ( s .ne. 0 ) goto 9999
          valid_samps = 0
          do i = 1, list_len
             if ((1.le.list(i)) .and. (list(i).le.num_samp)) then
                 valid_samps = valid_samps + 1
             else
                 list(i) = 0
             end if
          end do

      end if

      if (s.ne.0) goto 9999
      return

C Error Handling
 9999 continue
          if ( s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_SET_SLIST' )
          end if
          return
      end
