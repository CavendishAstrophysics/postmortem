
C     *****************************************************************
C
c+sample_num
C
      subroutine sample_num(  buff_num,
     *                        sample,
     *                        start_buffer,
     *                        s                      )

C
C     Returns a sample number for a given buffer number.
C
C     Given:
C         Buffer number.
              integer             buff_num

C     Returned:
C         Sample number corresponding to the given buffer number.
              integer*4           sample
C         Sample number of the start of this buffer
              integer             start_buffer
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Uses the current logical sample file common blocks to return
C     the number of the first sample in the given logical sample
C     buffer. Note that the subroutines buffer_num and sample_num
C     are not true inverses beacause there can be more than one sample
C     in a buffer.
C
C     If a buff_num is input which is less than zero or greater than
C     the number of LSF buffers then a status of ILL_BUFFER is returned
C     and the return values are those for the first and last valid
C     buffer respectively.
C
C     If the LSF has no valid samples a status of ILL_LSF is returned.
C
C-
C     ****************************************************************

      logical         util_tstbit

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_definition.inc'
      include  '/mrao/post/include/lsf_runtime.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         Flag indicating whether buffer has been found or not.
              logical     found
C         Current buffer in search
              integer     buffer
C         Last valid sample number found
              integer     valid_sn

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
      if ((buff_num .lt. curr_buff) .or. (curr_buff .le. 0) ) then
          buffer       = 1
          sample       = 1
          valid_sn     = 0
          start_buffer = 1
      else
          buffer       = curr_buff
          sample       = curr_samp
          valid_sn     = sample
          start_buffer = start_buff
      end if

      found = .false.
  100 continue
          if ( util_tstbit(samp_list, sample) ) then
              valid_sn = sample
              found    = (buffer .eq. buff_num)
              if ( .not. found ) then
                  buffer       = buffer + 1
                  sample       = start_buffer + samp_rate
                  start_buffer = sample
              end if
          else
              sample = sample + 1
              if (sample .ge. (start_buffer + samp_rate)) then
                  start_buffer = start_buffer + samp_rate
              end if
          end if

          if ( sample .gt. num_samp ) then
              if ( valid_sn .eq. 0 ) then
                  s = ILL_LSF
                  goto 9999
              else
                  sample       = valid_sn
                  start_buffer = int((valid_sn-1)/samp_rate)*samp_rate+1
                  s            = ILL_BUFFER
              end if
          end if
      if ((.not. found) .and. (s .eq. 0)) goto 100

      if ( buff_num .le. 0 ) s = ILL_BUFFER

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine SAMPLE_NUM' )
          return
      end
