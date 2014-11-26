C
C+buffer_num
C
      subroutine buffer_num(  samp_num,
     *                        buffer,
     *                        start_buffer,
     *                        s                      )

C
C     Returns a buffer number for a given sample number.
C
C     Given:
C         Sample number.
              integer*4           samp_num

C     Returned:
C         Buffer number corresponding to the given sample number
              integer             buffer
C         Sample number of the start of this buffer
              integer             start_buffer
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Uses the current logical sample file common blocks to return
C     a buffer number that corresponds to the given sample number.
C     Note that the subroutines buffer_num and sample_num are not true
C     inverses beacause there can be more than one sample in a buffer.
C
C     If a samp_num is input which is less than zero or greater than
C     the last valid sample then a status of ILL_BUFFER is returned
C     and the return values are those for the first (1), and last valid
C     buffer respectively.
C
C     If there are no valid samples then a status of ILL_LSF is
C     returned.
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
C         Current sample in search
              integer     sample

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
      if ((samp_num .lt. curr_samp) .or. (curr_samp .le. 0)) then
          buffer       = 1
          sample       = 1
          start_buffer = 1
      else
          buffer       = curr_buff
          sample       = curr_samp
          start_buffer = start_buff
      end if

      found = .false.
  100 continue
          if ( util_tstbit(samp_list, sample) ) then
              found = ((start_buffer + samp_rate) .gt. samp_num)
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
              if ( buffer .eq. 1 ) then
                  s = ILL_LSF
                  goto 9999
              else
                  buffer       = buffer - 1
                  start_buffer = start_buffer - samp_rate
                  s = ILL_BUFFER
              end if
          end if
      if ((.not. found) .and. (s .eq. 0)) goto 100

      if ( samp_num .lt. 0 ) s = ILL_BUFFER

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine BUFFER_NUM' )
          return
      end
