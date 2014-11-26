
C     *****************************************************************
C
C
C     *****************************************************************
C
C+lsf_sel_int_chop
C
      SUBROUTINE lsf_sel_int_chop (   int_chop_type,
     *                                int_chop_params,
     *                                s                      )

C
C     Asks the user to select values for the interference chop record.
C
C     Given:
C         Interference chop type
              integer*4       int_chop_type
C         Interference chop parameters
              integer*4       int_chop_params(10)

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C     Tidied by PJW to remove Course-clip (ie integer jansky)
C     New multi-level clip added
C                                                            PJW 19/9/90
C     new type 6 = type 3(noise) but test amplitude
C                              rather than cos .or. sin
C     type 3 selection suppressed                            PJW 301090
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/chrlib_functions.inc'
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/int_chop_record.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         Loop counter
              integer             i
C         Valid interference chop types and users selection
              character*(70)       chop_types(5), reply
              data    chop_types /
     *'no-clip ............ no interference clipping',
     *'clip-cos-sin ....... clip individual visibilities (standard)',
     *'percentage-clip .... clip & reject sample on percentage clipped',
     *'noise-clip  ........ clip & reject sample on sample noise',
     *'multi-clip  ........ clip at a spacing dependant level'/

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Copy interference chop parameters to definition record
      do 100, i = 1, 10
          int_chop_record(i) = int_chop_params(i)
  100 continue

C     ****************************************************************
C
C         Main Code
C         ---------
C
      i = chr_lenw( chop_types(int_chop_type+1) )
      call io_getopt( 'Interference-chop type (?=list) : ',
     *              chop_types(int_chop_type+1)(1:i), chop_types, 5,
     *              reply, s                   )
      if (s.ne.0) goto 9999

      if ( chr_cmatch( reply, chop_types(1) ) ) then
          int_chop_type = 0

      else if ( chr_cmatch( reply, chop_types(2) ) ) then
          if (int_chop_type.eq.2 .or. int_chop_type.eq.3) then
            max_int_signal = max_signal
          else if (int_chop_type.eq.1) then
            max_int_signal = int_max_signal
          else if (int_chop_type.eq.0) then
            max_int_signal = 10000.0
          end if
          call io_getr( 'Maximum signal on cos or sine channel :', '*',
     *                max_int_signal, s                            )
          max_int_signal = abs( max_int_signal )
          if (s.eq.0) int_chop_type = 4

      else if ( chr_cmatch( reply, chop_types(3) ) ) then
          if (int_chop_type.eq.1) then
            max_signal = int_max_signal
          else if (int_chop_type.eq.4) then
            max_signal = max_int_signal
          else if (int_chop_type.eq.0) then
            max_signal = 10000.0
          end if
          call io_getr( 'Maximum amplitude of visibility : ', '*',
     *                max_signal, s                                )
          max_signal = abs( max_signal )

          if (int_chop_type.ne.2) clip_limit = 100.0
          call io_getr(
     *    'Percentage visibilities clipped to reject sample-buffer : ',
     *    '*', clip_limit, s                                   )
          clip_limit = max( 0.0, min( 100.0, abs(clip_limit) ) )
          if (s.eq.0) int_chop_type = 2

      else if ( chr_cmatch( reply, chop_types(4) ) ) then
          if (int_chop_type.eq.1) then
            max_signal = int_max_signal
          else if (int_chop_type.eq.4) then
            max_signal = max_int_signal
          else if (int_chop_type.eq.0) then
            max_signal = 10000.0
          end if
          call io_getr( 'Maximum amplitude of visibility : ', '*',
     *                max_signal, s                                )
          max_signal = abs( max_signal )

          if (int_chop_type.ne.2) max_noise = max_signal/5.0
          call io_getr( 'Noise level to reject sample-buffer : ',
     *               '*', max_noise, s                             )
          max_noise = abs( max_noise )
          if (s.eq.0) int_chop_type = 6

      else if ( chr_cmatch( reply, chop_types(5) ) ) then
          do 150 i = 1, 10, 2
           multi_level(i) = -1.
           multi_level(i+1) = -1.
152        call io_getnr(
     *             'Maximum amplitude and corresponding spacing : ',
     *                  '*', multi_level(i), 2, s                 )
           if ( ( s .ne. 0 ) .or. ( multi_level(i) .eq. -1. ) ) goto 151
             if ( i .gt. 1 ) then
               if( multi_level(i+1) .le. multi_level(i-1) ) then
C              check increasing order
                 call io_wrout(
     *                       'Spacing must be larger than previous one')
                 goto 152
               endif
             endif
150       continue
151       s = 0
          int_chop_type = 5
      end if

      if (s .eq. 0) then
          do 200, i = 1, 10
              int_chop_params(i) = int_chop_record(i)
  200     continue
      else
          goto 9999
      end if
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s.ne.usr_break) then
              call lsf_wrerr( s, 'in subroutine LSF_SEL_INT_CHOP' )
              return
          end if
      end
