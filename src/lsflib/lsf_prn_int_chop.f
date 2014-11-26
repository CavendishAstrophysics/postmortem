C
C+lsf_prn_int_chop
C
      SUBROUTINE lsf_prn_int_chop(    int_chop_type,
     *                                int_chop_params,
     *                                s                      )

C
C     Prints a description of the interference chop on the output device
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
C-
C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/int_chop_record.inc'

C     ****************************************************************
C
C     Variables, equivalences and commons
C         String and length
              character*20    string
              integer         ls, i

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

C     Copy interference chop parameters to definition record
      do i = 1,10
            int_chop_record(i) = int_chop_params(i)
      enddo

C     ****************************************************************
C
C         Main Code
C         ---------
C
      if ( int_chop_type .eq. 0 ) then
          call io_wrout( '                  NONE' )
      else if ( int_chop_type .eq. 1 ) then
          call chr_chitoc( int_max_signal, string, ls )
          call io_wrout( '                 -- maximum amplitude '//
     *                string(1:ls)//' integer Jy' )
      else if ( int_chop_type .eq. 2 ) then
          call chr_chrtoc( max_signal, string, ls )
          call io_wrout( '                 -- maximum amplitude '//
     *                string(1:ls) // ' Jy' )
          call chr_chrtoc( clip_limit, string, ls )
          call io_wrout( '                 -- reject buffer if > '//
     *                string(1:ls) // '% of visibilities rejected' )
      else if ( int_chop_type .eq. 3 ) then
          call chr_chrtoc( max_signal, string, ls )
          call io_wrout( '                  -- maximum amplitude '//
     *                string(1:ls) // ' Jy' )
          call chr_chrtoc( max_noise, string, ls )
          call io_wrout(
     *                '                  -- reject buffer if noise > '//
     *                string(1:ls) // ' Jy on either cos or sin' )
      else if ( int_chop_type .eq. 4 ) then
          call chr_chrtoc( max_int_signal, string, ls )
          call io_wrout( '                  -- maximum cos/sin '//
     *                string(1:ls)//' Jy' )
      else if ( int_chop_type .eq. 5 ) then
          call io_wrout(
     *                '                 -- multi-level at amp/spacing:')
          write(1,'( 11X, 5( F9.3, F5.0 ))') multi_level
      else if ( int_chop_type .eq. 6 ) then
          call chr_chrtoc( max_signal, string, ls )
          call io_wrout( '                  -- maximum amplitude '//
     *                string(1:ls) // ' Jy' )
          call chr_chrtoc( max_noise, string, ls )
          call io_wrout(
     *                '                  -- reject buffer if noise > '//
     *                string(1:ls) // ' Jy ' )

      else
          s = ILL_INTCHOP
      end if

      if (s .ne. 0) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          call lsf_wrerr( s, 'in subroutine LSF_PRN_INT_CHOP' )
          return
      end
