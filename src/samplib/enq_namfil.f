C+ENQ_namfil

       subroutine enq_namfil ( lun, file_type, file_name, s )
C
C     Returns names of other files associated with a sample file.
C
C     Given:
C         Logical unit number of open physical sample file.
              integer         lun
C         File type ('samp', 'lsf', 'cal', 'flag', 'gain', 'ion', 'rem',
C                    'zero' )
              character*(*)   file_type
C
C     Returned:
C         Full file name.
              character*(*)   file_name
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     General purpose routine for getting hold of file names with
C     with the appropriate endings and directories. The following
C     status's can be returned:
C         0           - status O.K.
C         NOT_PHYSF   - specified file is not a physical sample file.
C         NO_FILE     - specified associated file does not exist.
C         ILL_FILETYPE- File type character code does not match one
C                       of the above alternatives.
C         Other       - Unexpected io_system error.
C
C     The error is logged if it is ILL_FILETYPE or an unexpected error.
C
C     NPR     25 November 1987.
C     PA      12 August 1991.  Added support for FLAG files
C     DJT     9 August 1993.  Unix implementation
C
C-
C
      include '/mrao/include/chrlib_functions.inc'
      include '/mrao/include/iolib_functions.inc'
      include '/mrao/post/include/phys_tscopes.inc'
      include '/mrao/post/include/samplib_errors.inc'
C
C     Local variable declarations
C         String length.
              integer         ls
C         Temporary full file name.
              character*64    temp_name
C         Sample file name.
              character*64    sf_name
C         Sample file type.
              integer         sf_type
C         Telescope name & code
              character       tscope_name*16
              integer         tscope_code

C     ****************************************************************

      if ( s .ne. 0 ) return

      inquire( unit=lun, name = sf_name, iostat = s )
      call enq_sftype( lun, sf_type, s )
      if ( s .ne. 0 ) goto 999

      if (sf_type .eq. 1 .or. sf_type .eq. 2) then
          if (sf_type .eq. 2) then
             call io_wrout('sf is actually a remove file')
          end if

          ls = chr_ilstc(sf_name,'/') - 1

          if ( chr_cmatch( file_type, 'samp' ) ) then
              continue
          else if ( chr_cmatch( file_type, 'cal' )) then
              temp_name = sf_name(1:ls) // '/cal'
          else if ( chr_cmatch( file_type, 'flag' )) then
              temp_name = sf_name(1:ls) // '/flag'
          else if ( chr_cmatch( file_type, 'ion' )) then
              temp_name = sf_name(1:ls) // '/ion'
          else if ( chr_cmatch( file_type, 'lsf' )) then
              temp_name = sf_name(1:ls) // '/lsf'
          else if ( chr_cmatch( file_type, 'rem' )) then
              temp_name = sf_name(1:ls) // '/rem'
          else if ( chr_cmatch( file_type, 'gain' )) then
              temp_name = sf_name(1:ls) // '/data'
          else if ( chr_cmatch( file_type, 'zero' )) then
              call enq_tscope( lun, tscope_name, tscope_code, s )
              if ( tscope_name .eq. 'T151' .or.
     *             tscope_name .eq. '38MHZ' ) then
                  call enq_v1_zfile( temp_name, s )
              else
                  s = ILL_TSCOPE
              end if
          else
              s = ILL_FILETYPE
              goto 999
          end if

          call io_namfil( temp_name, file_name, 0, s )
      else
          s = NOT_PHYSF
      end if

      return

C     Error Handling

C
 999  call smp_wrerr( s, 'in subroutine ENQ_NAMFIL' )

      end
