
C+save_sf

      subroutine save_sf( file_name, s )

C     Places the specified sample file on the save list.
C
C     Given:
C         Name of file to be saved - must be a valid sample file.
              character*(*)   file_name

C     Returned:
C         Status - must be zero on entry
              integer         s
C
C     Places the given file on the correct tape save list as determined
C     by the routines in examine-samp and examine-lsf. If the file is
C     a physical sample file then the ionospheric correction description
C     file is also placed on the save list. The current value of the
C     save indicator in a physical sample file is ignored.
C
C     The file must be a valid sample file or a file with the type 'LSF'
C     It is assumed to be closed and is left in this state.
C
C     S = 0 for successful return, otherwise errcode.
C
C     NPR,   September 1988.
C-
C
      integer         lun, sf_type, buffer(16), save_flg
      character*64    full_name, name
      character*33    user
      character*4     type

C     Check for non-zero entry status

      if ( s .ne. 0 ) return

C     Estabish whether the file exists.
      call io_namfil( file_name, full_name, 0, s )
      if (s.ne.0) goto 9999

C     Establish whether the file is a logical sample file
      call io_brkfil(full_name, user, name, type)

      if ((type.ne.'LSF ') .and. (type.ne.'FLAG')
     *                     .and. (type.ne.'FLG ') ) then
C         Open the sample file and find its type.
          call open_sf( lun, full_name, 'READ', 0, s )
          call enq_sftype( lun, sf_type, s )
          if (s.ne.0) goto 9999

          if (sf_type .eq. 1) then
              call enq_namfil( lun, 'ION', name, s )
              if (s.ne.0) then
C                 :ion file does not exist
                  s = 0
                  name = ' '
              end if
              call close_sf( lun, s )

C             Put sample file on save list if necessary.
              call exs_read( full_name, buffer, save_flg, s )
              call exs_update( full_name, save_flg, s )

C             Prepare to save :ion file
              full_name = name
          else
              call close_sf( lun, s )
          end if
      end if

C     Save remove, calibration and ion files.
      if (full_name .ne. ' ') then
          call exl_read( full_name, buffer, save_flg, s )
          if (save_flg .ge. 0) then
              save_flg = 1
              call exl_save( full_name, save_flg, s )
          end if
      end if

      if (s .ne. 0) goto 9999
      return

 9999 call smp_wrerr( s, ' in subroutine save_sf' )

      end
