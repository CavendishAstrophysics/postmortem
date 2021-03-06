C
C+map_save
C
      SUBROUTINE map_save (map_name, s)

C     Saves the current map definition.
C
C     Given:
C         Default map name.
              character*(*)       map_name
C
C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer             s
C
C last change GP 24 May 2000
C-
C     ****************************************************************
C
C     Function definitions
C
      include    '/mrao/include/iolib_functions.inc'

C     ****************************************************************
C
C     Global includes -
C         (i.e. Global constant, variable and common declarations)
C
      include    '/mrao/include/iolib_errors.inc'
      include    '/mrao/include/maplib_redtape.inc'

C     ****************************************************************
C
C     Local variables and arrays
C         Current logical sample file number
              integer         lsf_num
C         Physical sample file name, and logical sample file key
              character*80    sf_name
              integer         lsf_key
C         Map file unit number
              integer         lun
C         length of file-name
              integer         length
C       default map-name extensions
                character*4     map_type(3)/'map','beam','bset'/
C       default map directory, derived name
                character*80    def_dir, full_name

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if (s .ne. 0) return

C     ****************************************************************
C
C         Main Code
C         ---------
C

      call enmlsf(1, sf_name, lsf_key, s)

      if (lsf_key .eq. 1) then
C         Logical sample file must be saved
          call io_wrout('Logical sample file must be saved.')
          call lsf_open(sf_name, lsf_key, 'READ', lsf_num, s)
          call lsf_save(lsf_num, lsf_key, s)
          call lsf_close(lsf_num, s)
          call stmlsf(1, sf_name, lsf_key, s)
          if (s .ne. 0) goto 9999
      end if

        call enmdir   (def_dir, s)

        call io_makfil (def_dir, map_name, map_type(maptyp),
     *                    full_name, length)
        call io_getfil('Map file:', '*', full_name, s)
        map_name = full_name
        call io_nxtlun (lun, s)
        if (s .ne. 0) goto 9999

        open (lun,      file = map_name,
     *                  status = 'UNKNOWN',
     *                  iostat = s)

c      call getmap('Map name : ', '*', map_name, s)
c      call enmpag(num_pages, s)
      rtsflg = 0
c      call io_resfil(map_name, num_pages, .true., 0, s)
c      call opemap(lun, map_name, 'WRITE', 1, s)


      if (s .eq. 0) then
          call wrredt(lun, 0, s)
          close (lun)
      end if

      if (s .ne. 0 .and. s.ne. USR_BREAK) then
          s = 0
          call io_wrout('Save failed - investigate and try again.')
      end if

      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
          if (s.ne.USR_BREAK) then
              call map_wrerr(s, 'in subroutine MAP_SAVE')
          end if
          return
      end
