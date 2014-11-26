C+WRITE_CT

      subroutine write_ct ( lun, s )
C
C     Writes control tables.
C
C     Given:
C         Sample file logical unit number
              integer     lun
C     Returned:
C         Status - must be zero on entry. If non zero on exit it is
C         a io_system error message from WFILE.
              integer     s
C
C     Routine to write control tables to a sample file on disc from the
C     run-time common blocks. These common blocks are declared in
C     the include files:
C
C       '/mrao/post/include/control_tables.inc'
C
C     The disc file must be opened for write on unit LUN.
C
C-
      character*16   user
      integer        mode, termno
      integer        no_words
      integer        ct_length
c
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'
c
      if (s.ne.0) return

c     Set current user name

      call io_enqexe( user, mode, termno )
c     if (user.eq.'BATCH-USER' .or.
c    :    user.eq.'SYSTEM' .or. user.eq.'RT') user=ct_user
c     ct_user = user

      if (ct_vers .eq. 0) then
         ct_length = ctv0_pages
      elseif (ct_vers .eq. 1) then
         ct_length = ctv1_pages
      elseif (ct_vers .eq. 2) then
         ct_length = ctv2_pages
      else
         s = ILL_CONTTAB
      endif

      if (s.eq.0) then
         no_words = ct_length*page
         call io_wrfile( lun, 1, ct_all, no_words, s )
         if (s.ne.0) goto 999
      endif
      ct_lun = lun

      return

 999  call smp_wrerr( s, 'in subroutine WRITE_CT' )

      end
