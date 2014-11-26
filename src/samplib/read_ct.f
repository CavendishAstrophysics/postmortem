C+READ_CT

      subroutine read_ct ( lun, s )
C
C     Reads control tables.
C
C     Given:
C         Sample file logical unit number
              integer     lun
C     Returned:
C         Status - must be zero on entry. If non zero on exit it is
C         a io_system error message.
              integer     s
C
C     Routine to read control tables from a sample file on disc into
C     the run-time common blocks. These common blocks are declared in
C     the include files:
C
C       '/mrao/post/include/control_tables.inc'  Outline and common block
C       '/mrao/post/include/control_tab_v0.inc'       Version 0 control tables
C       '/mrao/post/include/control_tab_v1.inc'       Version 1 control tables
C       '/mrao/post/include/control_tab_v2.inc'       Version 2 control tables
C
C     Disc file must be opened on unit LUN.  If the current control
C     tables are from the requested unit number no read is performed
C     and the routine is a null routine.
C
C-
      integer   no_words
      integer   ct_length
c
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'
c

      if (s.ne.0) return

      if (lun.ne.ct_lun) then
c
         no_words = ct_min_len*page
         call io_rdfile( lun, 1, ct_all, no_words, s )
         if (s.ne.0) goto 999
         if (ctv0_verno.eq.0 .and. ctv0_pages.eq.4) then
            ct_vers = ctv0_verno
            ct_length = ctv0_pages
         elseif (ctv1_verno.eq.1 .and. ctv1_pages.eq.6) then
            ct_vers = ctv1_verno
            ct_length = ctv1_pages
         elseif (ctv2_verno.eq.2 .and.
     :          (ctv2_pages.eq.6 .or. ctv2_pages.eq.8)) then
            ct_vers = ctv2_verno
            ct_length = ctv2_pages
         else
            s = ILL_CONTTAB
         endif
c
         if (s .eq. 0) then
            if (ct_length.gt.ct_min_len) then
               no_words = ct_length*page
               call io_rdfile( lun, 1, ct_all, no_words, s )
               if (s.ne.0) goto 999
            endif
            ct_lun = lun
         endif
c
      endif
c
      return

 999  call smp_wrerr( s, 'in subroutine READ_CT' )

      end
