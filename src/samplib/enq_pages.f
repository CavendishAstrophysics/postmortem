

C+ENQ_PAGES

       subroutine enq_pages ( lun, num_pages, s )
C
C     Returns the number of pages in a sample file.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C
C     Returned:
C         The size of the sample file in pages.
              integer        num_pages
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     The size of the sample file is calculated from the information
C     contained in the control tables.
C
C     Possible return status's are:
C         ILL_CONTTAB     - Error in control tables.
C
C     DJT, 28/10/91
C
C-

C     Global includes -
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/ctab_pack.inc'
      include '/mrao/post/include/src_pack.inc'

      integer     max_bytes
      integer     Rmax

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .eq. 0) then
         s = Rmax( lun, max_bytes )
         num_pages = max_bytes/2048
      else if (ct_vers .ge. 1) then
         call enq_ctab_pack( lun, ctab_pack, s )
         call enq_src_pack( lun, ct_num_src, src_pack, s )
         num_pages = (src_samp_ptr+src_num_samp*src_samp_len-1)/page + 1
      end if

      if ( s .ne. 0 ) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine ENQ_PAGES' )

      end
