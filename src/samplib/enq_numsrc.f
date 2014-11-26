

C+ENQ_NUMSRC

       subroutine enq_numsrc ( lun, num_src, s )
C
C     Returns the number of sources in a sample file.
C
C     Given:
C         The logical unit number of the sample file.
              integer        lun
C
C     Returned:
C         The number of sources in the sample file.
              integer        num_src
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     The information is obtained from the control tables so the
C     sources do not have to be open.
C
C     Possible return status's are:
C         ILL_CONTTAB     - Error in control tables.
C
C-

C     Global includes -
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/ctab_pack.inc'

      integer     sf_type

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .eq. 0) then
          call enq_sftype( lun, sf_type, s )
          if ( sf_type .eq. 1 ) then
C             Normal sample file with one source
              num_src = 1
          else if ( sf_type .eq. 2 ) then
C             Old remove file
              num_src = nrem
          else
              s = ILL_CONTTAB
          end if
      else if (ct_vers .ge. 1) then
          call enq_ctab_pack( lun, ctab_pack, s )
          num_src = ct_num_src
      end if

      if ( s .ne. 0 ) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine ENQ_NUMSRC' )

      end
