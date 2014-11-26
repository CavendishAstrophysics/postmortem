C+ENQ_ION_NAME

       subroutine enq_ion_name ( lun,
     *                           corr_num,
     *                           corr_name,
     *                           s           )
C
C     Returns the name of an ionospheric correction.
C
C     Given:
C         The logical unit number of the physical sample file.
              integer         lun
C         Ionospheric correction number.
              integer         corr_num
C
C     Returned:
C         The name for this correction.
              character*(*)   corr_name
C         Status variable - must be zero on entry otherwise error.
              integer         s
C
C     Reads the control tables for the specified file and returns the
C     given ionospheric correction information.
C
C     Possible return status's are:
C         NO_SRCNUM       - No such correction. (Error not logged)
C
C-

C     Global includes -
C
      include '/mrao/post/include/samplib_errors.inc'
      include '/mrao/post/include/ion_definition.inc'

      integer     src_offset, ls, i, chr_lenb

      if ( s .ne. 0 ) return

      call read_ion_ct ( lun, s )
      if ( s .ne. 0 ) goto 999

      if (corr_num .le. 0 .or. corr_num .gt. num_corrs) then
          s = NO_SRCNUM
      else
          src_offset  = 1
          do i = 1, corr_num-1
              src_offset = src_offset + ion_srcs(i)
          end do

          corr_name = ion_source(src_offset)
          ls = chr_lenb( corr_name ) + 1
          do i = src_offset+1, src_offset+ion_srcs(corr_num)-1
              corr_name(ls:) = ', '//ion_source(i)
              ls = ls + chr_lenb(ion_source(i)) + 2
          end do
      end if

      if ( s .ne. 0 ) goto 999

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_ION_NAME' )

      end
