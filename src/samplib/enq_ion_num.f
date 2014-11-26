

C+ENQ_ION_NUM

       subroutine enq_ion_num ( lun, key, corr_num, s )
C
C     Returns the number of an ionospheric correction.
C
C     Given:
C         The logical unit number of the physical sample file.
              integer         lun
C         Ionospheric correction key.
              integer         key

C
C     Returned:
C         The number of this ionospheric correction.
              integer         corr_num
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

      integer     i

      if ( s .ne. 0 ) return

      call read_ion_ct ( lun, s )
      if ( s .ne. 0 ) goto 999

      corr_num = 0
      if (key.ne.0) then
          do 10, i = 1, num_corrs
              if (ion_key(i).eq.key) corr_num = i
  10      continue
      end if

      if (corr_num.eq.0) s = NO_SRCNUM
      if (s.ne.0) goto 999
      return

 999  call smp_wrerr( s, 'in subroutine ENQ_ION_NUM' )

      end
