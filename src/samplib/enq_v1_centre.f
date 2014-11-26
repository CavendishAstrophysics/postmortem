C+ENQ_V1_CENTRE

       subroutine enq_v1_centre ( num_centre, src_name, sf_name,
     :                       num_samp, ra_ref, dec_ref, centre_num, s )
C
C     Returns the pointing centres associated with a sample file.
C
C     Returned:
C         Number of pointing centres
              INTEGER         NUM_CENTRE
C         Source name for each pointing centre
              CHARACTER*24    SRC_NAME(*)
C         Sample file name for each pointing centre
              CHARACTER*40    SF_NAME(*)
C         Number of samples at each centre (multi-centre observations)
              INTEGER         NUM_SAMP(*)
C         RA, Dec at reference date for each centre
              REAL*8          RA_REF(*)
              REAL*8          DEC_REF(*)
C         Pointing centre index for this sample file
              INTEGER         CENTRE_NUM
C         Status
              INTEGER         S
C
C     This routine returns the details of the pointing centres for the
C     sample file opened on logical unit LUN.  Single-centre observations
C     return NUM_SAMP equal to 1.
C
C     Control Tables Version 1 support routine for ENQ_CENTRES
C
C     DJT     23 March 1992
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/control_tab_v0.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      num_centre = 1
      src_name(1) = source
      sf_name(1) = sfile
      num_samp(1) = 1
      ra_ref(1) = raref
      dec_ref(1) = decref
      centre_num = 1

      end
