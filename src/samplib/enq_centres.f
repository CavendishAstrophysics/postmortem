C+ENQ_CENTRES

       subroutine enq_centres ( lun, num_centre, src_name, sf_name,
     :                        num_samp, ra_ref, dec_ref, centre_num, s )
C
C     Returns the pointing centres associated with a sample file.
C
C     Given:
C         Sample file Fortran logical unit number.
              INTEGER         LUN
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
C     This routine returns the details of the pointing centres associated
C     with the sample file opened on logical unit LUN.  Single-centre
C     observations produce one sample file and return NUM_CENTRE,
C     CENTRE_NUM and NUM_SAMP equal to 1.  Multi-centre observations
C     produce NUM_CENTRE sample files, each containing the visibility data
C     for a single pointing centre.  This routine returns the details of
C     all pointing centres together with the index CENTRE_NUM identifying
C     which centre is associated with the data in the current file.
C     NUM_SAMP gives the number of consecutive samples observed at each
C     centre during the run.
C
C     DJT     23 March 1992
*-

      include '/mrao/post/include/control_tables.inc'
      include '/mrao/post/include/samplib_errors.inc'

      if ( s .ne. 0 ) return

      call read_ct( lun, s )
      if ( s .ne. 0 ) goto 999

      if (ct_vers .le. 1) then
         call enq_v1_centre( num_centre, src_name, sf_name, num_samp,
     :                                   ra_ref, dec_ref, centre_num, s)
      else if (ct_vers .eq. 2) then
         call enq_v2_centre( num_centre, src_name, sf_name, num_samp,
     :                                   ra_ref, dec_ref, centre_num, s)
      else
         s = ILL_CONTTAB
         goto 999
      endif

      return

 999  call smp_wrerr( s, 'in subroutine ENQ_CENTRES' )

      end
