


C+ enq_samp_rt

       subroutine enq_samp_rt( lun, sr_status, sr_ra, sr_dec,
     *                              sr_sid, sr_wt, s         )
C
C returns the sample redtape parameters
C
C Given
C   lun             -         i4         -      logical unit number
C Returned
C   sr_status       -         i4         -      sample status
C   sr_ra           -         r8         -      sample RA
C   sr_dec          -         r8         -      sample DEC
C   sr_sid          -         i4         -      sample ST
C   sr_wt           -         i4         -      sample weight
C   s               -         i4         -      error return code
C
C returns the parameters for the sample redtape for the current
C sample. This routine should therefore be proceeded to a call
C to read sample. S is not changed by this routine.
C
C In the present version no distinction is made between V1 and V2 of
C the CT.
C
C [PA, 9/11/88]
C-

       integer      lun, s
       integer      sr_status, sr_sid, sr_wt
       real*8       sr_ra, sr_dec

       include '/mrao/post/include/samp_rt.inc'

C check status on entry
       if ( s.ne.0 ) return

C assign values
c      print *,'.. assigning RT values in ENQ_SAMP_RT'
       sr_status = samp_status
       sr_ra     = samp_ra
       sr_dec    = samp_dec
       sr_sid    = samp_sid_time
       sr_wt     = samp_wt

       end
