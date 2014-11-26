C***********************************************************************
C
C     samp_rt.inc
C
C     Common block for redtape for the current sample.
C
C&**********************************************************************
C
C     This common block is stored in exactly this format with
C     every sample in the sample file.  It contains global
C     information for the visibilities that were collected.

          integer             samp_rt_length
          parameter         ( samp_rt_length = 8 )
          integer*4           samp_rt( samp_rt_length )
          common   / rtsamp / samp_rt

C     Number of visibilities correlated.
          integer*4           num_vis_corr
          equivalence       ( num_vis_corr, samp_rt( 1 ) )

C     Sample status word (see below for bit interpretations)
          integer*4           samp_status
          equivalence       ( samp_status, samp_rt( 2 ) )

C     Phase centre of sample (ra and dec in radians).
          real*8              samp_ra, samp_dec
          equivalence       ( samp_ra,  samp_rt( 3 ) ),
     *                      ( samp_dec, samp_rt( 5 ) )

C     Sidereal time sample was taken at - in 10ths/sec since sidereal
C     midnight.
          integer*4           samp_sid_time
          equivalence       ( samp_sid_time, samp_rt( 7 ) )

C     Weight to be given to sample - normally the number of correlator
C     samples included in this integrated sample.
          integer*4           samp_wt
          equivalence       ( samp_wt, samp_rt( 8 ) )

C     Sample status word - bit interpretations.
C         Bit         Meaning
C          0           indicates quarter wave state (normally zero).
C          1           always unset.
C          2           bad sample, correlator error or interference
C                                  rejection during observation.
C          8-11        old-style ionospheric correction (1-4) exists.
C          12-15       indicates phase of cycle for special observations
C                                (e.g. EXCYC, PCYC, NODDY running).
C


