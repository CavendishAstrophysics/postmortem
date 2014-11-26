C     *********************************************************
C
C     Common block for monitor information for the current sample.
C
C                  Version 1   for the   CLFST
C
C     *********************************************************
C
C     This common block is stored in exactly this format with
C     every sample in the sample file.  It contains global
C     information for how the telescope was performing whilst
C     taking the sample.

          integer             num_aerials
          parameter         ( num_aerials = 60 )
          integer             mon_length
          parameter         ( mon_length = num_aerials * 6 + 10 )
          integer*2           mon_block( mon_length )
          common  / monitor_v1 / mon_block

C     Range of digitised values is -max_par to +max_par
          integer             max_par
          parameter         ( max_par = 128 )

C     Required and actual 8 bit digitised values of hour angle and dec
C     for each aerial.
          integer*2           mon_ha ( num_aerials, 2 ),
     *                        mon_dec( num_aerials, 2 )
          equivalence       ( mon_ha,  mon_block( 1 )),
     *                      ( mon_dec, mon_block(num_aerials*2+1))

C     Actual 8 bit digitised values of pc & agc value for each aerial.
          integer*2           mon_pc(  num_aerials ),
     *                        mon_agc( num_aerials )
          equivalence       ( mon_pc,  mon_block(num_aerials*4+1)),
     *                      ( mon_agc, mon_block(num_aerials*5+1))

C     Count incremented during the run when the each of the above four
C     were last updated.
          integer*2           mon_update( 4 )
          equivalence       ( mon_update, mon_block(num_aerials*6+1))

C     Cumulative totals of the six different run time errors that
C     can occur.
C         1 - Zero errors
C         2 - Shift errors
C         3 - Totally correlated count error
C         4 - All '1's output
C         5 - Timing error, samples missed
C         6 - Interference recognised in AMPHI routine LAMBDA2
          integer*2           mon_errors( 6 )
          equivalence       ( mon_errors, mon_block(num_aerials*6+5))
