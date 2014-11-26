C **********************************************************************
C
C     Common block for monitor information for the current sample.
C
C                Version 2    for the  Ryle telescope
C
C     This common block is stored with every sample in the sample file.
C     It contains flags, autocorrelations, pointing errors,
C     receiver parameters, rain-gauge data  and miscellaneous data.
C
C     Last updated 20 Dec 01 [GP]
C
C **********************************************************************

C     Parameter definitions

          integer             num_items
          parameter         ( num_items = 16 )
          integer             num_aerials
          parameter         ( num_aerials = 8 )
          integer             num_auto
          parameter         ( num_auto = 5*8*4 )
          integer             num_flag
          parameter         ( num_flag = 28*5*8*4/16 )
          integer             num_misc
          parameter         ( num_misc = num_aerials )
          integer             mon_length
          parameter         ( mon_length = num_aerials*num_items
     :                                   + num_auto
     :                                   + num_flag              )

C **********************************************************************

          integer*2           mon_block( mon_length )
          common  / monitor_v2 / mon_block

C **********************************************************************

C     Definition of the Monitor-Block Record

* Contents:
*     flags
*     autocorrelations
*     assorted ae-based parameters
*
*         0   ae status
*         1,2 HA pointing err (mean, mean mod)
*         3   spare
*         4,5 Dec pointing err (mean, mean mod)
*         6   spare
*         7   Tcryo
*         8   LO alc
*         9   vac
*        10   rain
*        11,12 rain corr fac (R*4)
*        13   misc [first = windspeed]
*        14   IF alc
*        15   P He


C     Visibility flag bits (allowing 4 bits per visibility)
          integer*2           mon_flags( num_flag )
          equivalence       ( mon_flags, mon_block(1) )

C     Auto-correlations
          integer*2           mon_auto( num_auto )
          equivalence       ( mon_auto, mon_block(num_flag+1) )

C     Array containing aerial based parameters
          integer*2           mon_pars( num_items*num_aerials )
          equivalence       ( mon_pars, mon_block(num_flag+num_auto+1) )

C     Aerial status words updated during the run
          integer*2           mon_ae ( num_aerials )
          equivalence       ( mon_ae,  mon_pars(1) )

* Mean and mean absolute pointing errors in HA and Dec
* during the current sample, in arcsec.
* (the third column of this array is now spare)
          integer*2           mon_ha ( num_aerials, 3 ),
     *                        mon_dec( num_aerials, 3 )
          equivalence       ( mon_ha,  mon_pars(num_aerials+1) ),
     *                      ( mon_dec, mon_pars(num_aerials*4+1) )

C     Actual 16 bit digitised values for Tcryo, alc1 and alc2
          integer*2           mon_Tcryo( num_aerials ),
     *                        mon_alc1 ( num_aerials ),
     *                        mon_vac  ( num_aerials ),
     *                        mon_PHe  ( num_aerials )
          equivalence       ( mon_Tcryo,mon_pars(num_aerials*7+1) ),
     *                      ( mon_alc1, mon_pars(num_aerials*8+1) ),
     *                      ( mon_vac,  mon_pars(num_aerials*9+1) ),
     *                      ( mon_PHe,  mon_pars(num_aerials*15+1))

C     Averaged rain gauge readings, 12 bits, (0-4095 represents 0-10V)
C     and derived rain correction factors
          integer*2           mon_rn  ( num_aerials )
          real*4              mon_rfac( num_aerials )
          equivalence       ( mon_rn,   mon_pars(num_aerials*10+1) )
          equivalence       ( mon_rfac, mon_pars(num_aerials*11+1) )

C     Array containing miscellaneous parameters
          integer*2           mon_misc( num_misc )
          equivalence       ( mon_misc, mon_pars(num_aerials*13+1) )

C     Wind gauge reading, 12 bits, (0-4095 represents 0-10V, 1V=10knots)
          integer*2           mon_wind
          equivalence       ( mon_wind, mon_misc(1) )

* IF alc readings

          integer*2           mon_IFalc(num_aerials)
          equivalence        (mon_IFalc, mon_pars(num_aerials*14+1))

* end
