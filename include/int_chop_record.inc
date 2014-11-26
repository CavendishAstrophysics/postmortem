C
C Interference chop definition record
C -----------------------------------
C
C     Interference chop records define the interference chops which
C     are valid for logical sample files.
C
C ======================================================================

C     Interference chop definition
C     ----------------------------

C     Interference chop record.
          integer*4       int_chop_record( 10 )

C     Interference chop type 1 parameter:
C     Single integer value for maximum signal level on Cos or Sine.
          integer         int_max_signal
          equivalence   ( int_max_signal, int_chop_record(1) )

C     Interference chop types 2 & 3 parameters:
C     Single real value for maximum amplitude on (Cos+Sine)
          real            max_signal
          equivalence   ( max_signal, int_chop_record(1) )

C     Chop Type 2:
C     Percentage of visibilities clipped for whole sample rejection.
          real            clip_limit
          equivalence   ( clip_limit, int_chop_record(2) )

C     Chop Type 3:
C     S.D. of noise on either channel for whole sample rejection.
          real            max_noise
          equivalence   ( max_noise, int_chop_record(2) )

C     Interference chop type 4 parameters:
C     Single real value for maximum signal level on Cos or Sine.
          real            max_int_signal
          equivalence   ( max_int_signal, int_chop_record(1) )

C     chop type 5:
C     Clips above the line defined by pairs amplitudes and spacings.
C     Upto 5 pairs of max amplitude and corresponding spacing
C
          real            multi_level(10)
          equivalence   ( multi_level, int_chop_record(1) )
