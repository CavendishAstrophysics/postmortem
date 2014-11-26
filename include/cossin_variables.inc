C
C     Local parameters
C         Conversion from period to index and radians to index
              integer         p2index
              parameter     ( p2index = 1024 )
              real*8          r2index
              parameter     ( r2index = p2index/const_2pi )
C         Sine offset from cosine
              integer         sin_offset
              parameter     ( sin_offset = -p2index/4 )
C         Uninitialised value of the table center value
              integer         init_cntr_cos
              parameter     ( init_cntr_cos = 2**30 )

C     Saved tabulated values
C         Tabulated sines and cosines
              real            table( -(p2index-sin_offset):p2index )
C         Current cosine index at the table centre
              integer         cntr_cos
          common   / cossin / table, cntr_cos
          data                cntr_cos / init_cntr_cos /

C     Local variables
C         Current index
              integer         index
C         Pointer in the table to the current value of cosine
              integer         cos_ptr
C         Loop counter
              integer         local_ctr

