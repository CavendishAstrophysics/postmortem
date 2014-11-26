C     /mrao/post/include/src_pack.inc
C     *****************************************************************
C
C     Record defining how source data is stored in a sample file.
C
C     *****************************************************************

          integer*4           src_pack_len
          parameter         ( src_pack_len = 32 )
          integer*4           src_pack( src_pack_len )

C     Source packing type
          integer*4           src_pack_type
          equivalence       ( src_pack_type, src_pack( 1 ) )

C     Word offset in file where first sample starts.
          integer*4           src_samp_ptr
          equivalence       ( src_samp_ptr, src_pack( 2 ) )

C     Number of samples in sample file.
          integer*4           src_num_samp
          equivalence       ( src_num_samp, src_pack( 3 ) )

C     Length of each sample in words.
          integer*4           src_samp_len
          equivalence       ( src_samp_len, src_pack( 4 ) )

C     Start of redtape
          integer*4           src_start_rt
          equivalence       ( src_start_rt, src_pack( 5 ) )

C     Redtape length
          integer*4           src_length_rt
          equivalence       ( src_length_rt, src_pack( 6 ) )

C     Word number of the start of visibility list in sample.
          integer*4           src_start_vis
          equivalence       ( src_start_vis, src_pack( 7 ) )

C     Number of spacings in each sample
          integer*4           src_max_vis
          equivalence       ( src_max_vis, src_pack( 8 ) )

C     Internal data type of visibilities. ( 1=I*2, 2=I*4, 3=R*4 )
          integer*4           src_data_type
          equivalence       ( src_data_type, src_pack( 9 ) )

C     External units (Jy) to internal units conversion factor.
          real*4              src_amp_factor
          equivalence       ( src_amp_factor, src_pack( 10 ) )

C     Word offset in sample where monitor block starts
          integer*4           src_start_mon
          equivalence       ( src_start_mon, src_pack( 11 ) )

C     Monitor block length
          integer*4           src_length_mon
          equivalence       ( src_length_mon, src_pack( 12 ) )

C     The following three variables are for use when accessing the
C     file using sidereal time as a key. The start and stop times are in
C     units of 10ths of sidereal seconds.
C     Sidereal time at start of the file.
          integer*4           src_start_time
          equivalence       ( src_start_time, src_pack( 13 ) )

C     Sidereal time at end of the file.
          integer*4           src_stop_time
          equivalence       ( src_stop_time, src_pack( 15 ) )

C     Interpolation type to use when interpolating between samples.
C         ( 1 = use next sample value, 2 = linear interpolation )
          integer*4           src_interp_type
          equivalence       ( src_interp_type, src_pack( 16 ) )
