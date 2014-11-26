C     /mrao/post/include/sf_pack.inc

C     *********************************************************
C
C     Common block for file control block information.
C
C     *********************************************************

          integer*4           sf_pack_len
          parameter         ( sf_pack_len = 32 )
          integer*4           sf_pack( sf_pack_len )

          common  / sf_pack / sf_pack

C     Logical unit number for sample file.
          integer*4           sf_lun
          equivalence       ( sf_lun, sf_pack( 1 ) )

C     Source number for this buffer.
          integer*4           sf_src_num
          equivalence       ( sf_src_num, sf_pack( 2 ) )

C     Buffer start
          integer             buffer_ptr
          equivalence       ( buffer_ptr, sf_pack(  3 ) )

C     Buffer length
          integer             buffer_len
          equivalence       ( buffer_len, sf_pack(  4 ) )

C     Sample file type ( 1 = physical sample file, 2 = remove, 3 = cal )
          integer             sf_type
          equivalence       ( sf_type, sf_pack( 5 ) )

C     Word offset in file where first sample starts.
          integer             sf_first_samp_ptr
          equivalence       ( sf_first_samp_ptr, sf_pack( 6 ) )

C     Length of each sample in words.
          integer             samp_len
          equivalence       ( samp_len, sf_pack( 7 ) )

C     Number of samples in sample file.
          integer             num_samp
          equivalence       ( num_samp, sf_pack( 8 ) )

C     Word number of the start of visibility list in sample.
          integer             start_vis
          equivalence       ( start_vis, sf_pack( 9 ) )

C     Number of spacings in each sample
          integer             sf_num_vis
          equivalence       ( sf_num_vis, sf_pack( 10 ) )

C     Internal data type of visibilities. ( 1=I*2, 2=I*4, 3=R*4 )
          integer             data_type
          equivalence       ( data_type, sf_pack( 11 ) )

C     Internal units to external units (Jy) conversion factor.
          real*4              amp_factor
          equivalence       ( amp_factor, sf_pack( 12 ) )

C     Start of redtape
          integer             start_rt
          equivalence       ( start_rt, sf_pack( 13 ) )

C     Redtape length
          integer             length_rt
          equivalence       ( length_rt, sf_pack( 14 ) )

C     Start of monitor block
          integer             start_mon
          equivalence       ( start_mon, sf_pack( 15 ) )

C     Length of monitor block
          integer             length_mon
          equivalence       ( length_mon, sf_pack( 16 ) )

C     Start of ionospheric correction data block
          integer             start_ion
          equivalence       ( start_ion, sf_pack( 17 ) )

C     Length of ionospheric correction data block
          integer             length_ion
          equivalence       ( length_ion, sf_pack( 18 ) )

C     Offset from the start of this source's buffer to the start
C     of the data for the first complete sample.
          integer             data_offset
          equivalence       ( data_offset, sf_pack( 19 ) )

C     Number of first complete sample in buffer.
          integer             first_samp
          equivalence       ( first_samp, sf_pack( 20 ) )

C     Current sample number
          integer             curr_samp
          equivalence       ( curr_samp, sf_pack( 21 ) )

C     Number of last complete sample in buffer.
          integer             last_samp
          equivalence       ( last_samp, sf_pack( 22 ) )

C     Block size in words.
          integer             block_size
          equivalence       ( block_size, sf_pack( 23 ) )

C     The following three variables are for use when accessing the
C     file using sidereal time as a key. The start and stop times are in
C     units of 10ths of sidereal seconds.
C     Sidereal time at start of the file.
          integer             start_time
          equivalence       ( start_time, sf_pack( 24 ) )

C     Sidereal time at end of the file.
          integer             stop_time
          equivalence       ( stop_time, sf_pack( 25 ) )

C     Interpolation type to use when interpolating between samples.
C         ( 1 = use next sample value, 2 = linear interpolation )
          integer             interp_type
          equivalence       ( interp_type, sf_pack( 26 ) )

C     Flag set to indicate that data in the buffer has been updated
C     and should be written to disc.
          integer             update_flag
          equivalence       ( update_flag, sf_pack( 27 ) )

