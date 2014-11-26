C     /mrao/post/include/sf_save.inc

C     *********************************************************
C
C     Common block for saving file control block information.
C
C     *********************************************************

          integer*4           max_sf
          parameter         ( max_sf = 10 )

C         Pointers to the only elements in the packing parameters
C         that are referred to explicitly when the packing parameters
C         are in this save block.

          integer             lun_ptr, src_ptr, start_ptr, length_ptr
          parameter         ( lun_ptr    = 1 )
          parameter         ( src_ptr    = 2 )
          parameter         ( start_ptr  = 3 )
          parameter         ( length_ptr = 4 )

          integer*4           sf_save( sf_pack_len, max_sf )
          common  / sf_save / sf_save
