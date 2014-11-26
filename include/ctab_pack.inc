C     /mrao/post/include/ctab_pack.inc
C     *****************************************************************
C
C     Record defining how source data is stored in the control tables.
C
C     *****************************************************************

          integer*4           ctab_pack_len
          parameter         ( ctab_pack_len = 16 )
          integer*4           ctab_pack( ctab_pack_len )

C     Control tables packing block length in words ( = 16 )
          integer*4           ct_pack_len
          equivalence       ( ct_pack_len, ctab_pack( 1 ) )

C     Control tables length in pages ( = 6 ).
          integer*4           ct_pages
          equivalence       ( ct_pages, ctab_pack( 2 ) )

C     Control tables packing version number (1=CLFST, 2=Ryle)
          integer*4           ct_pack_type
          equivalence       ( ct_pack_type, ctab_pack( 3 ) )

C     Control tables type (1=Physical SF, 2=remove file, 3=gains file)
          integer*4           ct_type
          equivalence       ( ct_type, ctab_pack( 4 ) )

C     Sample file saved indicator.
          logical*4           ct_sf_saved
          equivalence       ( ct_sf_saved, ctab_pack( 5 ) )

C     Number of sources in sample file.
          integer*4           ct_num_src
          equivalence       ( ct_num_src, ctab_pack( 6 ) )

C     Maximum number of sources in sample file.
          integer*4           ct_max_src
          equivalence       ( ct_max_src, ctab_pack( 7 ) )

C     Word offset in control tables where first source information is.
          integer*4           ct_src_ptr
          equivalence       ( ct_src_ptr, ctab_pack( 8 ) )

C     Length of sample file source definitions
          integer*4           ct_len_src
          equivalence       ( ct_len_src, ctab_pack( 9 ) )

C     Length of source packing block ( = 16 )
          integer*4           ct_len_pack
          equivalence       ( ct_len_pack, ctab_pack( 10 ) )

