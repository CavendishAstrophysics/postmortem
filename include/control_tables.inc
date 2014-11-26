C***********************************************************************
C
C     Control tables Run Time Common Block.
C     -------------------------------------
C
C     This definition contains the bare essentials about the control
C     tables currently set up in common.  Pointers are defined here
C     to aid the process of interpreting control table data within
C     the SAMPLIB routines READ_CT and WRITE_CT.
C
C     The current set of valid control table versions includes:
C
C         Version 0 : CLFST observation control tables.
C         Version 1 : CLFST postmortem control tables.
C         Version 2 : Ryle Telescope control tables.
C
C***********************************************************************

C     A few essential constant declarations.
C         Page length in words (four bytes/word)
              integer         page
              parameter     ( page = 512 )
C         Maximum and minimum control tables length (in pages)
              integer         ct_min_len, ct_max_len
              parameter     ( ct_min_len  = 4 )
              parameter     ( ct_max_len  = 8 )
C         The current maximum version number for the control tables
              integer         ct_max_vers
              parameter     ( ct_max_vers = 2 )
C         The start of the control table packing block
              integer         ctv1_pack_ptr
              integer         ctv2_pack_ptr
              parameter     ( ctv1_pack_ptr = 1440 )
              parameter     ( ctv2_pack_ptr = 112 )
C         The start of the variant part of the control tables
              integer         ctv1_var_ptr
              integer         ctv2_var_ptr
              parameter     ( ctv1_var_ptr = ctv1_pack_ptr )
              parameter     ( ctv2_var_ptr = 2944 )

C     Runtime information.
C         Current control tables logical unit number
              integer         ct_lun
C         Current ionospheric correction tables logical unit number
              integer         ion_lun
C         Control tables version number
              integer         ct_vers

C     Control Table definition.
C         Control tables array
              integer*4       ct_all( ct_max_len*page )
C         Control tables version number
              integer*2       ctv0_verno
              integer*4       ctv1_verno
              integer*4       ctv2_verno
              equivalence   ( ctv0_verno, ct_all(835) )
              equivalence   ( ctv1_verno, ct_all(ctv1_pack_ptr+3) )
              equivalence   ( ctv2_verno, ct_all(ctv2_pack_ptr+3) )
C         Control tables length (in pages)
              integer*4       ctv0_pages
              integer*4       ctv1_pages
              integer*4       ctv2_pages
              equivalence   ( ctv0_pages, ct_all(1803) )
              equivalence   ( ctv1_pages, ct_all(ctv1_pack_ptr+2) )
              equivalence   ( ctv2_pages, ct_all(ctv2_pack_ptr+2) )
C         User identifier of last access
*             character*16    ct_user


C         Common block declaration.
C         Initialisation of runtime variables is performed in block data.
              common   / control_tables / ct_all
              common   / ct_runtime / ct_lun, ct_vers, ion_lun

