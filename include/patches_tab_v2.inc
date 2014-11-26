C
C  Official patches to Version 2 control tables.
C
C  [RYLE telescope sample files only.]
C
C  These patches are applied by the PATCH-SF program, and are flagged
C  in the control tables by setting bits in the PATCH_LEVEL variable.
C
       integer    no_patches
       parameter (no_patches=1)
       character  patches(no_patches)*64
       data       patches /
     :   '1 ....... channel frequencies re-defined, 18/4/90' /
