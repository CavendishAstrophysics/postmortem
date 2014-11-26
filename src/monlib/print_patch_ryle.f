

*+PRINT_PATCH_RYLE

       subroutine print_patch_ryle (file, status)
C      ------------------------------------------
C
C  Executes the PRINT-PATCH-LEVEL command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to print out the patches applied to the sample file.
C
C  DJT, 19/4/90
*-
       character  file*(*)
       integer    status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/patches_tab_v2.inc'

       integer*2    patch_top
       equivalence (patch_top, patch_level)

       integer      ifile, iout, patch

       if (status.ne.0) return

       call io_enqout(iout)

C  Open the sample file and read control tables

       call open_sf(ifile,file,'read',0,status)

C  Report patches applied

       if (status.eq.0) then
         write(iout,*)
         if (patch_level.eq.0 .or. patch_top.ne.0) then
           write(iout,*)'No patches applied'
         else
           write(iout,*)'Patches applied'
           do patch = 1, no_patches
             if (btest(patch_level,patch-1).ne.0) then
               write(iout,*)' ',patches(patch)
             endif
           enddo
         endif
         call close_sf(ifile,status)
       endif

       write(iout,*)

       end
