

*+APPLY_PATCH_RYLE

       subroutine apply_patch_ryle (file, status)
C      ------------------------------------------
C
C  Executes the APPLY-PATCH command.
C
C  Given:
C      FILE          char*(*)      current sample file name
C      STATUS        integer       status value
C
C  Routine to apply an official patch to the sample file.
C
C  DJT, 19/4/90
*-
       character  file*(*)
       integer    status
c
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/patches_tab_v2.inc'
c
       integer*2  patch_top
       equivalence (patch_top, patch_level)

       integer    ifile, iout, patch
       integer    i,j,k
       real*8     sfreq

       intrinsic  ibset
c
       if (status.ne.0) return

       call io_enqout(iout)

c  Open the sample file and read control tables

       call open_sf(ifile, file, 'WRITE', 0, status)

c  Clear patch level variable, for archaic sample files (early files
c  have rubbish, not zero, in this variable)

       if (patch_top.ne.0) patch_level = 0

c  Select patch to apply, check if already done

       patch = 1
       write(iout,*)
       write(iout,*)'Patch ',patches(patch)
       if (btest(patch_level,patch-1).ne.0) then
         write(iout,*)'*** this patch has already been applied'
         write(iout,*)
         return
       endif

c  Apply patch

       if (patch.eq.1) then

c    [19/4/90] Re-order channel frequencies and re-compute baselines.
c     Sample files prior to this date have the visibilities for channels
c     within sub-bands recorded in descending order of frequency, though
c     the channel frequencies are in ascending order. This patch resets
c     the channel frequency array so that visibilities are correctly
c     labelled.  The baseline arrays X,Y,Z must also be re-computed.

         write(iout,*)
         write(iout,*) ' channel frequency offsets'
         write(iout,'(8F8.2)') (chan_freq(i),i=1,Nchannel)
         do i = 1, Nchannel/2
           sfreq = chan_freq(i)
           chan_freq(i) = chan_freq(Nchannel-i)
           chan_freq(Nchannel-i) = sfreq
         enddo
         write(iout,*)' changed to'
         write(iout,'(8F8.2)') (chan_freq(i),i=1,Nchannel)

         do k = 1, Naes
           do j = 1, Nsubb
           do i = 1, Nchannel
             sfreq = 1.D-3 * (freq + subb_freq(iba_code(j))
     :                             + chan_freq(ich_code(i)))
             x(i,j,k) = sfreq * x8(iae_code(k))
             y(i,j,k) = sfreq * y8(iae_code(k))
             z(i,j,k) = sfreq * z8(iae_code(k))
           enddo
           enddo
         enddo
         write(iout,*)' baselines re-computed'
         write(iout,*)

         patch_level = ibset(patch_level,patch-1)

       endif

c  Write control tables back and close file

       if (status.eq.0) then
         call write_ct(ifile, status)
       else
         call smp_wrerr(status,'in routine APPLY_PATCH_RYLE')
         status = 0
       endif

       call close_sf(ifile, status)

       end
