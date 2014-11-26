C Include file for LSFUN1 minimization routine
C ---------------------------------------------
C
C
       real*8           lsfun1_array(2*max_samp)
       integer          imode
       real*8           phase(max_samp), pos(max_aes)
       equivalence     (phase(1),       lsfun1_array(1))
       equivalence     (pos(1),         lsfun1_array(max_samp+1))
       real*8           vis_phase(max_samp), vis_HA(max_samp)
       equivalence     (vis_phase(1),   lsfun1_array(1))
       equivalence     (vis_HA(1),      lsfun1_array(max_samp+1))
C
       common /lsfun1_include/ lsfun1_array, imode
C
