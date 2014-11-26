C
C Current Aerial and Visibilities Gains solution
C ----------------------------------------------
C
C Last updated by P. Alexander, 12/11/91
C
       logical   current_solution, current_gains_table
       integer   current_refant
       complex   ae_gains( max_channel, max_subb, max_RT_aes )
       complex   vis_gains( max_vis )
C
       common /cal_gains_soln/    current_solution,
     *                            current_gains_table,
     *                            current_refant
       common /cal_ae_gains_soln/ ae_gains, vis_gains
C
