

*+PRINT_OBS_STATUS

       subroutine print_obs_status (status)
C      ------------------------------------
C
C  Executes the OBSERVING-STATUS command.
C
C  Given:
C      STATUS        integer       status value
C
C  Routine to print out the current observing status.
C
C  The STATUS values should be zero on entry.
C
*-
       integer    status
c
       if (status.ne.0) return
c
       call obs_stat_clfst( status )
c
       end
