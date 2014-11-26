

C+cal_set_logical

       subroutine cal_set_logical( isb, ich, s )
C      -----------------------------------------
C
C Set the description of the current logical aerial
C
C Given:
C   sub-band identifier
       integer     isb
C   channel identifier
       integer     ich
C Returned:
C   status word
       integer     s
C
C Set the definition of the current logical aerial:
C  if the logical aerial is a channel isb and ich are non zero
C  if the logical aerial is a sub-band isb is non zero & ich is zero
C  if the logical aerial is a physical aerial isb and ich are zero
C
C-
       include '/mrao/post/include/cal_control.inc'

       if (s.ne.0) return
       current_subband = isb
       current_channel = ich

       if (s.ne.0) call cal_wrerr( s, 'in cal_set_logical' )

       end
