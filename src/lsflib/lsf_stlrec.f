C
C
C     *****************************************************************
C
C+ lsf_stlrec

       subroutine lsf_stlrec( rec_num, s )
C
C Save the last record number written to the lsf
C
C Given:
C   record number
       integer    rec_num
C Returned:
C   error status
       integer    s
C
C Routine used to save the last record written to the LSF file so that
C the information is available at the calling level via an enquiry
C routine.
C
C PA 16/4/90
C-
C local common block to save value
       integer                     save_rec_num
       common /lsf_local_save_rec/ save_rec_num

       if (s.ne.0) return
       save_rec_num = rec_num
       end
