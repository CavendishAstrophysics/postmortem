C
C
C     *****************************************************************
C
C+ lsf_enlrec

       subroutine lsf_enlrec( rec_num, s )
C
C Enquire the last record number written to the lsf
C
C Returned:
C   record number
       integer    rec_num
C   error status
       integer    s
C
C Routine used to enquire the last record written to the LSF file.
C
C PA 16/4/90
C-
C local common block to save value
       integer                     save_rec_num
       common /lsf_local_save_rec/ save_rec_num

       if (s.ne.0) return
       rec_num = save_rec_num
       end
