C-
C+lsf_load_common

      subroutine lsf_load_common( status )
C     ------------------------------------
C
C Dummy routine to ensure maximum amount of work-space is linked
C
C PA, 16/6/91.  Temporary fix to use of /POST/ workspace without
C               include file or equivalences.
C
       integer    status
       include  '/mrao/post/include/post_work_array.inc'
       end
