*+MERGE_VIS_BUFFER

       subroutine merge_vis_buffer (vis, no_vis, merge_list, no_groups,
     :                                    group_size, merge_vis, status)
C
C  Merges a visibility buffer by spacing groups.
C
C  Given:
C      VIS         complex(*)  visibility buffer
C      NO_VIS      integer     size of visibility buffer
C      MERGE_LIST  integer(*)  array containing buffer index numbers
C                              arranged in merge groups
C      NO_GROUPS   integer     number of groups
C      GROUP_SIZE  integer(*)  length of each group
C
C  Returned:
C      MERGE_VIS   complex(*)  merged visibility buffer
C      STATUS      integer     status value
C
C  Routine to merge an input visibility buffer according to the supplied
C  merge list.  The merge list consists of index numbers to the input
C  buffer, arranged in groups of given size.  The final merged visibilities
C  are normalised using the appropriate group size.
C
C  29-Oct-92  flagged data treated properly (ignored).
C
C  The status value should be zero on entry.
C
*-
       integer   merge_list(*)
       integer   no_vis, no_groups
       integer   group_size(no_groups), status
       complex   vis(no_vis), merge_vis(no_groups)
       integer   i, ig, is, n
c
c
       if (status.ne.0) return
c
       i=0
       do ig=1,no_groups
         n=0
         merge_vis(ig)=(0.0,0.0)
         if (group_size(ig).gt.0) then
           do is=1,group_size(ig)
             i=i+1
             if (vis(merge_list(i)).ne.(0.0,0.0)) then
               merge_vis(ig)=merge_vis(ig)+vis(merge_list(i))
               n=n+1
             endif
           enddo
           if (n.gt.0) merge_vis(ig)=merge_vis(ig)/n
         endif
       enddo
c
       end



