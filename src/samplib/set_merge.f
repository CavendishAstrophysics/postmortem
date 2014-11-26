

C+SET_MERGE

       subroutine set_merge ( lun, vis_list, no_vis, merge_type,
     :                        merge_list, no_groups, group_size,
     :                                                   status )
C
C  Resolves a spacing list into groups for merging.
C
C  Given:
C      LUN          integer     sample file logical unit number
C      VIS_LIST     integer(*)  input visibility list
C      NO_VIS       integer     number of spacings in the list
C      MERGE_TYPE   integer     merge control
C
C  Returned:
C      MERGE_LIST   integer(*)  array containing visibility index numbers
C                               ordered into groups according to merge type
C      NO_GROUPS    integer     number of groups
C      GROUP_SIZE   integer(*)  length of each group
C      STATUS       integer     status value
C
C  Routine to resolve a visibility list into groups according to the
C  merge types outlined in (postmortem)merge_types.inc', viz.:
C
C      MERGE_TYPE = 1  no merging
C      MERGE_TYPE = 2  merge by aerials
C      MERGE_TYPE = 3  merge west aerials into east aerials only.
C      MERGE_TYPE = 4  merge east aerials into west aerials only.
C      MERGE_TYPE = 5  merge frequencies for each aerial.
C      MERGE_TYPE = 6  merge frequency channels for each sub-band.
C      MERGE_TYPE = 7  merge sub-bands for each spacing
C      MERGE_TYPE = 8  merge by huts (switch groups, East x West)
C      MERGE_TYPE = 9  merge by huts (complete merge)
C      MERGE_TYPE = 10 total merge
C
C  The groups are returned as sets of index numbers to the input visibility
C  list stored consecutively in the output list, together with the number
C  of spacings in each group.  Note that each element of the input
C  list may possibly occur twice in the output list.  The output list may
C  be used to merge visibility data according to the given merge type.
C
C  For MERGE_TYPE 1, there is an output group for each visibility.
C  For MERGE_TYPE 2, there is an output group for each aerial.
C  For MERGE_TYPE 3, there is an output group for each aerial but only
C                    east aerials have non-zero group sizes.
C  For MERGE_TYPE 4, there is an output group for each aerial but only
C                    west aerials have non-zero group sizes.
C  For MERGE_TYPE 5, there is an output group for each aerial.
C  For MERGE_TYPE 6, there is an output group for each sub-band*spacing.
C  For MERGE_TYPE 7, there is an output group for each spacing.
C  For MERGE_TYPE 8, there is an output group for each East/West hut pair.
C  For MERGE_TYPE 9, there is an output group for each hut.
C  For MERGE_TYPE 10, there is one output group.
C
C  The status value should be zero on entry.
C
*-
C SF unit number, number of visibilities in input list, status
       integer   lun, no_vis, status

C list of visibility indexex, merge type
       integer   vis_list(no_vis), merge_type

C returned merge list, size of each group, number of groups
       integer   merge_list(*), group_size(*), no_groups

C Physical telescope identifier
       integer   itscope
C
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/merge_types.inc'
C
C
       if (status.ne.0) return
       call enq_phys_tscope( lun, itscope, status )
       if (status .ne. 0) goto 999

       if (itscope.eq.clfst) then
         if (merge_type.eq.fr_aerial_merge .or.
     :       merge_type.eq.channel_merge .or.
     :       merge_type.eq.subband_merge) then
           status = ILL_MERGE
           goto 999
         end if
         call set_v1_merge(lun,vis_list,no_vis,merge_type,
     :                     merge_list,no_groups,group_size,status)

       elseif (itscope.eq.five_km) then
         if (merge_type.eq.hut_sw_merge .or.
     :       merge_type.eq.hut_merge) then
           status = ILL_MERGE
           goto 999
         end if
         call set_v2_merge(lun,vis_list,no_vis,merge_type,
     :                     merge_list,no_groups,group_size,status)

       else
         status = ILL_TSCOPE
         goto 999
       end if
       return
C
 999   call smp_wrerr( status, 'in subroutine SET_MERGE' )

       end
