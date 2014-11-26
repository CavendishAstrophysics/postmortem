

C+SET_V1_MERGE

       subroutine set_v1_merge ( lun, sp_list, no_spac, merge_type,
     :                           merge_list, no_groups, group_size,
     :                                                      status )
C
C  Resolves a spacing list into groups for merging (V1 support).
C
C  Given:
C      LUN         integer     sample file logical unit number
C      SP_LIST     integer(*)  input spacing list
C      NO_SPAC     integer     number of spacings in the list
C      MERGE_TYPE  integer     merge control
C
C  Returned:
C      MERGE_LIST  integer(*)  array containing spacing list index numbers
C                              ordered into groups according to merge type
C      NO_GROUPS   integer     number of groups
C      GROUP_SIZE  integer(*)  length of each group
C      STATUS      integer     status value
C
C  Routine to resolve a spacing list into groups according to the given
C  merge types outlined in (postmortem)merge_types.inc', viz.:
C
C      MERGE_TYPE = 1  no merging
C      MERGE_TYPE = 2  merge by aerials
C      MERGE_TYPE = 3  merge west aerials into east aerials only.
C      MERGE_TYPE = 4  merge east aerials into west aerials only.
C      MERGE_TYPE = 8  merge by huts (switch groups, East x West)
C      MERGE_TYPE = 9  merge by huts (complete merge)
C      MERGE_TYPE = 10 total merge
C
C  The groups are returned as sets of index numbers to the input spacing list,
C  stored consecutively in the output spacing list, together with the number
C  of spacings in each group.  Note that each element of the input spacing
C  list may possibly occur twice in the output list.  The output list may
C  be used to merge visibility data according to the given merge type.
C
C  For MERGE_TYPE 1, there is an output group for each spacing
C  For MERGE_TYPE 2, there is an output group for each aerial.
C  For MERGE_TYPE 3, there is an output group for each aerial but only
C                    east aerials have non-zero group sizes.
C  For MERGE_TYPE 4, there is an output group for each aerial but only
C                    west aerials have non-zero group sizes.
C  For MERGE_TYPE 8, there is an output group for each East/West hut pair.
C  For MERGE_TYPE 9, there is an output group for each hut.
C  For MERGE_TYPE 10, there is one output group.
C
C  The status value should be zero on entry.
C
*-
       integer   lun, no_spac, status
       integer   sp_list(no_spac), merge_type
       integer   merge_list(*), group_size(*), no_groups
       integer   i, iae, iaw, iae1, iae2, iaw1, iaw2
       integer   igroup, ihut, ihute, ihutw, isp
       integer   i_tscope
C
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/control_tab_v1.inc'
       include '/mrao/post/include/merge_types.inc'
C
C
       if (status.ne.0) return
       call enq_phys_tscope( lun, i_tscope, status )
       if (status .ne. 0) goto 999

       if (i_tscope.ne.clfst) then
         status = ILL_TSCOPE

       else if (merge_type.eq.no_merge) then
C No merge (special case)
         no_groups=no_spac
         do isp=1,no_spac
           group_size(isp) = 1
           merge_list(isp) = isp
         enddo

       elseif (merge_type.eq.aerial_merge .or.
     *         merge_type.eq.west_aerials .or.
     *         merge_type.eq.east_aerials      ) then
C Merge by aerial

         no_groups=max_aes

         i=0
         do iae=1,max_aes
           group_size(iae)=0

           if ((merge_type.eq.aerial_merge) .or.
     *         (merge_type.eq.west_aerials .and. iae.gt.neast_aes) .or.
     *         (merge_type.eq.east_aerials .and. iae.le.neast_aes)) then

             do isp=1,no_spac
               if (iae.eq.ispae(1,sp_list(isp)) .or.
     :             iae.eq.ispae(2,sp_list(isp))) then
                 group_size(iae)=group_size(iae)+1
                 merge_list(i+1)=isp
                 i=i+1
               endif
             enddo
           endif
         enddo

       else if (merge_type .eq. hut_sw_merge) then
C Merge by huts (switch groups, East x West)
         no_groups=neast_huts*nwest_huts

         i=0
         igroup=0
         do ihute=1,neast_huts
           call enq_ae_hut(lun,ihute,iae1,iae2,status)
           do ihutw=neast_huts+1,max_huts
             call enq_ae_hut(lun,ihutw,iaw1,iaw2,status)

             igroup=igroup+1
             group_size(igroup)=0
             do isp=1,no_spac
               iae=ispae(1,sp_list(isp))
               iaw=ispae(2,sp_list(isp))
               if (iae1.le.iae .and. iae.le.iae2 .and.
     :             iaw1.le.iaw .and. iaw.le.iaw2) then
                 group_size(igroup)=group_size(igroup)+1
                 merge_list(i+1)=isp
                 i=i+1
               endif
             enddo

           enddo
         enddo
       elseif (merge_type.eq.hut_merge) then
C Merge by huts (no switch groups)

         no_groups=max_huts

         i=0
         do ihut=1,neast_huts
           group_size(ihut)=0
           call enq_ae_hut(lun,ihut,iae1,iae2,status)
           do isp=1,no_spac
             iae=ispae(1,sp_list(isp))
             if (iae1.le.iae .and. iae.le.iae2) then
               group_size(ihut)=group_size(ihut)+1
               merge_list(i+1)=isp
               i=i+1
             endif
           enddo
         enddo

         do ihut=neast_huts+1,max_huts
           group_size(ihut)=0
           call enq_ae_hut(lun,ihut,iaw1,iaw2,status)
           do isp=1,no_spac
             iaw=ispae(2,sp_list(isp))
             if (iaw1.le.iaw .and. iaw.le.iaw2) then
               group_size(ihut)=group_size(ihut)+1
               merge_list(i+1)=isp
               i=i+1
             endif
           enddo
         enddo

       elseif (merge_type.eq.total_merge) then
C Total merge of all spacings
         no_groups=1
         group_size(1)=no_spac
         do isp=1,no_spac
           merge_list(isp)=isp
         enddo
       endif
       return

 999   call smp_wrerr( status, 'in subroutine SET_V1_MERGE' )

       end
