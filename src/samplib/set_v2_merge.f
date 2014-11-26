

C+SET_V2_MERGE

       subroutine set_v2_merge ( lun, sp_list, no_spac, merge_type,
     :                           merge_list, no_groups, group_size,
     :                                                      status )
C
C  Resolves a spacing list into groups for merging (V2 support).
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
C      MERGE_TYPE = 2  merge all visibilities for each aerial
C      MERGE_TYPE = 5  merge all frequencies/sub-bands for each aerial
C      MERGE_TYPE = 6  merge all frequency channels
C      MERGE_TYPE = 7  merge all sub-bands
C      MERGE_TYPE = 10  total merge
C
C  The groups are returned as sets of index numbers to the input spacing list,
C  stored consecutively in the output spacing list, together with the number
C  of spacings in each group.  Note that each element of the input spacing
C  list may possibly occur twice in the output list.  The output list may
C  be used to merge visibility data according to the given merge type.
C
C  For MERGE_TYPE 1,  there is an output group for each visibility.
C  For MERGE_TYPE 2,  there is an output group for each aerial.
C  For MERGE_TYPE 5,  there is an output group for each aerial.
C  For MERGE_TYPE 6,  there is an output group for each sub-band.
C  For MERGE_TYPE 7,  there is an output group for each spacing.
C  For MERGE_TYPE 10,  there is one output group.
C
C  The status value should be zero on entry.
C
C-
C  History
C  -------
C
C Based on set_v1_merge and adapted for RYLE telescope
C
C  PA, last updated 20/9/89
C  PA, added support for channel and sub-band meges
C  PA, added support for logical aerial merges: fr_aerial_merge
C
*-
       integer   lun, no_spac, status
       integer   sp_list(no_spac), merge_type
       integer   merge_list(*), group_size(*), no_groups
       integer   i, iae, is0, is1, is2, isb, ib0, isb1, ich, ic0, ich1
       integer   igroup, ispac, isp, isp1, iba, iae1, iae2, i1, i2, ig
       integer   i_tscope

       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/control_tab_v2.inc'
       include '/mrao/post/include/merge_types.inc'


       if (status.ne.0) return
       call enq_phys_tscope( lun, i_tscope, status )
       if (status .ne. 0) goto 999

       if (i_tscope.ne.five_km) then
         status = ILL_TSCOPE

       else if (merge_type.eq.no_merge) then
C No merge (special case)
         no_groups=no_spac
         do isp=1,no_spac
           group_size(isp) = 1
           merge_list(isp) = isp
         enddo

       elseif (merge_type.eq.aerial_merge ) then
C Merge by aerial

         no_groups=max_aes
         do i = 1,no_groups
           group_size(i) = 0
         end do

         i=0
         do iae=1,naes
           call enq_iae_code(lun,iae,is0,status)
           do isp=1,no_spac
             call enq_ae_vis(lun,sp_list(isp),is1,is2,status)
             if (is0.eq.is1 .or. is0.eq.is2) then
               group_size(is0)=group_size(is0)+1
               merge_list(i+1)=isp
               i=i+1
             endif
           enddo
         enddo

       elseif (merge_type.eq.fr_aerial_merge ) then
C Merge by aerial for each channel and sub-band
         no_groups=naes*nsubb*nchannel
         do i = 1,no_groups
           group_size(i) = 0
         end do

         i=0
         do iae=1,naes
           call enq_iae_code(lun,iae,is0,status)

           do isb=1,nsubb
             call enq_iba_code(lun,isb,ib0,status)

             do ich=1,nchannel
               call enq_ich_code(lun,ich,ic0,status)
               ig = (iae-1)*nsubb*nchannel + (isb-1)*nchannel + ich

               do isp=1,no_spac
                 call enq_ae_vis(lun,sp_list(isp),is1,is2,status)
                 call enq_vis_desig(lun,sp_list(isp),isp1,isb1,ich1,
     *                              status)
                 if ( (is0.eq.is1 .or. is0.eq.is2) .and.
     *                (ib0.eq.isb1) .and. (ic0.eq.ich1) ) then
                    group_size(ig)=group_size(ig)+1
                    merge_list(i+1)=isp
                    i=i+1
                 endif

               enddo
             enddo
           enddo
         enddo

       elseif (merge_type.eq.total_merge) then
C Total merge of all spacings
         no_groups=1
         group_size(1)=no_spac
         do isp=1,no_spac
           merge_list(isp)=isp
         enddo

       elseif (merge_type.eq.subband_merge) then
C Merge of sub-bands
         no_groups = 0
         igroup = 0
         iae1 = 0
         iae2 = 0
         do i=1,no_spac
           call enq_vis_desig(lun,sp_list(i),isp,iba,ich,status)
           call enq_ae_vis(lun,sp_list(i),i1,i2,status)
           if (isp.eq.igroup .and. i1.eq.iae1 .and. i2.eq.iae2) then
             group_size(no_groups) = group_size(no_groups) + 1
           else
             igroup = isp
             iae1 = i1
             iae2 = i2
             no_groups = no_groups + 1
             group_size(no_groups) =  1
           end if
           merge_list(i) = i
         end do

       elseif (merge_type.eq.channel_merge) then
C Merge of channels
         no_groups = 0
         igroup = 0
         ispac  = 0
         iae1 = 0
         iae2 = 0
         do i=1,no_spac
           call enq_vis_desig(lun,sp_list(i),isp,iba,ich,status)
           call enq_ae_vis(lun,sp_list(i),i1,i2,status)
           if (iba.eq.igroup .and. isp.eq.ispac .and.
     *         i1.eq.iae1 .and. i2.eq.iae2           ) then
             group_size(no_groups) = group_size(no_groups) + 1
           else
             no_groups = no_groups + 1
             group_size(no_groups) = 1
             igroup = iba
             ispac  = isp
             iae1 = i1
             iae2 = i2
           end if
           merge_list(i) = i
         end do

       endif
       return
C
 999   call smp_wrerr( status, 'in subroutine SET_V2_MERGE' )

       end
