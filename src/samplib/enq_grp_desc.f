C+ENQ_GRP_DESC

       subroutine enq_grp_desc ( lun, merge_type,
     *                           sp_list, nspac, merge_list,
     *                           no_groups, group_size, group_num,
     *                           description, s )
C
C     Returns a description for a given merge group.
C
C     Given:
C         Sample file logical unit number
              integer         lun
C         The merge type
              integer         merge_type
C         Spacing list and length that merge list was derived from.
              integer         nspac, sp_list(nspac)
C         Array containing buffer index numbers arranged in merge groups
              integer         merge_list(*)
C         Number of groups and array of group sizes.
              integer         no_groups, group_size(*)
C         Group number for description
              integer         group_num

C     Returned:
C         Character description.
              character*(*)   description
C         Status value
              integer         s
C
C  The status value should be zero on entry.
C
C-
      include        '/mrao/post/include/merge_types.inc'
      include        '/mrao/post/include/phys_tscopes.inc'
      include        '/mrao/post/include/clfst_constants.inc'
      include        '/mrao/post/include/samplib_errors.inc'

      real            spac_no
      integer         e_hut, w_hut
      character*1     huts(8), bands(5)
      character*4     ae1, ae2, cspac
      integer         iae1, iae2, l1, l2, ls
      integer         n, ng, isp, iba, ich
      integer         itscope
      data     bands / 'A', 'B', 'C', 'D', 'E' /
      data     huts  / 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H' /

      if ( s .ne. 0) return
      call enq_phys_tscope( lun, itscope, s )

C find index to first member of merge list in current group
      ng = 1
      do n = 1, group_num-1
         ng = ng + group_size(n)
      end do

C find designation for this spacing
      call enq_vis_desig( lun,sp_list(merge_list(ng)),isp,iba,ich,s )
      call enq_ae_vis( lun,sp_list(merge_list(ng)),iae1,iae2,s )
      call chr_chitoc( iae1,ae1,l1 )
      call chr_chitoc( iae2,ae2,l2 )
      call chr_chitoc( isp,cspac,ls )

      if (itscope.eq.clfst .or. itscope.eq.ryle) then
          if (merge_type .eq. no_merge) then
              call enq_spac_no(lun,sp_list(group_num),spac_no,s)
              call enq_ae_vis(lun,sp_list(group_num),iae1,iae2,s )
              call chr_chitoc( iae1,ae1,l1 )
              call chr_chitoc( iae2,ae2,l2 )
              write( description, '(X,A,F6.1,4A)' )
     *            'Spacing ', spac_no,
     *            ' Aerials ', ae1(1:l1), '/', ae2(1:l2)
          else if (merge_type .eq. aerial_merge .or.
     *             merge_type .eq. west_aerials .or.
     *             merge_type .eq. east_aerials      ) then
              write( description, '(X,A,I2)' )
     *            'Merge of aerial ', group_num
          else if (merge_type .eq. hut_sw_merge) then
              e_hut = int((group_num-1)/nwest_huts)+1
              w_hut = mod( group_num-1, nwest_huts)+1+neast_huts
              write( description, '(X,A,A1,A,A1)' )
     *            'Merge of hut ', huts(e_hut), ' vs hut ', huts(w_hut)
          else if (merge_type .eq. hut_merge) then
              write( description, '(X,A,A1)' )
     *            'Merge of hut ', huts(group_num)
          else if (merge_type .eq. total_merge) then
              write( description, '(X,A)' )
     *            'Merge of all spacings'
          else if (merge_type .eq. subband_merge) then
              write( description, '(X,6A)' )
     *            'Merge over sub-bands.  spacing ', cspac(1:ls),
     *            ' ae ', ae1(1:l1), '/', ae2(1:l2)
          else if (merge_type .eq. channel_merge) then
              write( description, '(X,8A)' )
     *            'Merge over channels.  sp ', cspac(1:ls),
     *            ' ae ', ae1(1:l1), '/', ae2(1:l2),
     *            ' sb ',bands(iba)
          else
              s  =  ill_merge
          end if
      else
          s = ILL_TSCOPE
      end if

      if (s.ne.0) call smp_wrerr( s, 'in subroutine ENQ_GRP_DESC' )

      end


