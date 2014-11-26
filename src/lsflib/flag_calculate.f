C
*+ flag_calculate

       subroutine flag_calculate( lsf_num, sf_lun, src_num,
     *                            key, record, array, status )
C      -------------------------------------------------------
C
C Calculate the flagging array
C
C Given:
C   logical sample file number
       integer       lsf_num
C   sample file logical unnit number
       integer       sf_lun
C   source number in sample file
       integer       src_num
C   key to list of versions
       integer*4     key
C   last record to use
       integer       record
C
C Updated
C   bit array for flag-table array
       integer       array(*)
C   error return code
       integer       status
C
C The flag table array for the specified sample file and lsf is
C calculated.
C
C [PA, 14/8/91]
C-

       include '/mrao/include/chrlib_functions.inc'
       include '/mrao/include/iolib_functions.inc'
c      include '/mrao/include/cmd_functions.inc'
       include '/mrao/post/include/global_constants.inc'
       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'

C Local variables
C   counters
       integer       n, nn, i1, i2, is, iv, istat
C   number of visibilities and samples in the sample file
       integer       nv, ns
C   flag file name and id
       character*64  ff_name
       integer       flag_id
C   list of version numbers
       integer       vlist(32), nvlist
C   list of records
       integer       rlist(250), nrlist
C   information in each record
       integer       flag_version, flag_timdat(6)
       character     flag_updated*32, flag_comment*16,
     *               flag_spac_list*192, flag_time_range*192,
     *               flag_operation*192
C   visibility and sample lists
       integer       vis_list(max_vis), nvis
       integer       samp_list(max_samp), nsamp

C check status on entry
       if (status.ne.0) return

C find information on the sample file
       call enq_numvis( sf_lun, nv, status )
       call enq_numsamp( sf_lun, src_num, ns, status )

C clear the bit array
       call util_clrbfd( array, 1, ns*nv )

C open flag file
       call enq_namfil( sf_lun, 'FLAG', ff_name, status )
       call flag_open( ff_name, flag_id, status )
c      if (cmd_dblev(3)) then
c        print *,'..(FLAG-CALCULATE) opened file flag_id = ',flag_id
c      end if

C find list of version numbers to scan
       call flag_get_list( flag_id, key, vlist, nvlist, status )

C loop for each version number in the table:
       do n=1,nvlist
C .. find records
         call flag_enq_entries( flag_id, vlist(n),
     *                          nrlist, rlist, status )
C .. loop and read each record
         do nn=1,nrlist
           if (rlist(nn).le.record) then
             call flag_read_entry( flag_id, rlist(nn),
     *                             flag_version, flag_timdat,
     *                             flag_updated,
     *                             flag_spac_list, flag_time_range,
     *                             flag_operation, flag_comment,
     *                             status                         )
             if (status.eq.0) then
C .... interprete record and do flagging
               i1 = chr_intlc(flag_time_range)
               i2 = chr_lenb(flag_time_range)
               call lsf_set_slist( lsf_num,
     *                             flag_time_range(i1:i2),
     *                             samp_list, nsamp, status )
               i1 = chr_intlc(flag_spac_list)
               i2 = chr_lenb(flag_spac_list)
               call set_spacings( sf_lun,
     *                            flag_spac_list(i1:i2),
     *                            vis_list, nv, nvis, status )
               i1 = chr_intlc(flag_operation)
               i2 = chr_lenb(flag_operation)
               if (chr_cmatch(flag_operation(i1:i2),'SET')) then
                 do is=1,nsamp
                   do iv=1,nvis
                     call util_setbit(array,
     *                    ((samp_list(is)-1)*nv+vis_list(iv)) )
                   end do
                 end do
               elseif (chr_cmatch(flag_operation(i1:i2),'UNSET')) then
                 do is=1,nsamp
                   do iv=1,nvis
                     call util_clrbit(array,
     *                    ((samp_list(is)-1)*nv+vis_list(iv)) )
                   end do
                 end do
               end if
             end if
           end if
         end do
       end do

999    call flag_err( status, 'FLAG_CALCULATE','Failed')
       flg_calculation = .false.
       flg_lsf = lsf_num
       istat = 0
       call flag_close( flag_id, istat )
       end
