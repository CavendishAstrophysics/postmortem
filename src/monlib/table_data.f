*+TABLE_DATA

       subroutine table_data(sf_lun,merge_type,values,list,
     *                      n_groups,prec,marginals,title,status)
C      ----------------------------------------------------------
C
C Tabulate data in a variety of formats depending on merge type.
C
C Given:
C    sample-file logical unit number
       integer        sf_lun
C    merge type
       integer     merge_type
C    data values (1 per group)
       real           values(*)
C    index numbers for groups
       integer        list(*)
C    number of groups
       integer        n_groups
C    precision for numbers in the table
       integer        prec
C    print marginals control flag (where appropriate)
       logical        marginals
C    title for table
       character*(*)  title
C
C Returned:
C    status
       integer        status
C
C This routine tabulates data in a variety of formats depending on the
C merge type and the telescope type. The only control the user has is
C over the precision with which the data are printed via PREC.
C
C [PA, 25/11/88]
C [DJT, 21/12/89]
C-
       integer        iout, chr_lenb, length_title
       integer        itscope
       character      line*80

       include '/mrao/post/include/merge_types.inc'
       include '/mrao/post/include/phys_tscopes.inc'
       include '/mrao/post/include/samplib_errors.inc'

C check status on entry
       if (status.ne.0) return

C check the telescope type
       call enq_phys_tscope(sf_lun,itscope,status)

       if (itscope.ne.CLFST .and. itscope.ne.RYLE) then
         status = ill_tscope
         goto 999
       end if

C Add title to the table
       call io_enqout(iout)
       call chr_chfill(line,'-')
       length_title = chr_lenb(title)
       write(iout,'(2(X,A/))')title,line(1:length_title)

C Take action depending on telescope type and merge type
       if (itscope.eq.CLFST) then
         if (merge_type.eq.no_merge) then
           call table_spac_clfst(sf_lun,values,
     *                           list,n_groups,prec,marginals,status)

         elseif (merge_type.eq.aerial_merge) then
           call table_aes(sf_lun,values,list,n_groups,prec,status)

         elseif (merge_type.eq.hut_sw_merge) then
           call table_huts_clfst(sf_lun,values,n_groups,prec,status)

         else
           status = ill_merge
           goto 999
         endif

       elseif (itscope.eq.RYLE) then
         if (merge_type.eq.no_merge) then
           call table_vis_ryle(sf_lun,values,list,n_groups,prec,status)

         elseif (merge_type.eq.subband_merge) then
           call table_spac_ryle(sf_lun,values,list,n_groups,prec,status)

         elseif (merge_type.eq.fr_aerial_merge) then
           call table_log_ryle(sf_lun,values,list,n_groups,prec,status)

         elseif (merge_type.eq.aerial_merge) then
           call table_aes(sf_lun,values,list,n_groups,prec,status)

         else
           status = ill_merge
           goto 999
         end if

       end if

       return

 999   call io_wrerr (status,'in routine TABLE_DATA')

       end
