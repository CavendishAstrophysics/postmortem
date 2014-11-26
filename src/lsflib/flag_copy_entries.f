*+ flag_copy_entries

       subroutine flag_copy_entries( flag_id, list_entries, num_entries,
     *                               flag_version, status              )
C      -----------------------------------------------------------------
C
C Copy entries in the flag table to a new version number
C
C Given:
C   flag file identifier
       integer         flag_id
C   number of entries to copy
       integer         num_entries
C   list of entries to copy
       integer         list_entries(*)
C Updated:
C   version number for new entry (-1 = latest version)
       integer         flag_version
C
C Updated:
C   error status code
       integer         status
C
C Entries are copied in the flag table at version FLAG_VERSION;
C if FLAG_VERSION=-1 they are written at the latest version,
C and if FLAG_VERSION>= latest_version, then they are written with
C a new version number.  FLAG_VERSION is updated by this routine to
C return the version actually written.
C
C [PA, 13/8/91] [save_sf added 8 Feb 93 GP]
C-

       include '/mrao/post/include/flag_definition.inc'
       include '/mrao/post/include/flag_errors.inc'
       include '/mrao/include/chrlib_functions.inc'

C local variables
       integer         n
       character*64    flag_file

       if (status.ne.0) return

C check the file is open
       if (flg_list_open(flag_id)) then

C .. read header entry
         read (flag_id, rec=1, iostat=status) flg_head_rec
         if (status.ne.0) goto 999

C .. check the version to write
         if (flag_version.lt.0) then
           flg_version = flg_last_version
         else if (flag_version.gt.flg_last_version) then
           flg_version = flg_last_version + 1
           flg_last_version = flg_last_version + 1
         else
           flg_version = flag_version
         end if
         flag_version = flg_version

C .. update entries in the record
         do n=1,num_entries
           if (list_entries(n).gt.0 .and.
     *         list_entries(n).le.flg_entries) then
             read (flag_id, rec=list_entries(n)+1, iostat=status)
     *            flg_record
             if (status.ne.0) goto 999
             flg_entries = flg_entries + 1
             flg_version = flag_version
             write (flag_id, rec=flg_entries+1, iostat=status )
     *              flg_record
           end if
         end do
         write (flag_id, rec=1, iostat=status )
     *          flg_head_rec
         flag_file = flg_list_name(flag_id)
c        call save_sf(flag_file, status)  ! add name to save-lsf:list
       else
         status = ill_flgopen
       end if

999    call flag_err( status, 'FLAG_COPY_ENTRIES', 'Failed' )
       end
C
C
