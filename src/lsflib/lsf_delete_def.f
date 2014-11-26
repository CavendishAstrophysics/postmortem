C
C+lsf_delete_def
C
      SUBROUTINE lsf_delete_def ( psf_lun, delete_key, s )

C     Marks a given LSF record as deleted or un-deleted
C
C     Given:
C         Physical sample file unit number (PSF must be open).
              integer         psf_lun
C         Delete key; 1=Delete, 0=Undelete
              integer         delete_key

C     Returned:
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Marks an LSF definition as deleted or undeleted, the user is
C     prompted for the LSF definition to update.
C
C     If successful the LSF file is put on the tape save list.
C
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      logical   yesno_num

C     ****************************************************************
C
C     Global includes -
C         ( i.e. Global constant, variable and common declarations )
C
      include  '/mrao/include/iolib_errors.inc'
      include  '/mrao/post/include/lsflib_errors.inc'
      include  '/mrao/post/include/lsf_record.inc'

C     ****************************************************************
C
C     Local constant and variable declarations
C         Loop counter and record number counter
              integer         i, next_rec
C         Logical sample file logical unit number
              integer         lsf_lun
C         User information
              character*16    user
              integer*4       mode, term_no
C         Full file name for file of lsf's.
              character*80    lsf_fname
C         Command line length
              integer         len_cli
C         LSF key
              integer         key
C         General purpose string and length
              character*160   string
              integer         ls

C     ****************************************************************
C
C     Subroutine initialisation
C
C     Check for non zero entry status
      if ( s .ne. 0 ) return

      lsf_lun = 0
      call io_enqexe( user, mode, term_no )

C     ****************************************************************
C
C         Main Code
C         ---------
C
C     Find one of the saved lsf's
      call enq_namfil( psf_lun, 'LSF', lsf_fname, s )
C     Change the error to be specific to LSF's if applicable
      if ( s .eq. NO_FILE ) s = NO_LSFSAVED
      if ( s .ne. 0 ) goto 9999
      call io_nxtlun( lsf_lun, s )

      open(   unit    = lsf_lun,
     *        file    = lsf_fname,
     *        access  = 'DIRECT',
     *        recl    = lsf_len*4,
     *        status  = 'OLD',
     *        iostat  = s             )
      if (s .ne. 0) goto 9999

C     Read through file until lsf is found.
      read( lsf_lun, rec=1, iostat = s ) log_samp_file
      next_rec = 2
      if ( s .ne. 0 ) goto 9999

      key = 0
      if ( mode .eq. 0 ) then
        call io_enqcli( string, len_cli)
        if (len_cli.eq.0) then
           string = 'Select logical sample file for deletion :'
           if (delete_key.eq.0) string(32:) = 'undeletion :'
           call io_wrout( string(1:chr_lenb(string)) )
c          call io_wrout(
c    *    'Select logical sample file for (un)deletion :' )
        end if
      end if

  100 continue
          if (key .eq. 0) then
              ls = chr_lenb( lsf_name )
              if (lsf_version.ge.0 .and. delete_key.eq.1) then
                write(string,'(3A,I2,5A)')
     *              char(13), char(10),
     *              ' LSF ',(next_rec-1),' : ',lsf_name(1:ls),
     *              char(13), char(10), '          Created '
                ls = ls + 33
                call util_extdat( lsf_time_created, 2, string(ls:), i )
                string(ls+i:)= ';  delete?'
                if (yesno_num(string,'No',next_rec,s)) key = lsf_key
              else if (lsf_version.lt.0 .and. delete_key.eq.0) then
                write(string,'(3A,I2,5A)')
     *              char(13), char(10),
     *              ' LSF ',(next_rec-1),' : ',lsf_name(1:ls),
     *              char(13), char(10), '          Created '
                ls = ls + 33
                call util_extdat( lsf_time_created, 2, string(ls:), i )
                string(ls+i:)= ';  undelete?'
                if (yesno_num(string,'No',next_rec,s)) key = lsf_key
              end if
              if ( s .ne. 0 ) goto 9999
          end if

          if ( key .ne. lsf_key ) then
              read(lsf_lun,rec=next_rec,iostat=s) log_samp_file
              next_rec = next_rec+1

              if ((s .ne. 0) .and. (mode .eq. 0)) then
                  goto 9999
              end if
          end if
      if ((key .ne. lsf_key) .and. (s .eq. 0)) goto 100

      if ((s .ne. 0) .or. (lsf_key .eq. 0)) then
          s = ILL_LSF
          goto 9999
      end if
      call io_wrout(' ')

C     Found LSF, so update LSF last use information.
      lsf_last_user   = user
      call util_enqnow( lsf_last_used )
      if (delete_key.eq.1) then
        lsf_version = - abs(lsf_version)
      else
        lsf_version = abs(lsf_version)
      end if
      write( lsf_lun, rec=(next_rec-1), iostat = s ) log_samp_file
      if ( s .ne. 0 ) goto 9999
      close( lsf_lun, iostat = s )
      lsf_lun = 0
      if ( s .ne. 0 ) goto 9999
      return

C     ****************************************************************
C
C         Error Handling
C         --------------
C
 9999 continue
C         Allow calling programs to deal with the problem of there
C         being no lsf's saved.
          if ( s .ne. NO_LSFSAVED .and. s .ne. USR_BREAK ) then
              call lsf_wrerr( s, 'in subroutine LSF_DELETE_DEF' )
          end if

          if (lsf_lun .ne. 0) close( lsf_lun )
          call io_wrout(' ')
          return
      end
