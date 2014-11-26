C+lsf_read_def
C
      SUBROUTINE lsf_read_def ( psf_lun, key, lsf_buffer, s )

C     Reads a given record definition for an LSF file.
C
C     Given:
C         Physical sample file unit number (PSF must be open).
              integer         psf_lun
C         Logical sample file key
              integer         key

C     Returned:
C         Logical sample file record
              integer         lsf_buffer(*)
C         Status variable - must be zero on entry - otherwise error
              integer         s
C
C     Opens the logical sample file associated with the given sample
C     file and having the key given by the 'key' parameter. If this
C     parameter is zero then the user is asked to select a LSF and the
C     key parameter is updated on return.
C
C     Possible return status's:
C     NO_LSFSAVED - opening with a key of zero, but no LSF's are saved.
C     ILL_LSF     - Error in the lsf definition or trying to do an
C                   interactive open non-interactively.
C     Other       - Unexpected system error.
C
C     NPR     7 October 1988
C     PA     20 March   1991 ; Added support for delete LSF's
C     DJT    12 March   1994 ; Modified selection interaction
C     GGP    28 Sept    1999 : improved printing of lsf no & date
C-
C     ****************************************************************
C
C     Function declarations
C
      include  '/mrao/include/chrlib_functions.inc'
      logical  yesno_num

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
              integer         i, irec, next_rec
C         Logical sample file logical unit number
              integer         lsf_lun
C         User information
              character*16    user
              integer*4       mode, term_no
C         Full file name for file of lsf's.
              character*80    lsf_fname
C         Command line length
              integer         len_cli
C         Counter for number of lsf saved and not marked as deleted
              integer         count
C         General purpose string and length
              character*160   string
              integer         ls
C         Interactive selection
              logical         inter

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
C     Open one of the saved lsf's
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

C     Read through file until correct lsf is found.
      read( lsf_lun, rec=1, iostat = s ) log_samp_file
      next_rec = 2
      if ( s .ne. 0 ) goto 9999

      inter = .false.
      if ((key .eq. 0) .and. (mode .eq. 0)) then
        call io_enqcli( string, len_cli)
        if (len_cli.eq.0) then
           call io_wrout(
     *    'Select one of the following logical sample files :' )
           inter = .true.
        end if
      end if

      count = 0
  100 continue
          if (key .eq. 0 .and. lsf_version.ge.0) then
              count = count + 1
              ls = chr_lenb( lsf_name )
              write(string,'(3A,I4,5A)')
     *            char(13), char(10),
     *            ' LSF ',(next_rec-1),' : ',lsf_name(1:ls),
     *            char(13), char(10), '          Created '
              ls = ls + 33
              call util_extdat( lsf_time_created, 0, string(ls:), i )
              string(ls+i:)= ' ?'
              irec = 0
              if (yesno_num(string,'No',irec,s)) key = lsf_key
              if (irec .gt. 0) then
                 read(lsf_lun,rec=irec,iostat=s) log_samp_file
                 if ((lsf_version .gt. 0) .and. (s .eq. 0)) then
                    string = ' '
                    write(string,'(A,I4,A)') 'LSF ',irec,' ('
                    ls = chr_lenb(string)
                    call util_extdat( lsf_time_created, 0,
     *                                              string(ls+1:), i )
                    string(ls+i+1:) = ') '
                    string(ls+i+3:) = lsf_name(1:chr_lenb(lsf_name))
                    if (inter) call io_wrout( ' ' )
                    call io_wrout(string(1:chr_lenb(string)))
                    next_rec = irec + 1
                    key = lsf_key
                 else
                    call lsf_wrerr( ILL_LSF, ' ' )
                    next_rec = 1
                    s = 0
                 end if
              end if
          end if

          if ((key .ne. lsf_key) .and. (s .eq. 0)) then
              read(lsf_lun,rec=next_rec,iostat=s) log_samp_file
              next_rec = next_rec+1
          end if

          if ((s .ne. 0) .and. (mode .eq. 0)) then
              if (count.gt.0) then
                 call io_wrout( '' )
                 call io_wrout(
     *                 'No more LSF''s - one MUST be selected.')
                 read(lsf_lun,rec=1,iostat=s) log_samp_file
                 next_rec = 2
                 count = 0
              else
                 s = no_LSFSAVED
                 goto 9999
              end if
          end if
      if ((key .ne. lsf_key) .and. (s .eq. 0)) goto 100

      if ((s .ne. 0) .or. (lsf_key .eq. 0)) then
          s = ILL_LSF
          goto 9999
      end if

C     Found LSF, so update LSF last use information.
      lsf_last_user   = user
      call util_enqnow( lsf_last_used )
      write( lsf_lun, rec=(next_rec-1), iostat = s ) log_samp_file
      if ( s .ne. 0 ) goto 9999
      close( lsf_lun, iostat = s )
      lsf_lun = 0
      if ( s .ne. 0 ) goto 9999

C     Copy back to return parameter.
      do 200, i = 1, lsf_len
          lsf_buffer(i) = log_samp_file(i)
  200 continue
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
              call lsf_wrerr( s, 'in subroutine LSF_READ_DEF' )
          end if

          if (lsf_lun .ne. 0) close( lsf_lun )
          return
      end
